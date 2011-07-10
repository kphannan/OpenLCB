using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Diagnostics;
using System.Threading;
using System.Net;
using System.Net.Sockets;
using System.IO;
using System.IO.Ports;
using Bonjour;

namespace ComGateway
{
    public partial class ComGateway : Form
    {
        const string INITCOMPLETE = "908F";
        const string VERIFYNODEIDS = "80AF";
        const string VERIFIEDNODEID = "90BF";
        const string IDENTIFIEDCONSUMER = "926F";
        const string IDENTIFIEDCONSUMERRANGE = "925F";
        const string IDENTIFIEDPRODUCER = "926F";
        const string IDENTIFIEDPRODUCERRANGE = "925F";

        // Bonjour
        private Bonjour.DNSSDService m_service = null;
        private Bonjour.DNSSDEventManager m_eventManager = null;
        private Bonjour.DNSSDService m_browser = null;
        private Bonjour.DNSSDService m_resolver = null;
 
        Dictionary<string, string> AliasTable = new Dictionary<string, string>();
        Dictionary<string, string> NodeIdTable = new Dictionary<string, string>();

        static long nodenumber = 0;
        static string alias = "";
        static byte[] inputbuffer = new byte[2000];
        static bool serverconnected = false;
        static Socket skt = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        static SerialPort com;
        static string line = "";
        static object loglock = new object();
        static bool CANdatagramstart = false;
        static string newcmd = "";
        static string aliascheck = "";
        string xml = "<cdi><id><Software>OpenLCB USB/RS232 Com Gateway</Software>"
            + "<Version>Mike Johnson 4 July 2011</Version></id>"
            +"<seg name=\"Port Speed\"><int name=\"Port Speed\" size=\"4\"/></seg></cdi>";

        public ComGateway()
        {
            InitializeComponent();

            try
            {
                m_service = new DNSSDService();
                m_eventManager = new DNSSDEventManager();
            }
            catch
            {
                MessageBox.Show("Bonjour Service is not available", "Error");
                Application.Exit();
            }

            CheckForIllegalCrossThreadCalls = false;
            StartGetNodeNumber();
        }

        private void ComGateway_FormClosing(object sender, FormClosingEventArgs e)
        {
            try
            {
                skt.Shutdown(SocketShutdown.Both);
                skt.Close();
            }
            catch { };
            try
            {
                com.Close();
                inputtask.Abort();
            }
            catch { };
        }
        
        public void EthernetSendHexString(string s)
        {
            if (!serverconnected)
                return;
            byte[] buffer = new byte[1 + s.Length / 2];
            buffer[0] = (byte)buffer.Length;
            log("E< " + buffer[0].ToString("X2") + s);
            int j = 1;
            for (int i = 0; i < s.Length; i += 2)
                buffer[j++] = (byte)Convert.ToByte(s.Substring(i, 2), 16);
            skt.Send(buffer);
        }

        public void log(string m)
        {
            lock (loglock)
            {
                string s = LogTB.Text;
                if (s.Length > 2000)
                    s = s.Substring(0, 2000);
                LogTB.Text = m + "\r\n" + s;
                LogTB.Refresh();
            }
        }

        // Get node number and connect to server

        public void StartGetNodeNumber()
        {
            // Start
            m_eventManager.ServiceFound += new _IDNSSDEvents_ServiceFoundEventHandler(ServiceFound);

            // Browse
            try
            {
                // params service discovery ref, interface index = 0 for all, service name,
                //      domain, callback fn, context=null
                m_browser = m_service.Browse(0, 0, "_OpenLCB._tcp", null, m_eventManager);
            }
            catch
            {
                MessageBox.Show("OpenLCB Server browse Failed", "Error");
                m_eventManager.ServiceFound -= new _IDNSSDEvents_ServiceFoundEventHandler(ServiceFound);
                Application.Exit();
            }
        }

        // callback from browse
        // params service discovery ref, status flags, interface index, error code ?, service name,
        //      registration type, domain, context=null
        public void ServiceFound(DNSSDService sref, DNSSDFlags flags, uint ifIndex, String serviceName, String regType, String domain)
        {
            m_browser.Stop();
            m_eventManager.ServiceFound -= new _IDNSSDEvents_ServiceFoundEventHandler(ServiceFound);
            m_eventManager.ServiceResolved += new _IDNSSDEvents_ServiceResolvedEventHandler(ServiceResolved);

            try
            {
                m_resolver = m_service.Resolve(0, ifIndex, serviceName, regType, domain, m_eventManager);
            }
            catch
            {
                MessageBox.Show("Unable to Resolve service", "Error");
                // tidy up
                m_eventManager.ServiceResolved -= new _IDNSSDEvents_ServiceResolvedEventHandler(ServiceResolved);
                Application.Exit();
            }
        }

        public void ServiceResolved(DNSSDService sref, DNSSDFlags flags, uint ifIndex, String fullName, String hostName, ushort port, TXTRecord txtRecord)
        {
            m_resolver.Stop();
            m_eventManager.ServiceResolved -= new _IDNSSDEvents_ServiceResolvedEventHandler(ServiceResolved);

            log("OpenLCB service on " + hostName + ":" + port.ToString());

            try
            {
                // connect to server
                int i = 0;
                IPAddress[] ipa = Dns.GetHostAddresses(hostName);
                for (i = 0; i < ipa.Length; i++)
                {
                    if (!(ipa[i].IsIPv6LinkLocal || ipa[i].IsIPv6Teredo || ipa[i].IsIPv6SiteLocal || ipa[i].IsIPv6Multicast))
                        break;
                }
                IPEndPoint ep = new IPEndPoint(ipa[i], port);
                skt.Connect(ep);
                byte[] buffer = new byte[12];
                skt.Receive(buffer);
                if ((buffer[1] << 8) + buffer[2] == 0x8080)
                {
                    nodenumber = ((long)buffer[3] << 40) + ((long)buffer[4] << 32) + (buffer[5] << 24) + (buffer[6] << 16) 
                        + (buffer[7] << 8) + buffer[8];
                    log("OpenLCB Node Number " + nodenumber.ToString("X12"));
                }
                else
                {
                    log("No node number allocated.");
                    return;
                }
                serverconnected = true;
                skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
                EthernetSendHexString(INITCOMPLETE + nodenumber.ToString("X12") + nodenumber.ToString("X12"));
            }
            catch (Exception e)
            {
                log("OpenLCB server connection failed " + e.ToString());
            }
        }
        
        public string GetAlias(long nodenumber)
        {
            long random = nodenumber;
            string alias;
            do
            {
                // generate alias from nodenumber
                alias = (((int)(random ^ (random >> 12) ^ (random >> 24) ^ (random >> 36))) & 0x00000FFF).ToString("X3");
                while (AliasTable.ContainsKey(alias))
                {
                    alias = (((int)(random ^ (random >> 12) ^ (random >> 24) ^ (random >> 36))) & 0x00000FFF).ToString("X3");
                };

                CAN("0" + ((nodenumber >> 36) & 0x00000FFF).ToString("X3") + alias, "");
                CAN("1" + ((nodenumber >> 24) & 0x00000FFF).ToString("X3") + alias, "");
                CAN("2" + ((nodenumber >> 12) & 0x00000FFF).ToString("X3") + alias, "");
                CAN("3" + ((nodenumber >> 0) & 0x00000FFF).ToString("X3") + alias, "");

                aliascheck = alias;
                for (int timeout = 0; timeout < 10; timeout++)
                {
                    Thread.Sleep(100);
                    if (aliascheck == "") // someone objected
                        break;
                }
            } while (aliascheck == "");
            aliascheck = "";

            AliasTable.Add(alias, nodenumber.ToString("X12"));
            NodeIdTable.Add(nodenumber.ToString("X12"), alias);
            CAN(INITCOMPLETE + alias, nodenumber.ToString("X12"));
            return alias;
        }

        private void ComCB_DropDown(object sender, EventArgs e)
        {
            ComCB.Items.Clear();
            foreach (string s in SerialPort.GetPortNames())
                ComCB.Items.Add(s);
        }

        public void ComCB_SelectedIndexChanged(object sender, EventArgs e)
        {
            try
            {
                com.Close();
                inputtask.Abort();
            }
            catch { };
            try
            {
                com = new SerialPort(ComCB.Text, 115200);
                com.Open();
                log("Com port opened.");
                inputtask = new Thread(inputtaskloop);
                inputtask.Name = "Input Task";
                inputtask.Start();
                AliasTable.Clear();
                NodeIdTable.Clear();
                alias = GetAlias(nodenumber);
                CAN(VERIFYNODEIDS + alias, "");
            }
            catch
            {
                log("Failed to open Com port.");
            }
        }

        //*********************************************************************************

        public string TranslateToAlias(string nodeid)
        {
            if (!NodeIdTable.ContainsKey(nodeid))
                return GetAlias(Convert.ToInt64(nodeid,16));
            return NodeIdTable[nodeid];
        }

        public string TranslateToNodeID(string alias, string cmd)
        {
            if (!AliasTable.ContainsKey(alias))
            {
                log("Unexpected alias in " + cmd);
                return 0.ToString("X12");
            }
            return AliasTable[alias];
        }

        //*********************************************************************************

        static Thread inputtask;

        public void inputtaskloop()
        {
            while (true)
            {
                // CAN input
                try
                {
                    line += com.ReadExisting();
                }
                catch (Exception e)
                {
                    log("Com input error " + e.ToString());
                    com.Close();
                    ComCB.Text = "";
                    return;
                }
                // ignore anything before :
                int l = line.IndexOf(':');
                if (l == -1)
                    continue;
                line = line.Substring(l);
                // find a complete CAN packet
                l = line.IndexOf(';');
                if (l == -1)
                    continue;
                string cmd = line.Substring(0, l + 1);
                line = line.Substring(l + 1);
                log("> " + cmd);
                string a = cmd.Substring(7,3);
                if (aliascheck == a)
                    aliascheck = "";
                if (cmd.Substring(3, 4) == "8004" || cmd.Substring(3, 4) == "8001" 
                    || cmd.Substring(3, 4) == VERIFIEDNODEID || cmd.Substring(3, 4) == INITCOMPLETE)
                {
                    string n = cmd.Substring(11, 12);
                    if (NodeIdTable.ContainsKey(n)) // remove old alias
                    {
                        AliasTable.Remove(NodeIdTable[n]);
                        NodeIdTable.Remove(n);
                    }
                    AliasTable.Add(a, n);
                    NodeIdTable.Add(n, a);
                }
                string data = cmd.Substring(11, cmd.Length-12);
                switch (cmd[3])
                {
                    case '0': // CIM
                        CANdatagramstart = true;
                        if (AliasTable.ContainsKey(a))
                        {
                            string n = AliasTable[a];
                            if (n.Substring(0, 3) != cmd.Substring(3, 3))
                                CAN("7000" + alias, ""); // send RID
                        }
                        break;
                    case '1': // CIM
                        CANdatagramstart = true;
                        if (AliasTable.ContainsKey(a))
                        {
                            string n = AliasTable[a];
                            if (n.Substring(3, 3) != cmd.Substring(3, 3))
                                CAN("7000" + alias, ""); // send RID
                        }
                        break;
                    case '2': // CIM
                        CANdatagramstart = true;
                        if (AliasTable.ContainsKey(a))
                        {
                            string n = AliasTable[a];
                            if (n.Substring(6, 3) != cmd.Substring(3, 3))
                                CAN("7000" + alias, ""); // send RID
                        }
                        break;
                    case '3': // CIM
                        CANdatagramstart = true;
                        if (AliasTable.ContainsKey(a))
                        {
                            string n = AliasTable[a];
                            if (n.Substring(9, 3) != cmd.Substring(3, 3))
                                CAN("7000" + alias, ""); // send RID
                        }
                        break;
                    case '8':
                        newcmd = cmd.Substring(3, 4) + TranslateToNodeID(cmd.Substring(7, 3), cmd) + data;
                        CANdatagramstart = true;
                        checkpacket(newcmd);
                        EthernetSendHexString(newcmd);
                        break;
                    case '9':
                        newcmd = cmd.Substring(3, 4) + TranslateToNodeID(cmd.Substring(7, 3), cmd) + data;
                        CANdatagramstart = true;
                        checkpacket(newcmd);
                        EthernetSendHexString(newcmd);
                        break;
                    case 'C':
                        if (CANdatagramstart)
                        {
                            newcmd = "E" + data.Substring(0, 2) + "0" + TranslateToNodeID(cmd.Substring(7, 3), cmd) 
                                + TranslateToNodeID(cmd.Substring(4, 3), cmd) + data.Substring(2);
                            CANdatagramstart = false;
                        }
                        else
                            newcmd += data;
                        break;
                    case 'D':
                        newcmd += data;
                        CANdatagramstart = true;
                        checkpacket(newcmd);
                        EthernetSendHexString(newcmd);
                        break;
                    case 'E':
                        newcmd = "E" + data.Substring(0, 2) + "0" + TranslateToNodeID(cmd.Substring(7, 3), cmd) 
                            + TranslateToNodeID(cmd.Substring(4, 3), cmd) + data.Substring(2);
                        CANdatagramstart = true;
                        checkpacket(newcmd);
                        EthernetSendHexString(newcmd);
                        break;
                    case 'F':
                        newcmd = "F000" + TranslateToNodeID(cmd.Substring(7, 3), cmd) 
                            + TranslateToNodeID(cmd.Substring(4, 3), cmd) + data;
                        CANdatagramstart = true;
                        EthernetSendHexString(newcmd);
                        break;
                    default:
                        CANdatagramstart = true;
                        break;
                }
            }
        }

        public void CAN(string s1, string s2)
        {
            s1 = ":X1" + s1 + "N" + s2 + ";";
            log("C< " + s1);
            try
            {
                com.WriteLine(s1);
            }
            catch
            {
                log("Com write error");
            }
        }

        public void CANSendHexString(string cmd)
        {
            log("C< " + cmd);
            int length = cmd.Length / 2;
            string sid = TranslateToAlias(cmd.Substring(6, 12));
            if (cmd.Substring(2, 1) == "E")
            { // datagram
                string did = TranslateToAlias(cmd.Substring(18, 12));
                if (length-15 <= 7)
                {
                    CAN("E" + did + sid, cmd.Substring(3,2) + cmd.Substring(30));
                }
                else
                {
                    length -= 15;
                    int pl = length;
                    if (pl >= 7)
                        pl = 7;
                    CAN("C" + did + sid, cmd.Substring(3, 2) + cmd.Substring(30, pl*2));
                    cmd = cmd.Substring(30 + pl * 2);
                    length -= pl;
                    while (length >= 8)
                    {
                        CAN("C" + did + sid, cmd.Substring(0, 16));
                        cmd = cmd.Substring(16);
                        length -= 8;
                    }
                    CAN("D" + did + sid, cmd);
                }
            }
            else if (cmd.Substring(2, 1) == "F")
            { // stream

            }
            else // broadcast
            {
                CAN(cmd.Substring(2, 4) + sid, cmd.Substring(18));
            }
        }

        // true if forwarded to CAN, false if blocked
        public bool checkpacket(string cmd)
        {
            string s;
            if (cmd.Substring(2,4)==VERIFYNODEIDS) {
                s = VERIFIEDNODEID + nodenumber.ToString("X12") + nodenumber.ToString("X12");
                EthernetSendHexString(s);
                CANSendHexString(s);
                return true;
            }
            if (cmd.Substring(2,4)==INITCOMPLETE || cmd.Substring(2,4)==VERIFIEDNODEID)
                return false;
            if (cmd.Substring(2,4)==IDENTIFIEDCONSUMER || cmd.Substring(2,4)==IDENTIFIEDCONSUMERRANGE)
                return false;
            if (cmd.Substring(2,4)==IDENTIFIEDPRODUCER|| cmd.Substring(2,4)==IDENTIFIEDPRODUCERRANGE)
                return false;
            if (cmd.Substring(2, 1) == "E" && cmd.Substring(18, 12) == nodenumber.ToString("X12")) // datagram to this node
            { 
                if (cmd.Substring(2, 4) == "E200" && cmd.Substring(30, 2) == "60" && cmd.Substring(40, 2) == "FF")
                {
                    // send XML file
                    string address = cmd.Substring(32, 8);
                    int ad = Convert.ToInt32(address, 16);
                    string data = "";
                    int l = Convert.ToInt32(cmd.Substring(42, 2), 16);
                    if (ad + l > xml.Length)
                        l = xml.Length - ad;
                    for (int i = 0; i < l; i++)
                        data += ((int)xml[ad + i]).ToString("X2");
                    s = "E200" + nodenumber.ToString("X12") + cmd.Substring(6, 12) + "30" + address + "FF" + data;
                    EthernetSendHexString(s);
                    CANSendHexString(s);
                }
                else if (cmd.Substring(2, 4) == "E0A0")
                {
                    s = VERIFIEDNODEID + nodenumber.ToString("X12") + nodenumber.ToString("X12");
                    EthernetSendHexString(s);
                    CANSendHexString(s);
                }
            }
            return true;
        }

        public void InputTask(IAsyncResult ar)
        {
            Socket s = (Socket)ar.AsyncState;
            int read = s.EndReceive(ar);
            string inputstring = "";
            for (int i=0; i<read; i++)
                inputstring += inputbuffer[i].ToString("X2");
            // log("E> " + inputstring);
            while (inputstring.Length > 0)
            {
                int length = Convert.ToInt32(inputstring.Substring(0, 2), 16);
                string cmd = inputstring.Substring(0, length * 2);
                if (inputstring.Length > length * 2)
                    inputstring = inputstring.Substring(length * 2);
                else
                    inputstring = "";
                log("E> " + cmd);
                if (checkpacket(cmd))
                    CANSendHexString(cmd);
            }
            skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
        }

    }
}
