﻿using System;
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
        // CAN MTIs
        const string FT_CIM0 = "7";
        const string FT_CIM1 = "6";
        const string FT_CIM2 = "5";
        const string FT_CIM3 = "4";
        const string FT_RID = "0700";
        const string FT_AMD = "0701";
        const string FT_AME = "0702";
        const string FT_VNSN = "88A7";
        const string FT_NSN = "88B7";
        const string FT_INIT = "8087";
        const string FT_IDEVENTS = "8AB7";
        const string FT_DGS = "A";
        const string FT_DGF = "B";
        const string FT_DGM = "C";
        const string FT_DGL = "D";
        const string FT_ADDR = "E";
        
        // Ethernet MTIs
        const int NODENUMBER = 0x0000;
        const string NOFILTER = "2017";
        const string INITCOMPLETE = "2087";
        const string VERIFYNODEIDS = "28A7";
        const string VERIFIEDNID = "28B7";
        const string IDENTIFYCONSUMERS = "2A4F";
        const string CONSUMERRANGE = "225F";
        const string CONSUMERINDENTIFIED = "226B";
        const string IDENTIFYPRODUCERS = "2A8F";
        const string PRODUCERRANGE = "229F";
        const string PRODUCERINDENTIFIED = "22AB";
        const string IDENTIFYEVENTS = "2AB7";
        const string EVENT = "2ADF";
        const string XPRESSNET = "2517";
        const string DATAGRAM = "3400";
        const string ACCEPTED = "34C0";
        const string REJECTED = "34D0";
        const string STREAM = "3690";

        public long nodenumber = 0;
        public string nodenumberstr = "";
        public string alias = "";
        public int port = 0;
        public bool localhub = false;

        public string xml = "<cdi><id><Software>OpenLCB USB/RS232 Com Gateway</Software>"
            + "<Version>Mike Johnson 31 May 2012, マイク12年5月31日</Version></id>"
            +"<seg name=\"Port Speed\" space=\"0\" buttons=\"1\"><int name=\"Port Speed\" size=\"4\"/></seg></cdi>";

        public string nodename = "ComGateway";

        StreamWriter savefile = new StreamWriter("logfile.log");

        public ComGateway()
        {
            InitializeComponent();
            CheckForIllegalCrossThreadCalls = false;

            String[] arguments = Environment.GetCommandLineArgs();
            for (int a = 1; a < arguments.Length; a++)
            {
                localhub = true;
                log("Arg " + arguments[a]);
                if (arguments[a].StartsWith("port"))
                {
                    int p = arguments[a].IndexOf('=');
                    if (p > 0)
                        port = Convert.ToInt32(arguments[a].Substring(p + 1));
                }
            }

            if (localhub)
            {
                IPEndPoint ep = new IPEndPoint(IPAddress.Loopback, port);
                skt.Connect(ep);
                byte[] buffer = new byte[12];
                skt.Receive(buffer);
                if ((buffer[1] << 8) + buffer[2] == NODENUMBER)
                {
                    nodenumber = ((long)buffer[3] << 40) + ((long)buffer[4] << 32) + (buffer[5] << 24) 
                        + (buffer[6] << 16) + (buffer[7] << 8) + buffer[8];
                    nodenumberstr = nodenumber.ToString("X12");
                    log("OpenLCB Node Number " + nodenumberstr);
                }
                else
                {
                    log("No node number allocated.");
                    return;
                }
                serverconnected = true;
                skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
                EthernetSendHexString(INITCOMPLETE + nodenumberstr + nodenumberstr);
            }
            else
            {
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

                StartGetNodeNumber();
            }
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
            savefile.Close();
        }
        
       public object loglock = new object();

       public void log(string m)
        {
            lock (loglock)
            {
                m = Environment.TickCount.ToString("D9") + ": " + m;
                string s = LogTB.Text;
                if (s.Length > 5000)
                    s = s.Substring(0, 5000);
                LogTB.Text = m + "\r\n" + s;
                LogTB.Refresh();
                savefile.WriteLine(m);
            }
        }

        //*******************************************************************************************************
        // Ethernet - Get node number and connect to server
        //*******************************************************************************************************

        public byte[] inputbuffer = new byte[2000];
        public bool serverconnected = false;
        public Socket skt = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);

        // Bonjour
        private Bonjour.DNSSDService m_service = null;
        private Bonjour.DNSSDEventManager m_eventManager = null;
        private Bonjour.DNSSDService m_browser = null;
        private Bonjour.DNSSDService m_resolver = null;

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
                    if (!(ipa[i].IsIPv6LinkLocal || ipa[i].IsIPv6SiteLocal || ipa[i].IsIPv6Multicast))
                        break;
                }
                IPEndPoint ep = new IPEndPoint(ipa[i], port);
                skt.Connect(ep);
                byte[] buffer = new byte[12];
                skt.Receive(buffer);
                if ((buffer[1] << 8) + buffer[2] == NODENUMBER)
                {
                    nodenumber = ((long)buffer[3] << 40) + ((long)buffer[4] << 32) + (buffer[5] << 24) 
                        + (buffer[6] << 16) + (buffer[7] << 8) + buffer[8];
                    nodenumberstr = nodenumber.ToString("X12");
                    log("OpenLCB Node Number " + nodenumberstr);
                }
                else
                {
                    log("No node number allocated.");
                    return;
                }
                serverconnected = true;
                skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
                EthernetSendHexString(INITCOMPLETE + nodenumberstr + nodenumberstr);
            }
            catch (Exception e)
            {
                log("OpenLCB server connection failed " + e.ToString());
            }
        }

        //*******************************************************************************************************
        // Ethernet I/O
        //*******************************************************************************************************

        public void InputTask(IAsyncResult ar)
        {
            Socket s = (Socket)ar.AsyncState;
            int read = 0;
            try
            {
                read = s.EndReceive(ar);
            }
            catch
            {
                return;
            }
            string inputstring = "";
            for (int i=0; i<read; i++)
                inputstring += inputbuffer[i].ToString("X2");
            while (inputstring.Length > 0)
            {
                int length = Convert.ToInt32(inputstring.Substring(0, 2), 16);
                string cmd = inputstring.Substring(0, length * 2);
                if (inputstring.Length > length * 2)
                    inputstring = inputstring.Substring(length * 2);
                else
                    inputstring = "";
                if (EthernetCB.Checked)
                    log("Ei> " + cmd);
                if (checkpacket(cmd.Substring(2), false))
                    CANSendHexString(cmd);
            }
            try
            {
                skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
            }
            catch { }
        }

        // s is the string to send without the length
        public void EthernetSendHexString(string s)
        {
            if (!serverconnected)
                return;
            byte[] buffer = new byte[1 + s.Length / 2];
            buffer[0] = (byte)buffer.Length;
            if (EthernetCB.Checked)
                log("Eo< " + buffer[0].ToString("X2") + s);
            int j = 1;
            for (int i = 0; i < s.Length; i += 2)
                buffer[j++] = (byte)Convert.ToByte(s.Substring(i, 2), 16);
            skt.Send(buffer);
        }

        //*******************************************************************************************************
        // Serial port connection
        //*******************************************************************************************************

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
                CAN(FT_INIT + alias, nodenumber.ToString("X12"));
                CAN(FT_VNSN + alias, "");
                Thread.Sleep(20);
                CAN(FT_IDEVENTS + alias, "");
            }
            catch
            {
                log("Failed to open Com port.");
            }
        }

        //*******************************************************************************************************
        // Serial port I/O
        //*******************************************************************************************************

        public SerialPort com;
        public string line = "";
        static Thread inputtask;
        public Dictionary<string, string> longdatagram = new Dictionary<string, string>();

        public void inputtaskloop()
        {
            while (true)
            {
                Thread.Sleep(1);
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
                while (true) // process all packets in the buffer
                {
                    // ignore anything before :
                    int l = line.IndexOf(':');
                    if (l == -1)
                        break;
                    line = line.Substring(l);
                    if (line.Length < 2)
                        break;
                    if (line[1] != 'X' && line[1] != 'x')
                    {
                        line = line.Substring(1);
                        continue;
                    }
                    // find a complete CAN packet
                    l = line.IndexOf(';');
                    if (l == -1)
                        break;
                    string cmd = line.Substring(0, l + 1);
                    line = line.Substring(l + 1);

                    ProcessCanCmd(cmd);
                }
            }
        }

        public void ProcessCanCmd(string cmd)
        {
            if (LogCB.Checked)
                log("Ci> " + cmd);
            string a = cmd.Substring(7, 3);
            if (aliascheck == a) // packet with same alias as being CIDed
                aliascheck = "";
            if (cmd.Substring(3, 4) == FT_NSN || cmd.Substring(3, 4) == FT_INIT
                || cmd.Substring(3, 4) == FT_AMD)
            {
                if (cmd.Length < 20)
                    log("Cmd too short " + cmd);
                else
                {
                    string n = cmd.Substring(11, 12);
                    if (NodeIdTable.ContainsKey(n)) // remove old alias for nodeid
                    {
                        AliasTable.Remove(NodeIdTable[n]);
                        NodeIdTable.Remove(n);
                    }
                    if (AliasTable.ContainsKey(a)) // remove old nodeid for alias
                    {
                        NodeIdTable.Remove(AliasTable[a]);
                        AliasTable.Remove(a);
                    }
                    AliasTable.Add(a, n);
                    NodeIdTable.Add(n, a);
                }
            }
            string data = cmd.Substring(11, cmd.Length - 12);
            string newcmd = "";
            switch (cmd[3])
            {
                case '0': // CAN Control
                    if (cmd.Substring(3, 4) == FT_AME) // alias enquiry
                    {
                        if (NodeIdTable.ContainsKey(cmd.Substring(11, 12)))
                            CAN(FT_AMD + NodeIdTable[cmd.Substring(11, 12)], cmd.Substring(11, 12));
                    }
                    break;
                case '7': // CIM
                    if (AliasTable.ContainsKey(a))
                    {
                        string n = AliasTable[a];
                        if (n.Substring(0, 3) != cmd.Substring(4, 3))
                            CAN(FT_RID + a, ""); // send RID
                    }
                    break;
                case '6': // CIM
                    if (AliasTable.ContainsKey(a))
                    {
                        string n = AliasTable[a];
                        if (n.Substring(3, 3) != cmd.Substring(4, 3))
                            CAN(FT_RID + a, ""); // send RID
                    }
                    break;
                case '5': // CIM
                    if (AliasTable.ContainsKey(a))
                    {
                        string n = AliasTable[a];
                        if (n.Substring(6, 3) != cmd.Substring(4, 3))
                            CAN(FT_RID + a, ""); // send RID
                    }
                    break;
                case '4': // CIM
                    if (AliasTable.ContainsKey(a))
                    {
                        string n = AliasTable[a];
                        if (n.Substring(9, 3) != cmd.Substring(4, 3))
                            CAN(FT_RID + a, ""); // send RID
                    }
                    break;
                case '8':
                    newcmd = "2" + cmd.Substring(4, 3);
                    newcmd += TranslateToNodeID(cmd.Substring(7, 3), cmd) + data;
                    checkpacket(newcmd, true);
                    EthernetSendHexString(newcmd);
                    break;
                case '9':
                    log("Unexpected MTI start 9xxx.");
                    break;
                case 'A': // single frame datagram
                    newcmd = DATAGRAM + TranslateToNodeID(cmd.Substring(7, 3), cmd)
                        + TranslateToNodeID(cmd.Substring(4, 3), cmd) + data;
                    checkpacket(newcmd, true);
                    EthernetSendHexString(newcmd);
                    break;
                case 'B': // start of datagram
                    if (longdatagram.ContainsKey(a)) {
                        longdatagram.Remove(a);
                        log("New datagram without an end of the previous.");
                    }
                    longdatagram.Add(a, DATAGRAM + TranslateToNodeID(cmd.Substring(7, 3), cmd)
                        + TranslateToNodeID(cmd.Substring(4, 3), cmd) + data);
                    break;
                case 'C':
                    if (longdatagram.ContainsKey(a)) // continue datagram
                        longdatagram[a] += data;
                    else // no datagram
                        log("Middle of datagram without a start.");
                    break;
                case 'D':
                    if (longdatagram.ContainsKey(a))
                    {
                        newcmd = longdatagram[a] + data;
                        longdatagram.Remove(a);
                        checkpacket(newcmd, true);
                        EthernetSendHexString(newcmd);
                    }
                    else
                        log("End of datagram without a start.");
                    break;
                case 'E':
                    newcmd = "3" + data.Substring(0, 2) + "0" + TranslateToNodeID(cmd.Substring(7, 3), cmd)
                        + TranslateToNodeID(cmd.Substring(4, 3), cmd) + data.Substring(2);
                    checkpacket(newcmd, true);
                    EthernetSendHexString(newcmd);
                    break;
                case 'F':
                    newcmd = STREAM + TranslateToNodeID(cmd.Substring(7, 3), cmd)
                        + TranslateToNodeID(cmd.Substring(4, 3), cmd) + data;
                    EthernetSendHexString(newcmd);
                    break;
            }
        }

        public void CAN(string s1, string s2)
        {
            s1 = ":X1" + s1 + "N" + s2 + ";";
            if (LogCB.Checked)
                log("Co< " + s1);
            try
            {
                com.WriteLine(s1);
            }
            catch
            {
                log("Com write error");
            }
        }

        // cmd is the string to send including 2 hex digits for length
        public void CANSendHexString(string cmd)
        {
            try
            {
                int length = cmd.Length / 2;
                string sid = TranslateToAlias(cmd.Substring(6, 12));
                if (cmd.Substring(2, 4) == STREAM) // stream
                {
                    string did = TranslateToAlias(cmd.Substring(18, 12));
                    CAN("F" + did + sid, cmd.Substring(18));
                    return;
                }
                if (cmd.Substring(2,4) == DATAGRAM) // datagram
                {
                    string did = TranslateToAlias(cmd.Substring(18, 12));
                    if (length - 15 <= 8)
                    {
                        CAN("A" + did + sid, cmd.Substring(30));
                    }
                    else
                    {
                        length -= 15;
                        int pl = length;
                        if (pl >= 8)
                            pl = 8;
                        CAN("B" + did + sid, cmd.Substring(30, pl * 2));
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
                    return;
                }
                if (cmd[2] == '3' && cmd[5] == '0') // addressed
                {
                    string did = TranslateToAlias(cmd.Substring(18, 12));
                    CAN("E" + did + sid, cmd.Substring(3, 2) + cmd.Substring(30));
                    return;
                }
                if (cmd[2] == '2') // simple broadcast
                {
                    string mti = "8"+cmd.Substring(3,3);
                    CAN(mti + sid, cmd.Substring(18));
                    return;
                }
            }
            catch (Exception e)
            {
                log("CAN output error " + e.ToString());
            }
        }

        //*******************************************************************************************************
        // Alias handling
        //*******************************************************************************************************

        SortedDictionary<string, string> AliasTable = new SortedDictionary<string, string>();
        SortedDictionary<string, string> NodeIdTable = new SortedDictionary<string, string>();
        public string aliascheck = "";

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
                aliascheck = alias;
                CAN(FT_CIM0 + ((nodenumber >> 36) & 0x00000FFF).ToString("X3") + alias, "");
                Thread.Sleep(2);
                if (aliascheck == "") // someone objected
                    continue;
                CAN(FT_CIM1 + ((nodenumber >> 24) & 0x00000FFF).ToString("X3") + alias, "");
                Thread.Sleep(2);
                if (aliascheck == "") // someone objected
                    continue;
                CAN(FT_CIM2 + ((nodenumber >> 12) & 0x00000FFF).ToString("X3") + alias, "");
                Thread.Sleep(2);
                if (aliascheck == "") // someone objected
                    continue;
                CAN(FT_CIM3 + ((nodenumber >> 0) & 0x00000FFF).ToString("X3") + alias, "");
                for (int timeout = 0; timeout < 10; timeout++)
                {
                    Thread.Sleep(25);
                    if (aliascheck == "") // someone objected
                        break;
                }
            } while (aliascheck == "");
            aliascheck = "";

            AliasTable.Add(alias, nodenumber.ToString("X12"));
            NodeIdTable.Add(nodenumber.ToString("X12"), alias);
            CAN(FT_RID + alias, "");
            CAN(FT_AMD + alias, nodenumber.ToString("X12"));
            return alias;
        }

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

        private void AliasBtn_Click(object sender, EventArgs e)
        {
            foreach (KeyValuePair<string, string> kvp in AliasTable)
                log("Aliases " + kvp.Key + " " + kvp.Value);
        }

        //*******************************************************************************************************
        // Ethernet format string packet checking
        //*******************************************************************************************************

        // SortedDictionary<ulong, ulong> events = new SortedDictionary<ulong, ulong>();

        // return true if forwarded to CAN, false if blocked

        public bool checkpacket(string cmd, bool fromCAN)
        {
            string s;
            if (cmd.Substring(0,4)==VERIFYNODEIDS) {
                s = VERIFIEDNID + nodenumberstr + nodenumberstr;
                EthernetSendHexString(s);
                CANSendHexString("00"+s);
                return true;
            }

            if (cmd.Substring(0,4) == DATAGRAM && cmd.Substring(16, 12) == nodenumberstr) // datagram to this node
            {
                if (cmd.Substring(28, 4) == "2060" && cmd.Substring(40, 2) == "FF")
                {
                    // send XML file
                    string address = cmd.Substring(32, 8);
                    int ad = Convert.ToInt32(address, 16);
                    string data = "";
                    int l = Convert.ToInt32(cmd.Substring(42, 2), 16);
                    byte[] utf8bytes = Encoding.UTF8.GetBytes(xml);
                    if (ad + l > utf8bytes.Length)
                        l = utf8bytes.Length - ad;
                    for (int i = 0; i < l; i++)
                        data += ((int)utf8bytes[ad + i]).ToString("X2");
                    s = DATAGRAM + nodenumberstr + cmd.Substring(4, 12) + "2030" + address + "FF" + data;
                    if (l < 64)
                        s += "00";
                    if (fromCAN)
                        CANSendHexString("00"+s);
                    else
                        EthernetSendHexString(s);
                    return false;
                }
                if (cmd.Substring(28, 4) == "2060" && cmd.Substring(40, 2) == "FB")
                {
                    // send node name
                    string data = "";
                    byte[] utf8bytes = Encoding.UTF8.GetBytes(nodename);
                    int l = utf8bytes.Length;
                    for (int i = 0; i < l; i++)
                        data += ((int)utf8bytes[i]).ToString("X2");
                    string str = DATAGRAM + nodenumber.ToString("X12") + cmd.Substring(4, 12) + "203000000000FB" + data + "00";
                    if (fromCAN)
                        CANSendHexString("00" + str);
                    else
                        EthernetSendHexString(str);
                    return false;
                }
                if (cmd.Substring(28, 4) == "2060" && cmd.Substring(40, 2) == "00")
                {
                    // send port speed
                    string address = cmd.Substring(32, 8);
                    string speed = "00000000";
                    try
                    {
                        speed = com.BaudRate.ToString("X8").PadLeft(8, '0');
                    }
                    catch { };
                    string data = "";
                    for (int i = 0; i < 8; i+=2)
                        data = speed.Substring(i,2) + data;
                    s = DATAGRAM + nodenumberstr + cmd.Substring(4, 12) + "2030" + address + "00" + data;
                    if (fromCAN)
                        CANSendHexString("00"+s);
                    else
                        EthernetSendHexString(s);
                    return false;
                }
            }
            return true;
        }

    }
}
