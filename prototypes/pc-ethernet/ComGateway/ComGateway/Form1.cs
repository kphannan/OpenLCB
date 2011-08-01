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
        // CAN MTIs
        const string FT_CIM0 = "7";
        const string FT_CIM1 = "6";
        const string FT_CIM2 = "5";
        const string FT_CIM3 = "4";
        const string FT_RID = "0700";
        const string FT_AMD = "0701";
        const string FT_VNSN = "80A7";
        const string FT_NSN = "90B7";
        const string FT_INIT = "9087";
        const string FT_IDEVENTS = "82B7";
        
        // Ethernet MTIs
        const string INITCOMPLETE = "3080";
        const string VERIFYNODEIDS = "10A0";
        const string VERIFIEDNODEID = "30B0";
        const string IDETIFYEVENTS = "12B0";
        const string IDENTIFIEDCONSUMER = "3263";
        const string IDENTIFIEDCONSUMERRANGE = "3252";
        const string IDENTIFIEDPRODUCER = "32A3";
        const string IDENTIFIEDPRODUCERRANGE = "3292";
        const string EVENT = "12D2";

        public long nodenumber = 0;
        public string nodenumberstr = "";
        public string alias = "";

        public string xml = "<cdi><id><Software>OpenLCB USB/RS232 Com Gateway</Software>"
            + "<Version>Mike Johnson 18 July 2011</Version></id>"
            +"<seg name=\"Port Speed\" space=\"0\" buttons=\"1\"><int name=\"Port Speed\" size=\"4\"/></seg></cdi>";

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
        
       public object loglock = new object();

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
                    if (!(ipa[i].IsIPv6LinkLocal || ipa[i].IsIPv6Teredo || ipa[i].IsIPv6SiteLocal || ipa[i].IsIPv6Multicast))
                        break;
                }
                IPEndPoint ep = new IPEndPoint(ipa[i], port);
                skt.Connect(ep);
                byte[] buffer = new byte[12];
                skt.Receive(buffer);
                if ((buffer[1] << 8) + buffer[2] == 0x3000)
                {
                    nodenumber = ((long)buffer[3] << 40) + ((long)buffer[4] << 32) + (buffer[5] << 24) + (buffer[6] << 16) 
                        + (buffer[7] << 8) + buffer[8];
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
            int read = s.EndReceive(ar);
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
                if (LogCB.Checked)
                    log("Ei> " + cmd);
                if (checkpacket(cmd.Substring(2), false))
                    CANSendHexString(cmd);
            }
            skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
        }

        public void EthernetSendHexString(string s)
        {
            if (!serverconnected)
                return;
            byte[] buffer = new byte[1 + s.Length / 2];
            buffer[0] = (byte)buffer.Length;
            if (LogCB.Checked)
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
                // CAN input
                try
                {
                    Thread.Sleep(10);
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

                ProcessCanCmd(cmd);

            }
        }

        public void ProcessCanCmd(string cmd)
        {
            if (LogCB.Checked)
                log("Ci> " + cmd);
            string a = cmd.Substring(7, 3);
            if (aliascheck == a) // packet with same alias as being CIMed
                aliascheck = "";
            if (cmd.Substring(3, 4) == FT_NSN || cmd.Substring(3, 4) == FT_INIT
                || cmd.Substring(3, 4) == FT_AMD)
            {
                if (cmd.Length < 20)
                    log("Cmd too short " + cmd);
                else
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
            }
            string data = cmd.Substring(11, cmd.Length - 12);
            string newcmd = "";
            switch (cmd[3])
            {
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
                    newcmd = "1" + cmd.Substring(4, 2);
                    if (cmd[6] == '7')
                        newcmd += "0";
                    if (cmd[6] == '3')
                        newcmd += "1";
                    if (cmd[6] == 'F')
                        newcmd += "2";
                    if (cmd[6] == 'B')
                        newcmd += "3";
                    newcmd += TranslateToNodeID(cmd.Substring(7, 3), cmd) + data;
                    checkpacket(newcmd, true);
                    EthernetSendHexString(newcmd);
                    break;
                case '9':
                    newcmd = "3" + cmd.Substring(4, 2);
                    if (cmd[6] == '7')
                        newcmd += "0";
                    if (cmd[6] == '3')
                        newcmd += "1";
                    if (cmd[6] == 'F')
                        newcmd += "2";
                    if (cmd[6] == 'B')
                        newcmd += "3";
                    newcmd += TranslateToNodeID(cmd.Substring(7, 3), cmd) + data;
                    checkpacket(newcmd, true);
                    EthernetSendHexString(newcmd);
                    break;
                case 'B':
                    if (longdatagram.ContainsKey(a))
                        longdatagram.Remove(a);
                    longdatagram.Add(a, "3" + data.Substring(0, 2) + "4" + TranslateToNodeID(cmd.Substring(7, 3), cmd)
                            + TranslateToNodeID(cmd.Substring(4, 3), cmd) + data.Substring(2));
                    break;
                case 'C':
                    if (longdatagram.ContainsKey(a))
                        longdatagram[a] += data;
                    else
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
                    newcmd = "3" + data.Substring(0, 2) + "4" + TranslateToNodeID(cmd.Substring(7, 3), cmd)
                        + TranslateToNodeID(cmd.Substring(4, 3), cmd) + data.Substring(2);
                    checkpacket(newcmd, true);
                    EthernetSendHexString(newcmd);
                    break;
                case 'F':
                    newcmd = "3694" + TranslateToNodeID(cmd.Substring(7, 3), cmd)
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

        public void CANSendHexString(string cmd)
        {
            try
            {
                int length = cmd.Length / 2;
                string sid = TranslateToAlias(cmd.Substring(6, 12));
                if (cmd[2] == '1') // simple broadcast
                {
                    string mti = "8"+cmd.Substring(3,2);
                    if (cmd[5] == '0')
                        mti += "7";
                    if (cmd[5] == '1')
                        mti += "3";
                    if (cmd[5] == '2')
                        mti += "F";
                    if (cmd[5] == '3')
                        mti += "B";
                    CAN(mti + sid, cmd.Substring(18));
                    return;
                }
                if (cmd.Substring(2, 4) == "3694") // stream
                {
                    string did = TranslateToAlias(cmd.Substring(18, 12));
                    CAN("F" + did + sid, cmd.Substring(18));
                    return;
                }
                if (cmd[2] == '3' && cmd[5] == '4') // datagram
                { 
                    string did = TranslateToAlias(cmd.Substring(18, 12));
                    if (length - 15 <= 7)
                    {
                        CAN("E" + did + sid, cmd.Substring(3, 2) + cmd.Substring(30));
                    }
                    else
                    {
                        length -= 15;
                        int pl = length;
                        if (pl >= 7)
                            pl = 7;
                        CAN("B" + did + sid, cmd.Substring(3, 2) + cmd.Substring(30, pl * 2));
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
                if (cmd[2] == '3') // not simple broadcast
                {
                    string mti = "9" + cmd.Substring(3, 2);
                    if (cmd[5] == '0')
                        mti += "7";
                    if (cmd[5] == '1')
                        mti += "3";
                    if (cmd[5] == '2')
                        mti += "F";
                    if (cmd[5] == '3')
                        mti += "B";
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

        SortedDictionary<ulong, ulong> events = new SortedDictionary<ulong, ulong>();

        // return true if forwarded to CAN, false if blocked

        public bool checkpacket(string cmd, bool fromCAN)
        {
            string s;
            if (cmd.Substring(0,4)==VERIFYNODEIDS) {
                s = VERIFIEDNODEID + nodenumberstr + nodenumberstr;
                EthernetSendHexString(s);
                CANSendHexString(s);
                return true;
            }
            if (cmd.Substring(0,4)==INITCOMPLETE || cmd.Substring(0,4)==VERIFIEDNODEID)
                return false;
            if (cmd.Substring(0,4)==IDENTIFIEDCONSUMER)
            {
                ulong ev = Convert.ToUInt64(cmd.Substring(16, 16),16);
                addevent(ev, ev);
                return false;
            }
            if (cmd.Substring(0, 4) == IDENTIFIEDCONSUMERRANGE)
            {
                ulong ev = Convert.ToUInt64(cmd.Substring(16, 16),16);
                ulong r = rangesize(ev);
                addevent(ev & ~r, ev | r);
                return false;
            }
            if (cmd.Substring(0,4)==IDENTIFIEDPRODUCER|| cmd.Substring(0,4)==IDENTIFIEDPRODUCERRANGE)
                return false;
            if (cmd.Substring(0, 4) == EVENT)
            {
                return findevent(Convert.ToUInt64(cmd.Substring(16, 16),16));
            }
            if (cmd[0] == '3' && cmd[3]=='4' && cmd.Substring(16, 12) == nodenumberstr) // datagram to this node
            {
                if (cmd.Substring(0, 4) == "3204" && cmd.Substring(28, 2) == "60" && cmd.Substring(38, 2) == "FF")
                {
                    // send XML file
                    string address = cmd.Substring(30, 8);
                    int ad = Convert.ToInt32(address, 16);
                    string data = "";
                    int l = Convert.ToInt32(cmd.Substring(40, 2), 16);
                    if (ad + l > xml.Length)
                        l = xml.Length - ad;
                    for (int i = 0; i < l; i++)
                        data += ((int)xml[ad + i]).ToString("X2");
                    s = "3204" + nodenumberstr + cmd.Substring(4, 12) + "30" + address + "FF" + data;
                    if (l < 64)
                        s += "00";
                    if (fromCAN)
                        CANSendHexString(s);
                    else
                        EthernetSendHexString(s);
                    return false;
                }
                if (cmd.Substring(0, 4) == "3204" && cmd.Substring(28, 2) == "60" && cmd.Substring(38, 2) == "00")
                {
                    // send port speed
                    string address = cmd.Substring(30, 8);
                    string speed = com.BaudRate.ToString("X8").PadLeft(8,'0');
                    string data = "";
                    for (int i = 0; i < 8; i+=2)
                        data = speed.Substring(i,2) + data;
                    s = "3204" + nodenumberstr + cmd.Substring(4, 12) + "30" + address + "00" + data;
                    if (fromCAN)
                        CANSendHexString(s);
                    else
                        EthernetSendHexString(s);
                    return false;
                }
            }
            if (cmd[0] == '3' && cmd[3] == '4') // datagram filter
            {
                return NodeIdTable.ContainsKey(cmd.Substring(16, 12));
            }
            return true;
        }

        public bool findevent(ulong ev)
        {
            foreach (KeyValuePair<ulong, ulong> kvp in events)
                if (ev >= kvp.Key && ev <= kvp.Value)
                    return true;
            return false;
        }

        public void addevent(ulong lower, ulong upper)
        {
            foreach (KeyValuePair<ulong, ulong> kvp in events)
            {
                if ((lower >= kvp.Key && lower <= kvp.Value) || lower == kvp.Value + 1)
                { // extend upper of range
                    if (events[kvp.Key] < upper)
                        events[kvp.Key] = upper;
                    return;
                }
                else if (upper >= kvp.Key && upper <= kvp.Value)
                { // merge keys
                    events.Add(lower, Math.Max(events[kvp.Key], upper));
                    events.Remove(kvp.Key);
                    return;
                }
            }
            events.Add(lower, upper);
        }

        public ulong rangesize(ulong s)
        {
            ulong mask = 1;
            ulong bit = 1;
            if ((s & 1) == 1)
            {
                for (int i = 0; i < 64; i++)
                {
                    bit <<= 1;
                    if ((s & bit) != bit)
                        break;
                    mask |= bit;
                }
            }
            else
            {
                for (int i = 0; i < 64; i++)
                {
                    bit <<= 1;
                    if ((s & bit) != 0)
                        break;
                    mask |= bit;
                }
            }
            return mask;
        }

        private void EvTableBtn_Click(object sender, EventArgs e)
        {
            foreach (KeyValuePair<ulong, ulong> kvp in events)
                log("Event: " + kvp.Key.ToString("X16") + " - " + kvp.Value.ToString("X16"));
        }

    }
}
