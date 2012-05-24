using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;
using System.Windows.Forms;
using System.Threading;
using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using System.IO;
using System.IO.Ports;
using System.Xml;
using Bonjour;

namespace OlcbSvr
{
    public partial class OlcbSvr : Form
    {
        const int NODENUMBER = 0x0000;
        const int NOFILTER = 0x2017;
        const int INITCOMPLETE = 0x2087;
        const int VERIFYNODEIDS = 0x28A7;
        const int VERIFIEDNID = 0x28B7;
        const int IDENTIFYCONSUMERS = 0x2A4F;
        const int CONSUMERRANGE = 0x225F;
        const int CONSUMERINDENTIFIED = 0x226B;
        const int IDENTIFYPRODUCERS = 0x2A8F;
        const int PRODUCERRANGE = 0x229F;
        const int PRODUCERINDENTIFIED = 0x22AB;
        const int IDENTIFYEVENTS = 0x2AB7;
        const int EVENT = 0x2ADF;
        const int XPRESSNET = 0x2517;
        const int DATAGRAM = 0x3400;
        const int DATAGRAMACK = 0x34C0;
        const int DATAGRAMNACK = 0x34D0;
        const int STREAM = 0x3690;

        //***************************************************************************
        // Connection class
        //***************************************************************************

        class CONNECTION
        {
            public Socket skt;
            public bool inuse;
            public bool filter;
            public long nodenumber;
            public List<long> nodeids = new List<long>();
            public SortedDictionary<ulong, ulong> events = new SortedDictionary<ulong, ulong>();
            public byte[] buffer = new byte[1600];

            public CONNECTION()
            {
                inuse = false;
                filter = true;
            }

            public void UpdateFilters(byte[] buffer, int start, bool localhub)
            {
                int mti = buffer[start + 1] << 8 | buffer[start + 2];
                if (mti == NOFILTER)
                    filter = false;
                else if (mti == INITCOMPLETE || mti == VERIFIEDNID)
                {
                    long srcnode = ((long)buffer[start + 3] << 40) + ((long)buffer[start + 4] << 32)
                        + ((long)buffer[start + 5] << 24) + ((long)buffer[start + 6] << 16)
                        + ((long)buffer[start + 7] << 8) + (long)buffer[start + 8];
                    if (!nodeids.Contains(srcnode))
                        nodeids.Add(srcnode);
                }
                else if (!localhub && (mti&0xFFFC) == (CONSUMERINDENTIFIED&0xFFFC))
                {
                    ulong ev = ((ulong)buffer[start + 9] << 56) + ((ulong)buffer[start + 10] << 48)
                        + ((ulong)buffer[start + 11] << 40) + ((ulong)buffer[start + 12] << 32)
                        + ((ulong)buffer[start + 13] << 24) + ((ulong)buffer[start + 14] << 16)
                        + ((ulong)buffer[start + 15] << 8) + (ulong)buffer[start + 16];
                    addevent(ev, ev);
                }
                else if (!localhub && mti == CONSUMERRANGE)
                {
                    ulong ev = ((ulong)buffer[start + 9] << 56) + ((ulong)buffer[start + 10] << 48)
                        + ((ulong)buffer[start + 11] << 40) + ((ulong)buffer[start + 12] << 32)
                        + ((ulong)buffer[start + 13] << 24) + ((ulong)buffer[start + 14] << 16)
                        + ((ulong)buffer[start + 15] << 8) + (ulong)buffer[start + 16];
                    ulong r = rangesize(ev);
                    addevent(ev & ~r, ev | r);
                }
            }

            public bool CheckFilter(byte[] buffer, int start)
            {
                if (!filter)
                    return true;
                int mti = buffer[start + 1] << 8 | buffer[start + 2];
                if (mti == NOFILTER || mti == INITCOMPLETE || mti == VERIFIEDNID
                    || (mti & 0xFFFC) == (CONSUMERINDENTIFIED & 0xFFFC) || mti == CONSUMERRANGE
                    || (mti & 0xFFFC) == (PRODUCERINDENTIFIED & 0xFFFC) || mti == PRODUCERRANGE)
                    return false;
                if ((mti & 0xF00F) == 0x3004) // datagram
                {
                    long dest = ((long)buffer[start + 9] << 40) + ((long)buffer[start + 10] << 32)
                        + ((long)buffer[start + 11] << 24) + ((long)buffer[start + 12] << 16)
                        + ((long)buffer[start + 13] << 8) + (long)buffer[start + 14];
                    if (nodeids.Contains(dest))
                        return true;
                    return false;
                }
                if (mti == EVENT) // event
                {
                    ulong ev = ((ulong)buffer[start + 9] << 56) + ((ulong)buffer[start + 10] << 48)
                        + ((ulong)buffer[start + 11] << 40) + ((ulong)buffer[start + 12] << 32)
                        + ((ulong)buffer[start + 13] << 24) + ((ulong)buffer[start + 14] << 16)
                        + ((ulong)buffer[start + 15] << 8) + (ulong)buffer[start + 16];
                    foreach (KeyValuePair<ulong, ulong> kvp in events)
                        if (ev >= kvp.Key && ev <= kvp.Value)
                            return true;
                    return false;
                }
                return true;
            }
            
            private void addevent(ulong lower, ulong upper)
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

            private ulong rangesize(ulong s)
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
        }

        //***************************************************************************
        // Main program
        //***************************************************************************

        static int MAXCONNECTIONS = 15;
        static CONNECTION[] connects;

        static long servernodenumber = 0x030400000000 + (2418 << 8) + 0xF0;
        static int port = 0;
        static IPEndPoint ep;
        static Socket ServerSkt = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        static Socket NumberServerSkt = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        static bool limitreached = false;
        static bool localhub = false;

        // Bonjour
        private Bonjour.DNSSDService m_service = null;
        private Bonjour.DNSSDService m_registrar = null;
        private Bonjour.DNSSDEventManager m_eventManager = null;
        private Bonjour.DNSSDService m_browser = null;
        private Bonjour.DNSSDService m_resolver = null;
        private Bonjour.DNSSDError m_error = 0;

        static object loglock = new object();
        static object sendlock = new object();

        public OlcbSvr()
        {
            InitializeComponent();
            CheckForIllegalCrossThreadCalls = false;
            try
            {
                m_service = new DNSSDService();
                m_eventManager = new DNSSDEventManager(); 
                m_eventManager.OperationFailed += new _IDNSSDEvents_OperationFailedEventHandler(OperationFailed);
             }
            catch
            {
                log("Bonjour Service is not available");
            }
            
            String[] arguments = Environment.GetCommandLineArgs();
            for (int a = 1; a < arguments.Length; a++)
            {
                localhub = true;
                log("Arg " + arguments[a]);
                if (arguments[a].StartsWith("port"))
                {
                    int p = arguments[a].IndexOf('=');
                    if (p > 0)
                        port = Convert.ToInt32(arguments[a].Substring(p+1));
                }
                if (arguments[a].StartsWith("max"))
                {
                    int p = arguments[a].IndexOf('=');
                    if (p > 0)
                        MAXCONNECTIONS = Convert.ToInt32(arguments[a].Substring(p+1));
                }
            }

            if (!localhub)
            {
                // create the async listening sockets
                ep = new IPEndPoint(IPAddress.Any, 0);
                ServerSkt.Bind(ep);
                ServerSkt.Listen(1);
                ServerSkt.BeginAccept(new AsyncCallback(Acceptcallback), 0);
                ep = ((IPEndPoint)ServerSkt.LocalEndPoint);
                log("OpenLCB Hub start on port " + ep.Port.ToString());
                // register server with zeroconfig, (alias bonjour)
                // params (flags, interface, instancename, servicetype, domain, host, port, TXTrecord, eventmanager)
                m_registrar = m_service.Register(DNSSDFlags.kDNSSDFlagsNoAutoRename, 0, "OpenLCB Hub Service",
                    "_OpenLCB._tcp", null, null, (ushort)ep.Port, null, m_eventManager);
            }
            if (localhub)
            {
                // connect via bonjour
                StartOpenLCBConnect();

                // create the async listening sockets
                ep = new IPEndPoint(IPAddress.Loopback, port);
                ServerSkt.Bind(ep);
                ServerSkt.Listen(1);
                ServerSkt.BeginAccept(new AsyncCallback(Acceptcallback), 0);
                ep = ((IPEndPoint)ServerSkt.LocalEndPoint);
                log("OpenLCB Hub start on port " + ep.Port.ToString());
            }

            if (!localhub)
                readxmldata();

            MaxConTB.Text = MAXCONNECTIONS.ToString();
            connects = new CONNECTION[MAXCONNECTIONS];
            for (int i = 0; i < MAXCONNECTIONS; i++)
                connects[i] = new CONNECTION();

            // node number server range
            GroupBox.Items.Add("NMRA");
            GroupBox.Items.Add("MERG");
            GroupBox.Items.Add("Fremo");
            GroupBox.Items.Add("Ntrak");
            RangeFromTB.Text = servernodenumber.ToString("X12");
            SetGroup(RangeFromTB.Text);
            byte6txt.Text = Convert.ToInt32(RangeFromTB.Text.Substring(10, 2), 16).ToString();
            membertxt.Text = Convert.ToInt32(RangeFromTB.Text.Substring(4, 6), 16).ToString();

            inithub();
        }

        public void OperationFailed(DNSSDService service, DNSSDError err)
        {
            m_error = err;
            if (err == DNSSDError.kDNSSDError_NameConflict)
                log("Bonjour error, Hub service already started.");
            else
                log("Bonjour error " + err);
        }

        public void readxmldata()
        {
            try
            {
                StreamReader file = new StreamReader("hubdata.xml");
                XmlDocument xmldoc = new XmlDocument();
                xmldoc.LoadXml(file.ReadToEnd());
                file.Close();
                XmlNode docnode = xmldoc.FirstChild.FirstChild;
                while (docnode != null)
                {
                    if ("nodenumber".StartsWith(docnode.Name))
                        servernodenumber = Convert.ToInt64(docnode.InnerText, 16);
                    if ("max".StartsWith(docnode.Name))
                        MAXCONNECTIONS = Convert.ToInt32(docnode.InnerText);
                    if ("log".StartsWith(docnode.Name))
                        LogCB.Checked = Convert.ToBoolean(docnode.InnerText);
                    docnode = docnode.NextSibling;
                }
            }
            catch (Exception e) 
            {
                log("Xml file " + e);
            };
        }

        public void writexmldata()
        {
            if (localhub)
                return;
            try
            {
                StreamWriter savefile = new StreamWriter("hubdata.xml", false);
                savefile.WriteLine("<hubconfig version=\"1\">");
                savefile.WriteLine("<nodenumber>" + servernodenumber.ToString("X12") + "</nodenumber>");
                savefile.WriteLine("<max>" + MAXCONNECTIONS.ToString() + "</max>");
                savefile.WriteLine("<log>" + LogCB.Checked.ToString() + "</log>");
                savefile.WriteLine("</hubconfig>");
                savefile.Close();
            }
            catch (Exception e)
            {
                log("Save failed " + e);
            }
        }

        public void inithub()
        {
            servernodenumber = Convert.ToInt64(RangeFromTB.Text, 16);
            for (int i = 0; i < MAXCONNECTIONS; i++)
            {
                try {
                    connects[i].skt.Shutdown(SocketShutdown.Both);
                    connects[i].skt.Close();
                }
                catch {};
                connects[i].inuse = false;
                connects[i].filter = true;
                connects[i].nodenumber = servernodenumber + i + 1;
            }
        }

        private void Server_FormClosing(object sender, FormClosingEventArgs e)
        {
            writexmldata();
            if (m_registrar != null)
                m_registrar.Stop();
            m_eventManager.OperationFailed -= new _IDNSSDEvents_OperationFailedEventHandler(OperationFailed);
        }

        //***************************************************************************
        // Connect to OpenLCB
        //***************************************************************************

        public void StartOpenLCBConnect()
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
                log("OpenLCB Server browse Failed");
                Thread.Sleep(1000);
                m_eventManager.ServiceFound -= new _IDNSSDEvents_ServiceFoundEventHandler(ServiceFound);
                Application.Exit();
            }
        }

        // callback from browse
        // params service discovery ref, status flags, interface index, error code ?, service name,
        //      registration type, domain, context=null
        public void ServiceFound(DNSSDService sref, DNSSDFlags flags, uint ifIndex, String serviceName,
            String regType, String domain)
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
                log("Unable to Resolve service");
                Thread.Sleep(1000);
                // tidy up
                m_eventManager.ServiceResolved -= new _IDNSSDEvents_ServiceResolvedEventHandler(ServiceResolved);
                Application.Exit();
            }
        }

        public void ServiceResolved(DNSSDService sref, DNSSDFlags flags, uint ifIndex, String fullName,
            String hostName, ushort port, TXTRecord txtRecord)
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
                    if (ipa[i].AddressFamily == AddressFamily.InterNetwork)
                        break;
                }
                IPEndPoint ep = new IPEndPoint(ipa[i], port);
                connects[0].skt.Connect(ep);
                connects[0].inuse = true;
                byte[] buffer = new byte[12];
                connects[0].skt.Receive(buffer);
                if ((buffer[1] << 8) + buffer[2] == NODENUMBER)
                {
                    connects[0].nodenumber = ((long)buffer[3] << 40) + ((long)buffer[4] << 32) + (buffer[5] << 24)
                        + (buffer[6] << 16) + (buffer[7] << 8) + buffer[8];
                    log("OpenLCB Node Number " + connects[0].nodenumber.ToString("X12"));
                }
                else
                {
                    log("No node number allocated.");
                    return;
                }

                while (true)
                {
                    int size = connects[0].skt.Receive(connects[0].buffer);
                    if (size == 0)
                        break;
                    if (LogCB.Checked)
                    {
                        string l = "";
                        for (i = 0; i < size; i++)
                            l += connects[0].buffer[i].ToString("X2");
                        log("< (" + 0.ToString() + ") " + l);
                    }
                    int p = 0;
                    while (p < size)
                    {
                        connects[0].UpdateFilters(connects[0].buffer, p, localhub);
                        SendToAll(connects[0].buffer, p, 0);
                        p += connects[0].buffer[p];
                    }
                } // end of while true
            }
            catch (Exception e)
            {
                log("OpenLCB server connection failed " + e.ToString());
            }
        }

        //***************************************************************************
        // Logging
        //***************************************************************************

        private void log(string m)
        {
            lock (loglock)
            {
                m = Environment.TickCount.ToString("D9") + " " + m;
                string s = LogTB.Text;
                if (s.Length>5000)
                    s = s.Substring(0, 5000);
                LogTB.Text = m + "\r\n" + s;
                LogTB.Refresh();
            }
        }

        //***************************************************************************
        // Ethernet connection
        //***************************************************************************

        private void Acceptcallback(IAsyncResult result)
        {
            int i;
            int index = (int)result.AsyncState;
            try
            {
                connects[index].inuse = true;
                log("Accept " + index.ToString() + ", Node number " + connects[index].nodenumber.ToString("X12"));
                connects[index].skt = ServerSkt.EndAccept(result);
                // start a new accept
                limitreached = true;
                for (i = 0; i < MAXCONNECTIONS; i++)
                {
                    if (!connects[i].inuse)
                    {
                        limitreached = false;
                        ServerSkt.BeginAccept(new AsyncCallback(Acceptcallback), i);
                        break;
                    }
                }
                if (limitreached)
                {
                    log("Connections limit. No new accept started.");
                    // stop Bonjour ?
                    m_registrar.Stop();
                }

                connects[index].nodeids.Clear();
                connects[index].events.Clear();

                // send node number
                connects[index].buffer[0] = 9;
                connects[index].buffer[1] = NODENUMBER >> 8;
                connects[index].buffer[2] = NODENUMBER & 0xFF;
                connects[index].buffer[3] = (byte)(connects[index].nodenumber >> 40);
                connects[index].buffer[4] = (byte)(connects[index].nodenumber >> 32);
                connects[index].buffer[5] = (byte)(connects[index].nodenumber >> 24);
                connects[index].buffer[6] = (byte)(connects[index].nodenumber >> 16);
                connects[index].buffer[7] = (byte)(connects[index].nodenumber >> 8);
                connects[index].buffer[8] = (byte)connects[index].nodenumber;
                connects[index].skt.Send(connects[index].buffer, 9, SocketFlags.None);

                // send VerifyNodeIDs
                connects[index].buffer[0] = 9;
                connects[index].buffer[1] = VERIFYNODEIDS >> 8;
                connects[index].buffer[2] = VERIFYNODEIDS & 0xFF;
                connects[index].buffer[3] = (byte)(servernodenumber >> 40);
                connects[index].buffer[4] = (byte)(servernodenumber >> 32);
                connects[index].buffer[5] = (byte)(servernodenumber >> 24);
                connects[index].buffer[6] = (byte)(servernodenumber >> 16);
                connects[index].buffer[7] = (byte)(servernodenumber >> 8);
                connects[index].buffer[8] = (byte)servernodenumber;
                connects[index].skt.Send(connects[index].buffer, 9, SocketFlags.None);

                // send Identify Events
                connects[index].buffer[0] = 9;
                connects[index].buffer[1] = IDENTIFYEVENTS >> 8;
                connects[index].buffer[2] = IDENTIFYEVENTS & 0xFF;
                connects[index].buffer[3] = (byte)(servernodenumber >> 40);
                connects[index].buffer[4] = (byte)(servernodenumber >> 32);
                connects[index].buffer[5] = (byte)(servernodenumber >> 24);
                connects[index].buffer[6] = (byte)(servernodenumber >> 16);
                connects[index].buffer[7] = (byte)(servernodenumber >> 8);
                connects[index].buffer[8] = (byte)servernodenumber;
                connects[index].skt.Send(connects[index].buffer, 9, SocketFlags.None);

                connects[index].nodeids.Add(connects[index].nodenumber); // should be done by InitComplete
                
                while (true)
                {
                    int size = connects[index].skt.Receive(connects[index].buffer);
                    if (size==0)
                        break;
                    if (LogCB.Checked)
                    {
                        string l = "";
                        for (i = 0; i < size; i++)
                            l += connects[index].buffer[i].ToString("X2");
                        log("< (" + index.ToString() + ") " + l);
                    }
                    int p = 0;
                    while (p < size)
                    {
                        connects[index].UpdateFilters(connects[index].buffer, p, localhub);
                        SendToAll(connects[index].buffer, p, index);
                        p += connects[index].buffer[p];
                    }
                } // end of while true
            }
            catch (Exception e)
            {
                log("Exception in accept " + e.ToString());
            }

            // Close connection
            try
            {
                connects[index].skt.Shutdown(SocketShutdown.Both);
                connects[index].skt.Close();
            }
            catch (SocketException)
            {
            }
            connects[index].inuse = false;
            log("Connection closed " + index.ToString());
            if (limitreached)
            {
                limitreached = false;
                ServerSkt.BeginAccept(new AsyncCallback(Acceptcallback), index);
                // m_registrar = m_service.Register(0, 0, Environment.UserName, "_OpenLCB._tcp", null, null, (ushort)ep.Port, null, null);
            }
        }

        public void SendToAll(byte[] buffer, int start, int index)
        {
            lock (sendlock)
            {
                int i;
                int size = buffer[start];
                string l = "";
                if (LogCB.Checked)
                {
                    for (i = 0; i < size; i++)
                        l += buffer[start + i].ToString("X2");
                }
                for (i = 0; i < MAXCONNECTIONS; i++)
                {
                    if (i != index && connects[i].inuse 
                        && (localhub || FilterCB.Checked || connects[i].CheckFilter(buffer, start)))
                    {
                        connects[i].skt.Send(buffer, start, size, SocketFlags.None);
                        if (LogCB.Checked)
                            log("> (" + i.ToString() + ") " + l);
                    }
                }
            }
        }

        //***************************************************************************
        // Set start of node number range
        //***************************************************************************
 
        private void GroupBox_SelectedIndexChanged(object sender, EventArgs e)
        {
            string s = GroupBox.Text;
            string n = "0000";
            if (s == "NMRA")
                n = "0302";
            else if (s == "MERG")
                n = "0304";
            else if (s == "Fremo")
                n = "0306";
            else if (s == "Ntrak")
                n = "0308";
            RangeFromTB.Text = n + RangeFromTB.Text.PadLeft(12, '0').Substring(4,8);
            byte6txt.Text = Convert.ToInt32(RangeFromTB.Text.Substring(10, 2), 16).ToString();
            membertxt.Text = Convert.ToInt32(RangeFromTB.Text.Substring(4, 6), 16).ToString();
            inithub();
        }

        private void SetGroup(string s)
        {
            s = s.Substring(0, 4);
            GroupBox.Text = "Not set";
            if (s == "0302")
                GroupBox.Text = "NMRA";
            else if (s == "0304")
                GroupBox.Text = "MERG";
            else if (s == "0306")
                GroupBox.Text = "Fremo";
            else if (s == "0308")
                GroupBox.Text = "Ntrak";
        }

        private void membertxt_Validating(object sender, CancelEventArgs e)
        {
            RangeFromTB.Text = RangeFromTB.Text.Substring(0, 4) + Convert.ToInt32(membertxt.Text).ToString("X6")
                + RangeFromTB.Text.Substring(10, 2);
            byte6txt.Text = Convert.ToInt32(RangeFromTB.Text.Substring(10, 2), 16).ToString();
            inithub();
        }

        private void byte6txt_Validating(object sender, CancelEventArgs e)
        {
            RangeFromTB.Text = RangeFromTB.Text.Substring(0, 10) + Convert.ToInt32(byte6txt.Text).ToString("X2");
            inithub();
        }

        private void RangeFromTB_Validating(object sender, CancelEventArgs e)
        {
            RangeFromTB.Text = RangeFromTB.Text.PadLeft(12, '0');
            SetGroup(RangeFromTB.Text);
            byte6txt.Text = Convert.ToInt32(RangeFromTB.Text.Substring(10, 2), 16).ToString();
            membertxt.Text = Convert.ToInt32(RangeFromTB.Text.Substring(4, 6), 16).ToString();
            inithub();
        }

        //***************************************************************************
        // Buttons
        //***************************************************************************

        private void NodeBtn_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < MAXCONNECTIONS; i++)
            {
                if (connects[i].inuse)
                {
                    foreach (long n in connects[i].nodeids)
                        log("Connection " + i.ToString() + " Node " + n.ToString("X12"));
                }
            }
        }

        private void EventBtn_Click(object sender, EventArgs e)
        {
            for (int i = 0; i < MAXCONNECTIONS; i++)
            {
                if (connects[i].inuse)
                {
                    foreach (KeyValuePair<ulong, ulong> kvp in connects[i].events)
                        log("Connection " + i.ToString() + " Event: " + kvp.Key.ToString("X16") 
                            + " - " + kvp.Value.ToString("X16"));
                }
            }
        }


    }
}

 