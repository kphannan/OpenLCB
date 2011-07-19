using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Windows.Forms;
using Bonjour;

namespace Throttle
{
    public partial class Throttle : Form
    {
        //*********************************************************************************
        //        Frame Types
        //*********************************************************************************

        enum FT
        {
            FT_CIM0 = 0x7000,   // Top 12 bits of NID
            FT_CIM1 = 0x6000,   // 2nd top 12 bits of NID
            FT_CIM2 = 0x5000,   // 3rd top 12 bits of NID
            FT_CIM3 = 0x4000,   // lowest 12 bits of NID
            FT_RID = 0x0700,   // Reserved ID
            FT_VNSN = 0x80AF,   // Verify Node Serial Number 
            FT_INIT = 0x908F,   // Normal Initialization Complete
            FT_NSN = 0x90BF,   // Node Serial Number 
            FT_EVENT = 0x82DF,   // EVENT - 82DF for JMRI
            FT_RFID = 0x8011,   // RFID tag
            FT_XPRESSNET = 0x8050, // XpressNet raw message from a command station 
            FT_DG = 0xC000,   // Datagram first packets
            FT_DGL = 0xD000,   // Datagram last packet
            FT_DGS = 0xE000,   // Datagram single packet
            FT_STREAM = 0xF000    // Stream data
        };

        // Datagram protocol id, 1st byte of data
        enum DG
        {
            DG_LOGMSG = 0x01,
            DG_LOGREPLY = 0x02,
            DG_VNSN = 0x0A,
            DG_OIR = 0x0C,
            DG_TDE = 0x0D,
            DG_MEMORY = 0x20,
            DG_REMOTE = 0x21,
            DG_DISPLAY = 0x28,
            DG_IDEVNT = 0x2B,
            DG_PSI = 0x2E,
            DG_PSR = 0x2F,
            DG_SDP = 0x4A,
            DG_SR = 0x4B,
            DG_OK = 0x4C,
            DG_ERR = 0x4D,
            DG_SIQ = 0x4E,
            DG_SIR = 0x4F,
        };

        // Memory transfer datagram, protocol id = 0x20
        enum DGM
        {
            DGM_WRITE = 0x20,
            DGM_REPLY = 0x30,
            DGM_READ = 0x60,
            DGM_UPDCOMP = 0xA4,
            DGM_REBOOT = 0xA5,
            DGM_FACTORY = 0xA6,
            DGM_LOADER = 0xA7,
        };

        public string INIT = FT.FT_INIT.ToString("X4");
        public string VERIFYNODEIDS = FT.FT_VNSN.ToString("X4");
        public string VERIFIEDNODEID = FT.FT_NSN.ToString("X4");
        public string XPRESSNET = FT.FT_XPRESSNET.ToString("X4");

        // Bonjour
        private Bonjour.DNSSDService m_service = null;
        private Bonjour.DNSSDEventManager m_eventManager = null;
        private Bonjour.DNSSDService m_browser = null;
        private Bonjour.DNSSDService m_resolver = null;

        static long nodenumber = 0;
        static byte[] inputbuffer = new byte[2000];
        static Socket skt = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        static bool serverconnected = false;
        string xml = "<cdi><id><Software>OpenLCB Simple Throttle</Software>"
            + "<Version>Mike Johnson 9 July 2011</Version></id></cdi>";

        public Throttle()
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
            StartGetNodeNumber();
        }

        public void SendHexString(string s)
        {
            if (!serverconnected)
                return;
            byte[] buffer = new byte[1 + s.Length / 2];
            buffer[0] = (byte)buffer.Length;
            int j = 1;
            for (int i = 0; i < s.Length; i += 2)
                buffer[j++] = (byte)Convert.ToByte(s.Substring(i, 2), 16);
            skt.Send(buffer);
        }

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
                MessageBox.Show("OpenLCB server browse Failed", "Error");
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
                }
                else
                {
                    return;
                }
                skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
                serverconnected = true;
                SendHexString(INIT + nodenumber.ToString("X12") + nodenumber.ToString("X12"));
            }
            catch
            {
            }

        }

        public void checkpacket(string cmd)
        {
            string s;
            if (cmd.Substring(2, 4) == VERIFYNODEIDS)
            {
                s = VERIFIEDNODEID + nodenumber.ToString("X12") + nodenumber.ToString("X12");
                SendHexString(s);
                return;
            }
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
                    if (l < 64)
                        s += "00";
                    SendHexString(s);
                }
                else if (cmd.Substring(2, 4) == "E0A0")
                {
                    s = VERIFIEDNODEID + nodenumber.ToString("X12") + nodenumber.ToString("X12");
                    SendHexString(s);
                }
            }
            return;
        }

        public void InputTask(IAsyncResult ar)
        {
            Socket s = (Socket)ar.AsyncState;
            int read = s.EndReceive(ar);
            string inputstring = "";
            for (int i = 0; i < read; i++)
                inputstring += inputbuffer[i].ToString("X2");
            while (inputstring.Length > 0)
            {
                int length = Convert.ToInt32(inputstring.Substring(0, 2), 16);
                string cmd = inputstring.Substring(0, length * 2);
                if (inputstring.Length > length * 2)
                    inputstring = inputstring.Substring(length * 2);
                else
                    inputstring = "";
                checkpacket(cmd);
            }
            skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
        }

        private void SpeedTB_ValueChanged(object sender, EventArgs e)
        {
            int s = Convert.ToInt16(SpeedTB.Value);
            trackBar1.Value = s;
            if (s == 0) // stop
                s = 0x80;
            else if (s < 0)
            { // rev
                s = ((-s) + 1);
                if (s > 0x7F)
                    s = 0x7F;
            }
            else
            { // fwd
                s = (s + 1);
                if (s > 0x7f)
                    s = 0x7F;
                s |= 0x80;
            }
            int loco = Convert.ToInt32(LocoTB.Text);
            if (loco>=100)
                loco |= 0xC000;

            SendHexString(XPRESSNET + nodenumber.ToString("X12") + "E413" + loco.ToString("X4") + s.ToString("X2") + "00");
        }

        private void StopBtn_Click(object sender, EventArgs e)
        {
            SpeedTB.Value = 0;
            trackBar1.Value = 0;
        }

        private void trackBar1_Scroll(object sender, EventArgs e)
        {
            int s = trackBar1.Value;
            if (s < -126)
                s = -126;
            if (s > 126)
                s = 126;
            SpeedTB.Value = s;
        }

    }
}
