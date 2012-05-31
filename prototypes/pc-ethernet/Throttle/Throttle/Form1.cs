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
            + "<Version>Mike Johnson 31 May 2012, マイク12年5月31日</Version></id></cdi>";

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
                    if (!(ipa[i].IsIPv6LinkLocal || ipa[i].IsIPv6SiteLocal || ipa[i].IsIPv6Multicast))
                        break;
                }
                IPEndPoint ep = new IPEndPoint(ipa[i], port);
                skt.Connect(ep);
                byte[] buffer = new byte[12];
                skt.Receive(buffer);
                if ((buffer[1] << 8) + buffer[2] == NODENUMBER)
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
                SendHexString(INITCOMPLETE + nodenumber.ToString("X12") + nodenumber.ToString("X12"));
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
                s = VERIFIEDNID + nodenumber.ToString("X12") + nodenumber.ToString("X12");
                SendHexString(s);
                return;
            }
            if (cmd.Substring(2,4) == DATAGRAM && cmd.Substring(18, 12) == nodenumber.ToString("X12")) // datagram to this node
            {
                if (cmd.Substring(30, 4) == "2060" && cmd.Substring(42, 2) == "FF")
                {
                    // send XML file
                    string address = cmd.Substring(34, 8);
                    int ad = Convert.ToInt32(address, 16);
                    string data = "";
                    int l = Convert.ToInt32(cmd.Substring(44, 2), 16);
                    byte[] utf8bytes = Encoding.UTF8.GetBytes(xml);
                    if (ad + l > utf8bytes.Length)
                        l = utf8bytes.Length - ad;
                    for (int i = 0; i < l; i++)
                        data += ((int)utf8bytes[ad + i]).ToString("X2");
                    s = DATAGRAM + nodenumber.ToString("X12") + cmd.Substring(6, 12) + "2030" + address + "FF" + data;
                    if (l < 64)
                        s += "00";
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
