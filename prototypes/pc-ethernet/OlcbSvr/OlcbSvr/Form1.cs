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
using Bonjour;

namespace OlcbSvr
{
    public partial class OlcbSvr : Form
    {
        const int MAXCONNECTIONS = 15;
        const int INITCOMPLETE = 0x3080;
        const int VERIFIEDNID = 0x30B0;
        const int CONSUMERINDENTIFIED = 0x3263;
        const int CONSUMERRANGE = 0x3252;
        const int EVENT = 0x12D2;

        class CONNECTION
        {
            public Socket skt;
            public bool inuse;
            public long nodenumber;
            public List<long> nodeids = new List<long>();
            public SortedDictionary<ulong, ulong> events = new SortedDictionary<ulong, ulong>();

            public void UpdateFilters(byte[] buffer, int start)
            {
                int mti = buffer[start + 1] << 8 | buffer[start + 2];
                if (mti == INITCOMPLETE || mti == VERIFIEDNID)
                {
                    long srcnode = ((long)buffer[start + 3] << 40) + ((long)buffer[start + 4] << 32)
                        + ((long)buffer[start + 5] << 24) + ((long)buffer[start + 6] << 16)
                        + ((long)buffer[start + 7] << 8) + (long)buffer[start + 8];
                    if (!nodeids.Contains(srcnode))
                        nodeids.Add(srcnode);
                }
                else if (mti == CONSUMERINDENTIFIED)
                {
                    ulong ev = ((ulong)buffer[start + 9] << 56) + ((ulong)buffer[start + 10] << 48)
                        + ((ulong)buffer[start + 11] << 40) + ((ulong)buffer[start + 12] << 32)
                        + ((ulong)buffer[start + 13] << 24) + ((ulong)buffer[start + 14] << 16)
                        + ((ulong)buffer[start + 15] << 8) + (ulong)buffer[start + 16];
                    addevent(ev, ev);
                }
                else if (mti == CONSUMERRANGE)
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
                int mti = buffer[start + 1] << 8 | buffer[start + 2];
                if ((mti & 0xF00F) == 0x3004) // datagram
                {
                    long dest = ((long)buffer[start + 9] << 40) + ((long)buffer[start + 10] << 32)
                        + ((long)buffer[start + 11] << 24) + ((long)buffer[start + 12] << 16)
                        + ((long)buffer[start + 13] << 8) + (long)buffer[start + 14];
                    if (nodeids.Contains(dest))
                        return true;
                    return false;
                }
                else if (mti == EVENT) // event
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

        static CONNECTION[] connects = new CONNECTION[MAXCONNECTIONS];

        static long servernodenumber = 0x030400000000 + (2418 << 8) + 0xF0;
        static Socket ServerSkt = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        static Socket NumberServerSkt = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);

        // Bonjour
        private Bonjour.DNSSDService m_service = null;
        private Bonjour.DNSSDEventManager m_eventManager = null;
        private Bonjour.DNSSDService m_registrar = null;

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
            }
            catch
            {
                MessageBox.Show("Bonjour Service is not available", "Error");
                Application.Exit();
            }

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

            // create the async listening sockets
            IPEndPoint ep = new IPEndPoint(IPAddress.Any, 0);
            ServerSkt.Bind(ep);
            ServerSkt.Listen(MAXCONNECTIONS);
            ServerSkt.BeginAccept(new AsyncCallback(Acceptcallback), 0);
            ep = ((IPEndPoint)ServerSkt.LocalEndPoint);
            log("OpenLCB Server start on port " + ep.Port.ToString());

            // register server with zeroconfig, (alias bonjour)
            m_registrar = m_service.Register(0, 0, System.Environment.UserName, "_OpenLCB._tcp", null, null, (ushort)ep.Port, null, null);
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
                connects[i].nodenumber = servernodenumber + i + 1;
            }
        }

        private void Server_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (m_registrar != null)
                m_registrar.Stop();
        }

        //***************************************************************************
        // Logging
        //***************************************************************************

        private void log(string m)
        {
            lock (loglock)
            {
                m = DateTime.Now.ToLongDateString() + " " + DateTime.Now.ToLongTimeString() + " " + m;
                string s = LogTB.Text;
                if (s.Length>5000)
                    s = s.Substring(0, 5000);
                LogTB.Text = m + "\r\n" + s;
                LogTB.Refresh();
            }
        }

        //***************************************************************************
        // OpenLCB server
        //***************************************************************************

        // Runs as a separate thread to handle a connection.
        // Re-connection is handled as a new connection because the loco/train last controlled 
        //    has probably been taken by another throttle.

        private void Acceptcallback(IAsyncResult result)
        {
            byte[] buffer = new byte[1600];
            int i;
            int index = (int)result.AsyncState;
            try
            {
                connects[index].inuse = true;
                log("Accept " + index.ToString() + ", Node number " + connects[index].nodenumber.ToString("X12"));
                connects[index].skt = ServerSkt.EndAccept(result);
                // start a new accept
                bool ok = false;
                for (i = 0; i < MAXCONNECTIONS; i++)
                {
                    if (!connects[i].inuse)
                    {
                        ok = true;
                        ServerSkt.BeginAccept(new AsyncCallback(Acceptcallback), i);
                        break;
                    }
                }
                if (!ok)
                    log("Connections limit. No new accept started.");

                // send node number
                buffer[0] = 9;
                buffer[1] = 0x30;
                buffer[2] = 0x00;
                buffer[3] = (byte)(connects[index].nodenumber >> 40);
                buffer[4] = (byte)(connects[index].nodenumber >> 32);
                buffer[5] = (byte)(connects[index].nodenumber >> 24);
                buffer[6] = (byte)(connects[index].nodenumber >> 16);
                buffer[7] = (byte)(connects[index].nodenumber >> 8);
                buffer[8] = (byte)connects[index].nodenumber;
                connects[index].skt.Send(buffer, 9, SocketFlags.None);

                connects[index].nodeids.Clear();
                connects[index].events.Clear();
                connects[index].nodeids.Add(connects[index].nodenumber); // should be done by InitComplete
                while (true)
                {
                    int size = connects[index].skt.Receive(buffer);
                    if (size==0)
                        break;
                    if (LogCB.Checked)
                    {
                        string l = "";
                        for (i = 0; i < size; i++)
                            l += buffer[i].ToString("X2");
                        log("< (" + index.ToString() + ") " + l);
                    }
                    int p = 0;
                    while (p < size)
                    {
                        connects[index].UpdateFilters(buffer, p);
                        SendToAll(buffer, p, index);
                        p += buffer[p];
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
                    if (i != index && connects[i].inuse && connects[i].CheckFilter(buffer, start))
                    {
                        connects[i].skt.Send(buffer, start, size, SocketFlags.None);
                        if (LogCB.Checked)
                            log("> (" + i.ToString() + ") " +l);
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

 