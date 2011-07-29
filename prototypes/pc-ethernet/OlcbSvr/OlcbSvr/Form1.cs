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
        const int MAXCONNECTIONS = 16;
        static Socket ServerSkt = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        static Socket NumberServerSkt = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        static Socket[] connections = new Socket[MAXCONNECTIONS];
        static bool[] inuse = new bool[MAXCONNECTIONS];
        static long[] nodenumberused = new long[MAXCONNECTIONS];

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

            // node number server range
            GroupBox.Items.Add("NMRA");
            GroupBox.Items.Add("MERG");
            GroupBox.Items.Add("Fremo");
            GroupBox.Items.Add("Ntrak");
            RangeFromTB.Text = (0x030400000000 + (2418 << 8) + 0xF0).ToString("X12");
            SetGroup(RangeFromTB.Text);
            byte6txt.Text = Convert.ToInt32(RangeFromTB.Text.Substring(10, 2), 16).ToString();
            membertxt.Text = Convert.ToInt32(RangeFromTB.Text.Substring(4, 6), 16).ToString();

            for (int i = 0; i < MAXCONNECTIONS; i++)
            {
                inuse[i] = false;
                nodenumberused[i] = Convert.ToInt64(RangeFromTB.Text, 16) + i;
            }

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
            Byte[] buffer = new Byte[1600];
            int i;
            int index = (int)result.AsyncState;
            try
            {
                inuse[index] = true;
                log("Accept " + index.ToString() + ", Node number " + nodenumberused[index].ToString("X12"));
                connections[index] = ServerSkt.EndAccept(result);
                // start a new accept
                bool ok = false;
                for (i = 0; i < MAXCONNECTIONS; i++)
                {
                    if (!inuse[i])
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
                buffer[3] = (byte)(nodenumberused[index] >> 40);
                buffer[4] = (byte)(nodenumberused[index] >> 32);
                buffer[5] = (byte)(nodenumberused[index] >> 24);
                buffer[6] = (byte)(nodenumberused[index] >> 16);
                buffer[7] = (byte)(nodenumberused[index] >> 8);
                buffer[8] = (byte)nodenumberused[index];
                connections[index].Send(buffer,9,SocketFlags.None);

                while (true)
                {
                    int size = connections[index].Receive(buffer);
                    if (size==0)
                        break;
                    if (LogCB.Checked)
                    {
                        string l = "< (" + index.ToString() + ") ";
                        for (i = 0; i < size; i++)
                            l += buffer[i].ToString("X2");
                        log(l);
                    }
                    SendToAll(buffer, size, index);
                } // end of while true
            }
            catch (Exception e)
            {
                log("Exception in accept " + e.ToString());
            }

            // Close connection
            try
            {
                connections[index].Shutdown(SocketShutdown.Both);
                connections[index].Close();
            }
            catch (SocketException)
            {
            }
            inuse[index] = false;
            log("Connection closed " + index.ToString());
        }

        public void SendToAll(Byte[] buffer, int size, int index)
        {
            lock (sendlock)
            {
                int i;
                string l = "";
                for (i = 0; i < size; i++)
                    l += buffer[i].ToString("X2");
                for (i = 0; i < MAXCONNECTIONS; i++)
                    if (i != index && inuse[i])
                    {
                        connections[i].Send(buffer, size, SocketFlags.None);
                        if (LogCB.Checked)
                            log("> (" + i.ToString() + ") " +l);
                    }
            }
        }

        // Set start of node number range
 
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
            for (int i = 0; i < MAXCONNECTIONS; i++)
                nodenumberused[i] = Convert.ToInt64(RangeFromTB.Text, 16) + i;
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
            for (int i = 0; i < MAXCONNECTIONS; i++)
                nodenumberused[i] = Convert.ToInt64(RangeFromTB.Text, 16) + i;
        }

        private void byte6txt_Validating(object sender, CancelEventArgs e)
        {
            RangeFromTB.Text = RangeFromTB.Text.Substring(0, 10) + Convert.ToInt32(byte6txt.Text).ToString("X2");
            for (int i = 0; i < MAXCONNECTIONS; i++)
                nodenumberused[i] = Convert.ToInt64(RangeFromTB.Text, 16) + i;
        }

        private void RangeFromTB_Validating(object sender, CancelEventArgs e)
        {
            RangeFromTB.Text = RangeFromTB.Text.PadLeft(12, '0');
            SetGroup(RangeFromTB.Text);
            byte6txt.Text = Convert.ToInt32(RangeFromTB.Text.Substring(10, 2), 16).ToString();
            membertxt.Text = Convert.ToInt32(RangeFromTB.Text.Substring(4, 6), 16).ToString();
            for (int i = 0; i < MAXCONNECTIONS; i++)
                nodenumberused[i] = Convert.ToInt64(RangeFromTB.Text, 16) + i;
        }
 
    }
}

 