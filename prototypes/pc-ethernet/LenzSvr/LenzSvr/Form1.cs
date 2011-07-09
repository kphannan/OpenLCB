using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Threading;
using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using System.IO;
using System.IO.Ports;
using Bonjour;

namespace LenzSvr
{
    public partial class Server : Form
    {

        const string INIT = "908F";
        const string XPRESSNET = "8050";
        const string VERIFYNODEIDS = "80AF";
        const string VERIFIEDNODEID = "90BF";

        // Bonjour
        private Bonjour.DNSSDService m_service = null;
        private Bonjour.DNSSDEventManager m_eventManager = null;
        private Bonjour.DNSSDService m_browser = null;
        private Bonjour.DNSSDService m_resolver = null;
        private Bonjour.DNSSDService m_registrar = null;
        static bool serverconnected = false;

        public class Loco
        {
            public int loco = 0;
            public char ls = 'L';
            public char direction = '1';
            public int speed = 0;
            public bool[] fnstate = new bool[29];
        }

        public class Connection
        {
            public bool open = false;
            public Socket skt = null;
            public string userid = "Unkown";
            public Loco t = new Loco();
            public Loco s = new Loco();
        }
        const int MAXCONNECTIONS = 10;
        static Connection[] connections = new Connection[MAXCONNECTIONS];
        static bool poweron = true;

        static Socket listenSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        static Socket skt = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        static DateTime time = DateTime.Now;
        static long nodenumber = 0;
        static byte[] inputbuffer = new byte[2000];
        string xml = "<cdi><id><Software>OpenLCB WiThrottle server to XpressNet</Software>"
            + "<Version>Mike Johnson 4 July 2011</Version></id></cdi>";

        public Server()
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

            // create the async listening sockets
            for (int i = 0; i < MAXCONNECTIONS; i++)
                connections[i] = new Connection();
            IPEndPoint ep = new IPEndPoint(IPAddress.Any, 0);
            listenSocket.Bind(ep);
            listenSocket.Listen(100);
            listenSocket.BeginAccept(new AsyncCallback(Acceptcallback), 0);
            ep = ((IPEndPoint)listenSocket.LocalEndPoint);
            m_registrar = m_service.Register(0, 0, System.Environment.UserName, "_withrottle._tcp", null, null, (ushort)ep.Port, null, null);
            log("WiThrottle Port " + ep.Port.ToString());
        }

        public void SendHexString(string s)
        {
            if (!serverconnected)
                return;
            log("> " + s);
            byte[] buffer = new byte[1 + s.Length / 2];
            buffer[0] = (byte)buffer.Length;
            int j = 1;
            for (int i = 0; i < s.Length; i += 2)
                buffer[j++] = (byte)Convert.ToByte(s.Substring(i, 2), 16);
            skt.Send(buffer);
        }
        
        static object loglock = new object();

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

        private void Server_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (m_registrar != null)
                m_registrar.Stop();
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
                MessageBox.Show("Node number server browse Failed", "Error");
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

            log("Node number service on " + hostName + ":" + port.ToString());

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
                skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
                log("OpenLCB service connected on " + hostName + ":" + port.ToString());
                serverconnected = true;
                SendHexString(INIT + nodenumber.ToString("X12"));
                SendHexString(XPRESSNET + nodenumber.ToString("X12") + "212405");
            }
            catch (Exception e)
            {
                log("Node number server Connection failed " + e.ToString());
            }          
        }

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
                connections[index].open = true;
                // log("Accept ...");
                connections[index].skt = listenSocket.EndAccept(result);
                // start a new accept
                bool ok = false;
                for (i = 0; i < MAXCONNECTIONS; i++)
                {
                    if (!connections[i].open)
                    {
                        ok = true;
                        listenSocket.BeginAccept(new AsyncCallback(Acceptcallback), i);
                    }
                }
                if (!ok)
                    log("No new accept started.");
                while (true)
                {
                    int size = connections[index].skt.Receive(buffer);
                    // log(Encoding.UTF8.GetString(buffer, 0, size));
                    if (size == 0) // lost connection ?
                        break;
                    int ptr = 0;
                    while (ptr < size) // multiple command in one packet
                    {
                        // log("loop " + ptr.ToString() + (Char)buffer[ptr] + buffer[ptr].ToString("X2"));
                        if (buffer[ptr] == 'Q' || buffer[ptr] == 'H') // ignore quit, hardware
                            break;
                        else if (buffer[ptr] == '*' || buffer[ptr] == '\r' || buffer[ptr] == '\n') // ignore heartbeat, cr, nl
                            ptr++;
                        else if (buffer[ptr] == 'N') // Name
                        {
                            connections[index].userid = Encoding.UTF8.GetString(buffer, 0, size - 1).Substring(1);
                            log(connections[index].userid + " connected.");
                            break;
                        }
                        else if (buffer[ptr] == 'T') // first Throttle command
                        {
                            String cmd = "";
                            while (ptr < size && buffer[ptr] != '\n')
                                cmd += (char)buffer[ptr++];
                            cmd += '\n';
                            Throttlecmd(connections[index].t, cmd, connections[index].skt, connections[index].userid);
                        }
                        else if (buffer[ptr] == 'S') // second Throttle command
                        {
                            String cmd = "";
                            while (ptr < size && buffer[ptr] != '\n')
                                cmd += (char)buffer[ptr++];
                            cmd += '\n';
                            Throttlecmd(connections[index].s, cmd, connections[index].skt, connections[index].userid);
                        }
                        else
                        {
                            log("Unkown packet type " + (Char)buffer[ptr] + ":" + buffer[ptr].ToString("X2"));
                            break;
                        }
                    } // end of inside packet loop
                } // end of while true
            }
            catch (Exception e)
            {
                log("Exception in accept " + e.ToString());
            }

            // Close connection
            try
            {
                connections[index].skt.Shutdown(SocketShutdown.Both);
                connections[index].skt.Close();
            }
            catch (SocketException)
            {
            }

            log(connections[index].userid + " disconnected.");
            connections[index].open = false;
        }

        // Handle a throttle command starting with S or T

        private void Throttlecmd(Loco loco, String buffer, Socket skt, String userid)
        {
            String reply;
            int i;
            int ptr = 0;
            // log(buffer);
            if (buffer[1] == 'L' || buffer[1] == 'S') // new address
            {
                reply = ""+buffer[0]; // T or S
                loco.ls = buffer[ptr + 1];
                loco.loco = 0;
                ptr += 2;
                while (buffer[ptr] >= '0' && buffer[ptr] <= '9')
                    loco.loco = loco.loco * 10 + buffer[ptr++] - '0';
                ptr++;
                reply += loco.loco.ToString() + "(" + loco.ls.ToString() + ")\n";
                // log(reply);
                skt.Send(Encoding.UTF8.GetBytes(reply));
                log(userid + ": Loco" + buffer[0] + " = " + loco.loco.ToString() + " Speed = " 
                    + loco.speed.ToString() + " Direction = " + loco.direction.ToString());

                // Initial function states
                if (buffer[0]=='T')
                    reply = "\r\nRPF}|{T";
                else
                    reply = "\r\nRPF}|{S";
                for (i = 0; i <= 28; i++)
                {
                    loco.fnstate[i] = false;
                    reply += "]\\[F" + i.ToString() + "}|{false";
                }
                reply += "\r\n\r\n";
                skt.Send(Encoding.UTF8.GetBytes(reply));
                // log(reply);

                // Function labels
                if (buffer[0] == 'T')
                    reply = "RF29";
                else
                    reply = "RS29";
                for (i = 0; i <= 28; i++)
                    reply += "]\\[F" + i.ToString();
                reply += "\r\n";
                skt.Send(Encoding.UTF8.GetBytes(reply));
            }
            else if (buffer[ptr + 1] == 'R') // new direction
            {
                loco.direction = (char)buffer[ptr + 2];
                ptr += 4;
                log(userid + ": Loco" + buffer[0] + " = " + loco.loco.ToString() + " Speed = "
                    + loco.speed.ToString() + " Direction = " + loco.direction.ToString());
                xnspeed(loco.loco, loco.speed, loco.direction);
            }
            else if (buffer[ptr + 1] == 'V') // new speed
            {
                loco.speed = 0;
                ptr += 2;
                while (buffer[ptr] >= '0' && buffer[ptr] <= '9')
                    loco.speed = loco.speed * 10 + buffer[ptr++] - '0';
                ptr++;
                log(userid + ": Loco" + buffer[0] + " = " + loco.loco.ToString() + " Speed = "
                    + loco.speed.ToString() + " Direction = " + loco.direction.ToString());
                if (loco.speed >= 1 && loco.speed < 126) // speed 1 is emergency stop
                    loco.speed++;
                xnspeed(loco.loco, loco.speed, loco.direction);
            }
            else if (buffer[ptr + 1] == 'I') // stop
            {
                ptr += 3;
                loco.speed = 0;
                log(userid + ": Loco" + buffer[0] + " = " + loco.loco.ToString() + " Speed = "
                    + loco.speed.ToString() + " Direction = " + loco.direction.ToString());
                xnspeed(loco.loco, loco.speed, loco.direction);
            }
            else if (buffer[ptr + 1] == 'X') // emergency stop
            {
                if (DateTime.Now > time.AddMilliseconds(200))
                {
                    loco.speed = 0;
                    log("Emergency stop button pushed.");
                    SendHexString(XPRESSNET + nodenumber.ToString("X12") + "8080");
                }
                ptr += 3;
                time = DateTime.Now;
            }
            else if (buffer[ptr + 1] == 'F') // Function
            {
                bool down = buffer[ptr + 2] == '1';
                ptr += 3;
                int fn = 0;
                while (buffer[ptr] >= '0' && buffer[ptr] <= '9')
                    fn = fn * 10 + buffer[ptr++] - '0';
                ptr++;
                if (!poweron)
                {
                    SendHexString(XPRESSNET + nodenumber.ToString("X12") +"2181A0"); // resume operation
                    return;
                } 
                if (down)
                {
                    loco.fnstate[fn] = !loco.fnstate[fn];
                    if (buffer[0] == 'T')
                        reply = "RPF}|{T]\\[F";
                    else
                        reply = "RPF}|{S]\\[F";
                    if (loco.fnstate[fn])
                        skt.Send(Encoding.UTF8.GetBytes(reply + fn.ToString() + "}|{true\r\n"));
                    else
                        skt.Send(Encoding.UTF8.GetBytes(reply + fn.ToString() + "}|{false\r\n"));
                    log(userid + ": Loco" + buffer[0] + " = " + loco.loco.ToString() + " Fn = " + fn.ToString()
                        + ((loco.fnstate[fn]) ? " on" : " off"));
                    xnfn(loco.loco, fn, loco.fnstate);
                }
            }
        }

        private void AllSocketsSend(string s)
        {
            for (int i = 0; i < MAXCONNECTIONS; i++)
            {
                if (connections[i].open)
                {
                    connections[i].skt.Send(Encoding.UTF8.GetBytes(s));
                }
            }
        }

        //*********************************************************************************

        private void InputTask(IAsyncResult ar)
        {
            Socket s = (Socket) ar.AsyncState;
            int read = s.EndReceive(ar);

            string inputstring = "";
            for (int i = 0; i < read; i++)
                inputstring += inputbuffer[i].ToString("X2");
            // log("< " + inputstring);
            while (inputstring.Length > 0)
            {
                int length = Convert.ToInt32(inputstring.Substring(0, 2),16);
                string cmd = inputstring.Substring(0, length * 2);
                if (inputstring.Length > length * 2)
                    inputstring = inputstring.Substring(length * 2);
                else
                    inputstring = "";
                log("E> " + cmd);
                if (cmd.Substring(2, 4) == VERIFYNODEIDS)
                    SendHexString(VERIFIEDNODEID + nodenumber.ToString("X12") + nodenumber.ToString("X12"));
                else if (cmd.Substring(2, 1) == "E" && cmd.Substring(18, 12) == nodenumber.ToString("X12"))
                { // datagram
                    if (cmd.Substring(2, 4) == "E200" && cmd.Substring(30, 2) == "60" && cmd.Substring(40, 2) == "FF")
                    {
                        // send XML file
                        string address = cmd.Substring(32, 8);
                        int ad = Convert.ToInt32(address,16);
                        string data = "";
                        int l = Convert.ToInt32(cmd.Substring(42, 2),16);
                        if (ad + l > xml.Length)
                            l = xml.Length - ad;
                        for (int i = 0; i < l; i++)
                            data += ((int)xml[ad + i]).ToString("X2");
                        SendHexString("E200" + nodenumber.ToString("X12") + cmd.Substring(6, 12) + "30" + address + "FF" + data);
                    }
                    else if (cmd.Substring(2, 4) == "E0A0")
                        SendHexString(VERIFIEDNODEID + nodenumber.ToString("X12") + nodenumber.ToString("X12"));
                }
                else if (cmd.Substring(2, 4) == XPRESSNET)
                {
                    string xp = cmd.Substring(5);
                    if (xp == "610160" && poweron == false)
                    {
                        poweron = true;
                        AllSocketsSend("PPA1\n");
                        TrackBtn.Text = "Track On";
                        log("Track on.");
                    }
                    else if ((xp == "610061" || xp == "810081") && poweron == true)
                    {
                        poweron = false;
                        AllSocketsSend("PPA0\n");
                        TrackBtn.Text = "Track Off";
                        log("Track off.");
                    }
                    else if (xp == "62220040") // command station state ok
                    {
                        poweron = true;
                        AllSocketsSend("PPA1\n");
                        TrackBtn.Text = "Track OK";
                    }
                    else if (xp == "E40480000060" || xp == "E404000000E0") // loco information
                    {
                        // allocated = true;
                    }
                    log("< " + cmd);
                    TrackBtn.Update();
                }
            }
            skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
        }

        private void xnspeed(int loco, int speed, char dir)
        {
            if (dir == '1')
                speed |= 0x80;
            else
                speed &= 0x7F;

            if (loco>=100)
                loco |= 0xC000;

            if (!poweron)
                SendHexString(XPRESSNET + nodenumber.ToString("X12") + "2181A0"); // resume operation
            else
                SendHexString(XPRESSNET + nodenumber.ToString("X12") + "E413" + loco.ToString("X4") + speed.ToString("X2") + "00");
        }


        private void xnfn(int loco, int fn, bool[]fns)
        {
            if (loco >= 100)
                loco |= 0xC000;
            string l = loco.ToString("X4");
            int f = 0;
            int b = 0x01;
            if (fn >= 0 && fn <= 4)
            {
                if (fns[0])
                    f |= 0x10;
                for (int i = 1; i <= 4; i++, b <<= 1)
                    if (fns[i])
                        f |= b;
                SendHexString(XPRESSNET + nodenumber.ToString("X12") + "E420" + l + f.ToString("X2"));
            }
            else if (fn >= 5 && fn <= 8)
            {
                for (int i = 5; i <= 8; i++, b <<= 1)
                    if (fns[i])
                        f |= b;
                SendHexString(XPRESSNET + nodenumber.ToString("X12") + "E421" + l + f.ToString("X2"));
            }
            else if (fn >= 9 && fn <= 12)
            {
                for (int i = 9; i <= 12; i++, b <<= 1)
                    if (fns[i])
                        f |= b;
                SendHexString(XPRESSNET + nodenumber.ToString("X12") + "E422" + l + f.ToString("X2"));
            }
            else if (fn >= 13 && fn <= 20)
            {
                for (int i = 13; i <= 20; i++, b <<= 1)
                    if (fns[i])
                        f |= b;
                SendHexString(XPRESSNET + nodenumber.ToString("X12") + "E423" + l + f.ToString("X2"));
            }
            else if (fn >= 21 && fn <= 28)
            {
                for (int i = 21; i <= 28; i++, b <<= 1)
                   if (fns[i])
                        f |= b;
               SendHexString(XPRESSNET + nodenumber.ToString("X12") + "E428" + l + f.ToString("X2"));
            }
        }

        private void TrackBtn_Click(object sender, EventArgs e)
        {
            if (poweron)
                SendHexString(XPRESSNET + nodenumber.ToString("X12") + "2180A1");
            else
                SendHexString(XPRESSNET + nodenumber.ToString("X12") + "2181A0");
        }

        private void Server_Load(object sender, EventArgs e)
        {

        }

    }
}
