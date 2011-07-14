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
using System.Xml;
using Bonjour;

namespace Config
{
    public partial class Config : Form
    {
        const string INIT = "908F";
        const string VERIFYNODEIDS = "80AF";
        const string VERIFIEDNODEID = "90BF";

        // Bonjour
        private Bonjour.DNSSDService m_service = null;
        private Bonjour.DNSSDEventManager m_eventManager = null;
        private Bonjour.DNSSDService m_browser = null;
        private Bonjour.DNSSDService m_resolver = null;
        
        static long nodenumber = 0;
        static byte[] inputbuffer = new byte[2000];
        static Socket skt = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        static object loglock = new object();
        static bool serverconnected = false;

        static Queue<string> datagrams = new Queue<string>();

        static Label[] idlabels = new Label[4];
        static TextBox[] idtextboxes = new TextBox[4];
        static Label[] labels = new Label[10];
        static TextBox[] textboxes = new TextBox[10];
        static NumericUpDown[] numbers = new NumericUpDown[10];
        static int[] startofdata = new int[10];
        static int[] lengthofdata = new int[10];
        static string[] typeofdata = new string[10];

        static Thread background;
        static Semaphore taskcomplete = new Semaphore(0, 1);

        public Config()
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

            for (int i = 0; i < 4; i++)
            {
                idlabels[i] = new Label();
                idlabels[i].AutoSize = true;
                idlabels[i].Size = new System.Drawing.Size(50, 13);
                idlabels[i].Location = new System.Drawing.Point(10, i * 25 + 50);
                idlabels[i].Visible = false;
                Controls.Add(idlabels[i]);
                idtextboxes[i] = new TextBox();
                idtextboxes[i].Location = new System.Drawing.Point(90, i * 25 + 50);
                idtextboxes[i].Size = new System.Drawing.Size(375, 20);
                idtextboxes[i].ReadOnly = true;
                idtextboxes[i].Visible = false;
                Controls.Add(idtextboxes[i]);
            }
            for (int i = 0; i < 10; i++)
            {
                labels[i] = new Label();
                labels[i].AutoSize = true;
                labels[i].Visible = false;
                groupBox1.Controls.Add(labels[i]);
                textboxes[i] = new TextBox();
                textboxes[i].Size = new System.Drawing.Size(160, 20);
                textboxes[i].Validating += new System.ComponentModel.CancelEventHandler(this.textBox_Validating);
                textboxes[i].Visible = false;
                groupBox1.Controls.Add(textboxes[i]);
                numbers[i] = new NumericUpDown();
                numbers[i].Visible = false;
                numbers[i].ValueChanged += new System.EventHandler(this.numericUpDown_ValueChanged);
                groupBox1.Controls.Add(numbers[i]);
            }
            setBtns(0);

            StartGetNodeNumber();
        }

        public void SendHexString(string s)
        {
            if (!serverconnected)
                return;
            byte[] buffer = new byte[1 + s.Length / 2];
            buffer[0] = (byte)buffer.Length;
            log(">" + buffer[0].ToString("X2") + s);
            int j = 1;
            for (int i = 0; i < s.Length; i+=2)
                buffer[j++] = (byte)Convert.ToByte(s.Substring(i,2),16);
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
                skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
                serverconnected = true;
                SendHexString(INIT + nodenumber.ToString("X12") + nodenumber.ToString("X12"));
                SelectNodeCB.Items.Clear();
                SendHexString(VERIFYNODEIDS + nodenumber.ToString("X12"));
            }
            catch (Exception e)
            {
                log("OpenLCB server Connection failed " + e.ToString());
            }

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
                int length = Convert.ToInt32(inputstring.Substring(0, 2),16);
                string cmd = inputstring.Substring(0, length * 2);
                if (inputstring.Length > length * 2)
                    inputstring = inputstring.Substring(length * 2);
                else
                    inputstring = "";
                log("< " + cmd);
                if (cmd.Substring(2, 4) == VERIFIEDNODEID || cmd.Substring(2, 4) == INIT)
                {
                    if (!SelectNodeCB.Items.Contains(cmd.Substring(6, 12)))
                        SelectNodeCB.Items.Add(cmd.Substring(6, 12));
                }
                else if (cmd.Substring(2, 1) == "E" && cmd.Substring(18,12)==nodenumber.ToString("X12")) // datagram
                {
                    datagrams.Enqueue(cmd);
                }
             }
            skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
        }

        private void NodeIdsBtn_Click(object sender, EventArgs e)
        {
            SelectNodeCB.Items.Clear();
            SendHexString(VERIFYNODEIDS + nodenumber.ToString("X12"));
        }

        //******************************************************************************************************
        // XML Node information
        //******************************************************************************************************

        public XmlDocument xmld = new XmlDocument();
        public XmlNode SegmentXML = null;
        public string SegmentName = "";
        public int SegmentSpace = 0;
        public long SegmentOrg = 0;
        public int SegmentSize = 0;
        public int SegmentBtns = 3;

        public string SegmentData;
        public int bitpos;
        public int bytepos;

        public int bitsize = 0;

        //******************************************************************************************************
        // Node selection, Read XML
        //******************************************************************************************************

        private void SelectNodeCB_SelectedIndexChanged(object sender, EventArgs e)
        {
            for (int i = 0; i < 4; i++)
            {
                idlabels[i].Visible = false;
                idtextboxes[i].Visible = false;
            }
            SegmentsTB.Items.Clear();
            SegmentsTB.Text = "";
            // read XML from node
            background = new Thread(ChangeNodeSelected);
            background.Start();
            taskcomplete.WaitOne();
            DisplayNode();
        }

        public void ChangeNodeSelected()
        {
            string datagram = "";
            string x = "";
            int adr = 0;
            bool more = true;
            while (more)
            {
                SendHexString("E200" + nodenumber.ToString("X12") + SelectNodeCB.Text 
                    + "60" + adr.ToString("X8") + "FF" + "40");
                for (int w = 0; w < 500; w++)
                {
                    Thread.Sleep(10);
                    if (datagrams.Count != 0)
                    {
                        datagram = datagrams.Dequeue();
                        if (datagram.Substring(2, 4) == "E200")
                            break;
                    }
                }
                if (datagram.Length < 40)
                    break;
                for (int i = 42; i < datagram.Length; i += 2)
                {
                    int c = Convert.ToInt32(datagram.Substring(i, 2), 16);
                    if (c == 0)
                    {
                        more = false;
                        break;
                    }
                    x += (char)c;
                }
                adr += 64;
            } 
            log(x);
            if (!x.StartsWith("<cdi>")) {
                log("XML files does not start with <cdi>");
                return;
            }
            try
            {
                xmld.LoadXml(x);
            }
            catch (Exception e)
            {
                log("Xml file corrupt " + e.ToString());
            }
            taskcomplete.Release();
        }

        public void DisplayNode()
        {
            int i;
            XmlNode maintag = xmld.FirstChild.FirstChild;
            while (maintag != null)
            {
                if ("identification".StartsWith(maintag.Name))
                {
                    XmlNode fc = maintag.FirstChild;
                    for (i = 0; i < 4; i++)
                    {
                        if (fc == null)
                            break;
                        idlabels[i].Text = fc.Name;
                        idlabels[i].Visible = true;
                        idtextboxes[i].Text = fc.InnerText;
                        idtextboxes[i].Visible = true;
                        fc = fc.NextSibling;
                    }
                }
                else if ("segment".StartsWith(maintag.Name))
                {
                    for (i = 0; i < maintag.Attributes.Count; i++)
                    {
                        if ("name".StartsWith(maintag.Attributes[i].Name))
                        {
                            SegmentsTB.Items.Add(maintag.Attributes.GetNamedItem("name").InnerText);
                            break;
                        }
                    }
                    if (SegmentsTB.Items.Count == 1)
                         SegmentsTB.SelectedIndex = 0;
                }
                else
                    log("Unknown tag in XML " + maintag.Name);
                maintag = maintag.NextSibling;
            }
            SegmentChanged();
            groupBox1.Refresh();
        }

        public long GetNumber(string s)
        {
            if (s[0] == '#')
                return Convert.ToInt64(s.Substring(1), 16);
            else
                return Convert.ToInt64(s);
        }

        private void SegmentsTB_SelectedIndexChanged(object sender, EventArgs e)
        {
            SegmentChanged();
        }

        public void SegmentChanged()
        {
            for (int i = 0; i < 10; i++)
            {
                labels[i].Visible = false;
                textboxes[i].Visible = false;
                numbers[i].Visible = false;
            }
            SegmentName = SegmentsTB.Text;
            int btns = 0xFFFF;
            bool wrongname = true;

            SegmentXML = xmld.FirstChild.FirstChild;
            while(SegmentXML!=null)
            {
                SegmentSpace = 0;
                SegmentOrg = 0;
                btns = 0xFFFF;
                if ("identification".StartsWith(SegmentXML.Name))
                { }
                else if ("segment".StartsWith(SegmentXML.Name))
                {
                    for (int i = 0; i < SegmentXML.Attributes.Count; i++)
                    {
                        if ("space".StartsWith(SegmentXML.Attributes[i].Name))
                            SegmentSpace = (int)GetNumber(SegmentXML.Attributes[i].InnerText);
                        else if ("origin".StartsWith(SegmentXML.Attributes[i].Name))
                            SegmentOrg = GetNumber(SegmentXML.Attributes[i].InnerText);
                        else if ("buttons".StartsWith(SegmentXML.Attributes[i].Name))
                            btns = (int)GetNumber(SegmentXML.Attributes[i].InnerText);
                        else if ("name".StartsWith(SegmentXML.Attributes[i].Name)) 
                        {
                            if (SegmentName == SegmentXML.Attributes[i].InnerText)
                                wrongname = false;
                            else
                                break;
                        }
                        else
                            log("Segment, unknown attribute " + SegmentXML.Attributes[i].Name);
                    }
                    if (!wrongname)
                    {
                        SegmentSize = sizesegment(SegmentXML);
                        XmlNode fc = SegmentXML.FirstChild;
                        int index = 0;
                        int indent = 0;
                        while (fc != null)
                        {
                            if ("group".StartsWith(fc.Name))
                                index = group(index, indent, fc);
                            else if ("integer".StartsWith(fc.Name) || "character".StartsWith(fc.Name)
                              || "byte".StartsWith(fc.Name) || "bit".StartsWith(fc.Name))
                                index = var(index, indent, fc);
                            else
                                log(fc.Name);
                            fc = fc.NextSibling;
                        }
                        setBtns(btns);
                        break;
                    }
                }
                SegmentXML = SegmentXML.NextSibling;
            }
            groupBox1.Refresh();
        }

        public int group(int index, int indent, XmlNode n)
        {
            int rep = 1;
            string name = "Group";

            for (int i = 0; i < n.Attributes.Count; i++)
            {
                if ("replication".StartsWith(n.Attributes[i].Name))
                    rep = (int)GetNumber(n.Attributes[i].InnerText);
                else if ("name".StartsWith(n.Attributes[i].Name))
                    name = n.Attributes[i].InnerText;
                else
                    log("Group, unknown attribute " + n.Attributes[i].Name);
            }
            labels[index].Text = name + " (Index 0 - " + (rep-1).ToString() + ") :";
            labels[index].Location = new System.Drawing.Point(indent * 10 + 10, index * 25 + 15);
            labels[index].Visible = true;
            numbers[index].Text = "0";
            numbers[index].Location = new System.Drawing.Point(indent * 10 + 150, index * 25 + 15);
            numbers[index].Maximum = rep - 1;
            numbers[index].Visible = true;
            index++;
            indent++;
            XmlNode fc = n.FirstChild;
            while (fc != null)
            {
                if ("group".StartsWith(fc.Name))
                    index = group(index, indent, fc);
                else if ("integer".StartsWith(fc.Name) || "character".StartsWith(fc.Name)
                              || "byte".StartsWith(fc.Name) || "bit".StartsWith(fc.Name))
                    index = var(index, indent, fc);
                else
                    log(fc.Name);
                fc = fc.NextSibling;
            }
            return index;
        }

        public int var(int index, int indent, XmlNode n)
        {
            int isize = 1;
            string name = "unused";
            for (int i = 0; i < n.Attributes.Count; i++)
            {
                if ("size".StartsWith(n.Attributes[i].Name))
                    isize = (int)GetNumber(n.Attributes[i].InnerText);
                else if ("name".StartsWith(n.Attributes[i].Name))
                    name = n.Attributes[i].InnerText;
                else
                    log("Var, unknown attribute " + n.Attributes[i].Name);
            }
            if (name == "unused")
                return index;

            if ("integer".StartsWith(n.Name))
                typeofdata[index] = "int";
            else if ("character".StartsWith(n.Name))
                typeofdata[index] = "char";
            else if ("byte".StartsWith(n.Name))
                typeofdata[index] = "byte";
            else if ("bit".StartsWith(n.Name))
                typeofdata[index] = "bit";

            labels[index].Text = name + " (" + typeofdata[index] + isize.ToString() + ") :";
            labels[index].Location = new System.Drawing.Point(indent * 10 + 10, index * 25 + 15);
            labels[index].Visible = true;
            textboxes[index].Text = "";
            textboxes[index].Location = new System.Drawing.Point(indent * 10 + 150, index * 25 + 15);
            textboxes[index].Visible = true;
            return index + 1;
        }

        public void setBtns(int mask)
        {
            int pos = 19;
            ReadBtn.Visible = (mask & 0x0001) == 0x0001;
            if (ReadBtn.Visible)
            {
                ReadBtn.Location = new System.Drawing.Point(371, pos);
                pos += 29;
            }
            WriteBtn.Visible = (mask & 0x0002) == 0x0002;
            if (WriteBtn.Visible)
            {
                WriteBtn.Location = new System.Drawing.Point(371, pos);
                pos += 29;
            }
            ReadAllBtn.Visible = (mask & 0x0004) == 0x0004;
            if (ReadAllBtn.Visible)
            {
                ReadAllBtn.Location = new System.Drawing.Point(371, pos);
                pos += 29;
            }
            WriteAllBtn.Visible = (mask & 0x0008) == 0x0008;
            if (WriteAllBtn.Visible)
            {
                WriteAllBtn.Location = new System.Drawing.Point(371, pos);
                pos += 29;
            }
            DeleteBtn.Visible = (mask & 0x0010) == 0x0010;
            if (DeleteBtn.Visible)
            {
                DeleteBtn.Location = new System.Drawing.Point(371, pos);
                pos += 29;
            }
            DefaultBtn.Visible = (mask & 0x0020) == 0x0020;
            if (DefaultBtn.Visible)
            {
                DefaultBtn.Location = new System.Drawing.Point(371, pos);
                pos += 29;
            }
            RebootBtn.Visible = (mask & 0x0040) == 0x0040;
            if (RebootBtn.Visible)
            {
                RebootBtn.Location = new System.Drawing.Point(371, pos);
                pos += 29;
            }
        }

        //******************************************************************************************************
        // Size of segment, group or variable
        //******************************************************************************************************

        private int sizesegment(XmlNode n)
        {
            int size = 0;
            XmlNode fc = n.FirstChild;
            while (fc != null)
            {
                if ("group".StartsWith(fc.Name))
                    size += sizegroup(fc);
                else if ("integer".StartsWith(fc.Name) || "character".StartsWith(fc.Name)
                    || "byte".StartsWith(fc.Name) || "bit".StartsWith(fc.Name))
                    size += sizevar(fc);
                else
                    log(fc.Name);
                fc = fc.NextSibling;
            }
            return size;
        }

        public int sizegroup(XmlNode n)
        {
            int rep = 1;
            string name = "Group";
            int size = 0;
            for (int i = 0; i < n.Attributes.Count; i++)
            {
                if ("replication".StartsWith(n.Attributes[i].Name))
                    rep = (int)GetNumber(n.Attributes[i].InnerText);
                else if ("name".StartsWith(n.Attributes[i].Name))
                    name = n.Attributes[i].InnerText;
                else
                    log("Group, unknown attribute " + n.Attributes[i].Name);
            }
            XmlNode fc = n.FirstChild;
            while (fc != null)
            {
                if ("group".StartsWith(fc.Name))
                    size += sizegroup(fc);
                else if ("integer".StartsWith(fc.Name) || "character".StartsWith(fc.Name)
                  || "byte".StartsWith(fc.Name) || "bit".StartsWith(fc.Name))
                    size += sizevar(fc);
                else
                    log(fc.Name);
                fc = fc.NextSibling;
            }
            return size*rep;
        }

        public int sizevar(XmlNode n)
        {
            int isize = 1;
            string name = "unused";
            for (int i = 0; i < n.Attributes.Count; i++)
            {
                if ("size".StartsWith(n.Attributes[i].Name))
                    isize = (int)GetNumber(n.Attributes[i].InnerText);
                else if ("name".StartsWith(n.Attributes[i].Name))
                    name = n.Attributes[i].InnerText;
                else
                    log("Var, unknown attribute " + n.Attributes[i].Name);
            }

            if ("bit".StartsWith(n.Name))
            {
                bitsize += isize;
                isize = bitsize/8;
                bitsize = bitsize - isize*8;
            }
            else if (bitsize != 0)
            {
                isize++;
                bitsize = 0;
            }
            return isize;
        }

        //******************************************************************************************************
        // Read from node
        //******************************************************************************************************

        private void ReadBtn_Click(object sender, EventArgs e)
        {
            background = new Thread(ReadData);
            background.Start();
        }

        public void ReadData()
        {
            // read data
            string datagram = "";
            SegmentData = "";
            long adr = SegmentOrg;
            int left = SegmentSize;
            while (adr < SegmentOrg + SegmentSize)
            {
                int l = left;
                if (l > 64)
                    l = 64;
                SendHexString("E200" + nodenumber.ToString("X12") + SelectNodeCB.Text
                    + "60" + adr.ToString("X8") + SegmentSpace.ToString("X2") + l.ToString("X2"));
                for (int w = 0; w < 200; w++)
                {
                    Thread.Sleep(10);
                    if (datagrams.Count != 0)
                    {
                        datagram = datagrams.Dequeue();
                        if (datagram.Substring(2, 4) == "E200")
                            break;
                    }
                }
                if (datagram.Length < 42)
                {
                    log("Failed to read data from " + SelectNodeCB.Text);
                    return;
                }
                SegmentData += datagram.Substring(42);
                adr += 64;
                left -= 64;
            }
            DisplayData();
        }

        public void DisplayData()
        {
            bytepos = 0;
            bitpos = 0;
            // display data
            XmlNode fc = SegmentXML.FirstChild;
            int index = 0;
            while (fc != null)
            {
                if ("group".StartsWith(fc.Name))
                    index = DisplayGroup(index, fc);
                else if ("integer".StartsWith(fc.Name) || "character".StartsWith(fc.Name)
                    || "byte".StartsWith(fc.Name) || "bit".StartsWith(fc.Name))
                    index = DisplayVar(index, fc);
                else
                    log(fc.Name);
                fc = fc.NextSibling;
            }
            groupBox1.Refresh();
        }

        public int DisplayGroup(int index, XmlNode n)
        {
            int rep = 1;
            string name = "Group";

            for (int i = 0; i < n.Attributes.Count; i++)
            {
                if ("replication".StartsWith(n.Attributes[i].Name))
                    rep = (int)GetNumber(n.Attributes[i].InnerText);
                else if ("name".StartsWith(n.Attributes[i].Name))
                    name = n.Attributes[i].InnerText;
                else
                    log("Group, unknown attribute " + n.Attributes[i].Name);
            }
            int recsize = sizegroup(n) / rep;
            bytepos += Convert.ToInt32(numbers[index].Text) * recsize;
            index++;
            XmlNode fc = n.FirstChild;
            while (fc != null)
            {
                if ("group".StartsWith(fc.Name))
                    index = DisplayGroup(index, fc);
                else if ("integer".StartsWith(fc.Name) || "character".StartsWith(fc.Name)
                  || "byte".StartsWith(fc.Name) || "bit".StartsWith(fc.Name))
                    index = DisplayVar(index, fc);
                else
                    log(fc.Name);
                fc = fc.NextSibling;
            }
            return index;
        }

        public int DisplayVar(int index, XmlNode n)
        {
            int isize = 1;
            string name = "unused";
            for (int i = 0; i < n.Attributes.Count; i++)
            {
                if ("size".StartsWith(n.Attributes[i].Name))
                    isize = (int)GetNumber(n.Attributes[i].InnerText);
                else if ("name".StartsWith(n.Attributes[i].Name))
                    name = n.Attributes[i].InnerText;
                else
                    log("Var, unknown attribute " + n.Attributes[i].Name);
            }

            startofdata[index] = bytepos;
            lengthofdata[index] = isize * 2;
            if ("integer".StartsWith(n.Name))
            {
                string t = "";
                for (int i = 0; i < isize * 2; i += 2)
                    t = SegmentData.Substring(bytepos+i, 2) + t;
                bytepos += isize * 2;
                if (name != "unused") {
                    textboxes[index++].Text = Convert.ToInt64(t, 16).ToString();
                }
            }
            else if ("character".StartsWith(n.Name))
            {
                string t = "";
                for (int i = 0; i < isize * 2; i += 2)
                {
                    int x = Convert.ToInt16(SegmentData.Substring(bytepos+i, 2), 16);
                    if (x == 0)
                        break;
                    t += (char)x;
                }
                bytepos += isize * 2;
                if (name != "unused")
                {
                    textboxes[index++].Text = t;
                }
            }
            else if ("byte".StartsWith(n.Name))
            {
                string t = "";
                for (int i = 0; i < isize * 2; i += 2)
                    t = SegmentData.Substring(bytepos + i, 2) + t;
                bytepos += isize * 2;
                if (name != "unused")
                {
                    textboxes[index++].Text = t;
                }
            }
            else if ("bit".StartsWith(n.Name))
            {
                int i = Convert.ToInt16(SegmentData.Substring(bytepos, 2), 16);
                // extract bits

                bitpos += isize;
                if (bitpos == 8)
                {
                    bytepos++;
                    bitpos = 0;
                }
                if (name != "unused")
                {
                    textboxes[index++].Text = i.ToString();
                }
            }
            return index;
        }

        //******************************************************************************************************
        // Write to node
        //******************************************************************************************************

        private void WriteBtn_Click(object sender, EventArgs e)
        {
            background = new Thread(WriteData);
            background.Start();
        }

        public void WriteData()
        {
            string datagram ="";
            long adr = SegmentOrg;
            int left = SegmentSize;
            while (adr < SegmentOrg + SegmentSize)
            {
               int l = left;
                if (l > 64)
                    l = 64;
                SendHexString("E200" + nodenumber.ToString("X12") + SelectNodeCB.Text
                    + "20" + adr.ToString("X8") + SegmentSpace.ToString("X2")
                    + SegmentData.Substring((int)(adr - SegmentOrg) * 2, l * 2));
                for (int w = 0; w < 200; w++)
                {
                    Thread.Sleep(10);
                    if (datagrams.Count != 0)
                    {
                        datagram = datagrams.Dequeue();
                        if (datagram.Substring(2, 4) == "E4C0" || datagram.Substring(2, 4) == "E4D0")
                            break;
                    }
                }
                if (datagram.Length<30 || datagram.Substring(2, 4) != "E4C0")
                {
                    log("Failed to write data to " + SelectNodeCB.Text);
                    return;
                }
                adr += 64;
                left -= 64;
            }
        }

        private void numericUpDown_ValueChanged(object sender, EventArgs e)
        {
            DisplayData();
        }

        private void textBox_Validating(object sender, CancelEventArgs e)
        {
            TextBox tb = (TextBox)sender;
            int index = (tb.Location.Y - 15) / 25;
            if (typeofdata[index] == "byte")
            {
                string t = tb.Text.PadLeft(lengthofdata[index],'0');
                string r = "";
                for (int i = 0; i < lengthofdata[index]; i += 2)
                    r = t.Substring(i, 2) + r;
                SegmentData = SegmentData.Substring(0, startofdata[index]) + r
                    + SegmentData.Substring(startofdata[index] + lengthofdata[index]);
            }
            else if (typeofdata[index] == "char")
            {
                string t = tb.Text.PadRight(lengthofdata[index], '\0');
                string r = "";
                for (int i = 0; i < lengthofdata[index]/2; i++)
                    r += ((int)t[i]).ToString("X2");
                SegmentData = SegmentData.Substring(0, startofdata[index]) + r
                    + SegmentData.Substring(startofdata[index] + lengthofdata[index]);
            }
            else if (typeofdata[index] == "int")
            {
                string t = Convert.ToInt64(tb.Text).ToString("X" + lengthofdata[index].ToString());
                string r = "";
                for (int i = 0; i < lengthofdata[index]; i += 2)
                    r = SegmentData.Substring(i, 2) + r;
                SegmentData = SegmentData.Substring(0, startofdata[index]) + r 
                    + SegmentData.Substring(startofdata[index] + lengthofdata[index]);
            }
            else if (typeofdata[index] == "bit")
            {
                string t = tb.Text;
            }
        }

        private void RebootBtn_Click(object sender, EventArgs e)
        {
            SendHexString("E200" + nodenumber.ToString("X12") + SelectNodeCB.Text + "A5");
        }

        private void DefaultBtn_Click(object sender, EventArgs e)
        {
            SendHexString("E200" + nodenumber.ToString("X12") + SelectNodeCB.Text + "A6");
        }
    
    }
}
