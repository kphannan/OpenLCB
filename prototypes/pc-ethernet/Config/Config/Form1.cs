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

        static Semaphore dg = new Semaphore(0, 1);
        static string datagram = "";
        static int dgsize = 0;
        static XmlDocument xmld = new XmlDocument();
        static XmlNode xmln = null;
        static Label[] idlabels = new Label[4];
        static TextBox[] idtextboxes = new TextBox[4];
        static Label[] labels = new Label[10];
        static TextBox[] textboxes = new TextBox[10];
        static NumericUpDown[] numbers = new NumericUpDown[10];

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
                idlabels[i].Hide();
                Controls.Add(idlabels[i]);
                idtextboxes[i] = new TextBox();
                idtextboxes[i].Location = new System.Drawing.Point(90, i * 25 + 50);
                idtextboxes[i].Size = new System.Drawing.Size(375, 20);
                idtextboxes[i].Hide();
                Controls.Add(idtextboxes[i]);
            }
            for (int i = 0; i < 10; i++)
            {
                labels[i] = new Label();
                labels[i].AutoSize = true;
                labels[i].Hide();
                groupBox1.Controls.Add(labels[i]);
                textboxes[i] = new TextBox();
                textboxes[i].Hide();
                groupBox1.Controls.Add(textboxes[i]);
                numbers[i] = new NumericUpDown();
                numbers[i].Hide();
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
                    datagram = cmd.Substring(30);
                    dgsize = datagram.Length;
                    dg.Release(1);
                }
             }
            skt.BeginReceive(inputbuffer, 0, 2000, SocketFlags.None, (AsyncCallback)InputTask, skt);
        }

        private void NodeIdsBtn_Click(object sender, EventArgs e)
        {
            SelectNodeCB.Items.Clear();
            SendHexString(VERIFYNODEIDS + nodenumber.ToString("X12"));
        }
        
        // Node selection, Read XML

        private void SelectNodeCB_SelectedIndexChanged(object sender, EventArgs e)
        {
            int i;
            for (i = 0; i < 4; i++)
            {
                idlabels[i].Hide();
                idtextboxes[i].Hide();
            }
            // read XML from node
            SegmentsTB.Items.Clear();
            SegmentsTB.Text = "";
            ErasePage(xmln);
            string data = "";
            int adr = 0;
            do
            {
                dgsize = -1;
                SendHexString("E200" + nodenumber.ToString("X12") + SelectNodeCB.Text 
                    + "60" + adr.ToString("X8") + "FF" + "40");
                dg.WaitOne(2000);
                if (dgsize < 10)
                {
                    log("Failed to read XML from " + SelectNodeCB.Text);
                    return;
                }
                if (dgsize <= 12)
                    break;
                data += datagram.Substring(12);
                adr += 64;
            } while (dgsize == 128+12);
            string x = "";
            for (i = 0; i < data.Length; i+=2)
                x += (char)Convert.ToInt32(data.Substring(i, 2),16);
            log(x);
            xmld.LoadXml(x);
            if (xmld.FirstChild.Name != "cdi")
            {
                log("XML files does not start with <cdi>");
                return;
            }
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
                        idlabels[i].Show();
                        idtextboxes[i].Text = fc.InnerText;
                        idtextboxes[i].Show();
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
        }

        public void ErasePage(XmlNode xmln)
        {
            if (xmln != null) 
                xmln = null;
            for (int i = 0; i < 10; i++)
            {
                labels[i].Hide();
                textboxes[i].Hide();
                numbers[i].Hide();
            }
            setBtns(0);
            groupBox1.Invalidate(true);
        }

        private void SegmentsTB_SelectedIndexChanged(object sender, EventArgs e)
        {
            ErasePage(xmln);
            string name = SegmentsTB.Text;
            int space = 0;
            long org = 0;
            int size = 0x10000;
            int btns = 3;
            bool wrongname = true;

            xmln = xmld.FirstChild.FirstChild;
            while(xmln!=null)
            {
                space = 0;
                org = 0;
                size = 0x10000;
                btns = 3;
                if ("identification".StartsWith(xmln.Name))
                { }
                else if ("segment".StartsWith(xmln.Name))
                {
                    for (int i = 0; i < xmln.Attributes.Count; i++)
                    {
                        if ("space".StartsWith(xmln.Attributes[i].Name))
                            space = Convert.ToInt32(xmln.Attributes[i].InnerText);
                        else if ("origin".StartsWith(xmln.Attributes[i].Name))
                            org = Convert.ToInt32(xmln.Attributes[i].InnerText,16);
                        else if ("size".StartsWith(xmln.Attributes[i].Name))
                            size = Convert.ToInt32(xmln.Attributes[i].InnerText);
                        else if ("buttons".StartsWith(xmln.Attributes[i].Name))
                            btns = Convert.ToInt32(xmln.Attributes[i].InnerText);
                        else if ("name".StartsWith(xmln.Attributes[i].Name)) 
                        {
                            if (name == xmln.Attributes[i].InnerText)
                                wrongname = false;
                            else
                                break;
                        }
                        else
                            log("Segment, unknown attribute " + xmln.Attributes[i].Name);
                    }
                    if (!wrongname)
                    {
                        XmlNode fc = xmln.FirstChild;
                        int index = 0;
                        int indent = 0;
                        while (fc != null)
                        {
                            if ("group".StartsWith(fc.Name))
                                index = group(index, indent, fc);
                            else if ("integer".StartsWith(fc.Name) || "character".StartsWith(fc.Name) || "bit".StartsWith(fc.Name))
                                index = var(index, indent, fc);
                            else
                                log(fc.Name);
                            fc = fc.NextSibling;
                        }
                        setBtns(btns);
                        break;
                    }
                }
                xmln = xmln.NextSibling;
            }
            groupBox1.Invalidate(true);
        }

        public int group(int index, int indent, XmlNode n)
        {
            int rep = 1;
            string name = "Group";

            for (int i = 0; i < n.Attributes.Count; i++)
            {
                if ("replication".StartsWith(n.Attributes[i].Name))
                    rep = Convert.ToInt16(n.Attributes[i].InnerText);
                else if ("name".StartsWith(n.Attributes[i].Name))
                    name = n.Attributes[i].InnerText;
                else
                    log("Group, unknown attribute " + n.Attributes[i].Name);
            }
            labels[index].Text = name + " (Index 0 - " + (rep-1).ToString() + ") :";
            labels[index].Location = new System.Drawing.Point(indent * 10 + 10, index * 25 + 15);
            labels[index].Show();
            numbers[index].Text = "0";
            numbers[index].Location = new System.Drawing.Point(indent * 10 + 150, index * 25 + 15);
            numbers[index].Maximum = rep - 1;
            numbers[index].Show();
            index++;
            indent++;
            XmlNode fc = n.FirstChild;
            while (fc != null)
            {
                if ("group".StartsWith(fc.Name))
                    index = group(index, indent, fc);
                else if ("integer".StartsWith(fc.Name) || "character".StartsWith(fc.Name) || "bit".StartsWith(fc.Name))
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
                    isize = Convert.ToInt16(n.Attributes[i].InnerText);
                else if ("name".StartsWith(n.Attributes[i].Name))
                    name = n.Attributes[i].InnerText;
                else
                    log("Var, unknown attribute " + n.Attributes[i].Name);
            }
            labels[index].Text = name + " (" + n.Name + isize.ToString() + ") :";
            labels[index].Location = new System.Drawing.Point(indent * 10 + 10, index * 25 + 15);
            labels[index].Show();
            textboxes[index].Text = "";
            textboxes[index].Location = new System.Drawing.Point(indent * 10 + 150, index * 25 + 15);
            textboxes[index].Show();
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
    }
}
