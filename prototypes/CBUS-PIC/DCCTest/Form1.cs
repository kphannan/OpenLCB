using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO.Ports;
using System.Xml;
using System.Threading;

namespace ProgramOnMain
{
    public partial class Form1 : Form
    {
        /*  OpenLCB frametypes.c

            9 Dec 2009

            Copyright (C) 2009    Mike Johnson

            This program is free software: you can redistribute it and/or modify
            it under the terms of the GNU General Public License as published by
            the Free Software Foundation, either version 3 of the License, or
            any later version.

            This program is distributed in the hope that it will be useful,
            but WITHOUT ANY WARRANTY; without even the implied warranty of
            MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
            GNU General Public License for more details.

            You should have received a copy of the GNU General Public License
            along with this program.  If not, see <http://www.gnu.org/licenses/>.
        */

        //*********************************************************************************
        //        Frame Types
        //*********************************************************************************

        enum FT
        {
            FT_CIM0 = 0x0000,   // Top 12 bits of NID
            FT_CIM1 = 0x1000,   // 2nd top 12 bits of NID
            FT_CIM2 = 0x2000,   // 3rd top 12 bits of NID
            FT_CIM3 = 0x3000,   // lowest 12 bits of NID
            FT_MR = 0x7FFE,   // Reset mapping, not sure this is actually useful ?
            FT_RIM = 0x7FFF,   // RIM

            // Broadcast type

            // Misc
            FT_RESET = 0x8000,   // System reset
            FT_VNSN = 0x8001,   // Verify Node Serial Number 
            FT_INIT = 0x8002,   // Normal Initialization Complete
            FT_BOOT = 0x8003,   // Boot Loader Initialization Complete

            // Accessory
            FT_EVENT = 0x8010,   // EVENT
            FT_RFID = 0x8011,   // RFID tag

            // Track commands
            FT_TOF = 0x8020,   // Track Off, broadcast from CS
            FT_TON = 0x8021,   // Track On or Normal operation, broadcast from CS
            FT_ESTOP = 0x8022,   // Track Stopped (em. stop)
            // FT_CSRESET = 0x8023,   // Command station Reset
            FT_RTOF = 0x8024,   // Request Track Off, from CAB
            FT_RTON = 0x8025,   // Request Track On or Normal operation, from CAB
            FT_RESTP = 0x8026,   // Request Emergency Stop ALL

            // CAB commands
            FT_RLOC = 0x8030,   // Request loco info
            FT_STMOD = 0x8031,   // Request speed step change
            FT_DSPD = 0x8032,   // Set Engine Speed/Dir
            FT_DFUN = 0x8033,   // Set engine functions
            FT_PLOC = 0x8034,   // Engine report from CS
            FT_PLOCF = 0x8035,   // Engine function report from CS
            FT_KLOC = 0x8036,   // Release loco

            // Consist commands
            FT_PCON = 0x8037,   // Consist Engine
            FT_KCON = 0x8038,   // Remove engine from consist

            // DCC programming
            FT_RDCC3 = 0x8040,   // Request 3 byte DCC packet
            FT_RDCC4 = 0x8041,   // Request 4 byte DCC packet
            FT_RDCC5 = 0x8042,   // Request 5 byte DCC packet
            FT_RDCC6 = 0x8043,   // Request 6 byte DCC packet

            FT_DAA = 0xE000,   // Destination Alias Addressed, message type in the data
            FT_STREAM = 0xF000,   // Stream data
        };

        // Destination Addressed, 1st byte of data has message type
        // 0123456789012
        // :X1EdddsssN

        enum DAA
        {
            DAA_DATA = 0x00,      // up to 0F, 7 bytes of data sequence number in low 4 bits
            DAA_DATA0 = 0x00,
            DAA_DATA1 = 0x01,
            DAA_DATA2 = 0x02,
            DAA_DATA3 = 0x03,
            DAA_DATA4 = 0x04,
            DAA_DATA5 = 0x05,
            DAA_DATA6 = 0x06,
            DAA_DATA7 = 0x07,
            DAA_DATA8 = 0x08,
            DAA_DATA9 = 0x09,
            DAA_DATAA = 0x0A,
            DAA_DATAB = 0x0B,
            DAA_DATAC = 0x0C,
            DAA_DATAD = 0x0D,
            DAA_DATAE = 0x0E,
            DAA_DATAF = 0x0F,
            DAA_ACK = 0x10,      // ack with status
            // Loader
            DAA_UPGSTART = 0x20,      // enter loader
            DAA_UPGRESET = 0x21,      // start program
            DAA_UPGREAD = 0x22,      // read 64 bytes
            DAA_UPGADDR = 0x23,      // write 64 bytes
            // Events
            DAA_CEERASEH = 0x30,      // consumer erase events, High 7 bytes
            DAA_CEERASEL = 0x31,      // consumer erase events, Low byte
            DAA_CEREADH = 0x32,      // consumer read events, High 7 bytes
            DAA_CEREADL = 0x33,      // consumer read events, Low byte, index, data length byte
            DAA_CEWRITEH = 0x34,      // consumer write event, High 7 bytes
            DAA_CEWRITEL = 0x35,      // consumer write event, Low byte, data length, up to 5 data bytes
            DAA_PEERASE = 0x36,      // producer erase event, index
            DAA_PEREAD = 0x37,      // producer read event, index
            DAA_PEWRITEH = 0x38,      // producer write event, High 7 bytes
            DAA_PEWRITEL = 0x39,      // producer write event, Low byte, index  
            // Node variables
            DAA_NVREAD = 0x40,      // read, 1 byte index
            DAA_NVWRITE = 0x41,      // set, 1 byte index + 1 byte data
            DAA_NVREPLY = 0x42,      // reply to read
            // Misc
            DAA_NSN = 0x50,      // Node serial number
            DAA_DEFAULT = 0x51,      // Reset (almost) everything to default values
            DAA_REBOOT = 0x52       // Re-boot the module, after node ID write
        };

        enum ACK
        {
            ACK_OK = 0,         // OK
            // ACK_CRC      = 1,         // CRC error, no longer used
            ACK_TIMEOUT = 2,         // timeout on data transfer, 2 seconds
            ACK_NODATA = 3,         // The requested data does not exist 
            ACK_NOSPACE = 4,         // No space to store this data 
            ACK_ALIASERROR = 5         // Wrong SourceAlias, probably 2 writes at the same time
        };

        //*********************************************************************************
        // Dictionary
        //*********************************************************************************

        static SortedList<string, string> namedict = new SortedList<string, string>();

        private void readdictionary()
        {
            try
            {
                XmlDocument doc = new XmlDocument();
                string dictname = "local";
                doc.Load(dictname + ".xmldict");
                XmlNodeList names = doc.GetElementsByTagName("Name");
                XmlNodeList values = doc.GetElementsByTagName("Value");
                for (int i = 0; i < names.Count; ++i)
                {
                    namedict.Add(dictname + "." + names[i].InnerText, values[i].InnerText);
                }
            }
            catch { };
        }

        private string lookupv(string v)
        {
            // v can be 12 or 16 hex digits
            // full name of module or event in dictionary ?
            int i = namedict.IndexOfValue(v);
            if (i >= 0)
                return namedict.Keys[i];

            string modulename = v.Substring(0, 12);
            string eventname = "";
            // if event convert event index to decimal
            if (v.Length == 16)
            {
                int index = Convert.ToInt32(v.Substring(12, 4), 16);
                string t = "Off";
                if ((index & 1) == 1)
                    t = "On";
                eventname = "." + t + (index / 2).ToString();
            }
            // only module name in dictionary ?
            i = namedict.IndexOfValue(v.Substring(0, 12));
            if (i >= 0)
                return namedict.Keys[i] + eventname;

            // Merg module
            if (v.Substring(0, 4) == "0302")
                return "global.MERG" + Convert.ToInt32(v.Substring(4, 6), 16).ToString()
                    + "/" + Convert.ToInt32(v.Substring(10, 2), 16).ToString() + eventname;

            return v.Substring(0, 12) + eventname;
        }

        private string packettostring(string s)
        {
            if (s.StartsWith(">"))
                s = s.Substring(1);
            try
            {
                if (s.Substring(0, 3) == ":X1")
                {
                    switch (s.Substring(3, 1))
                    {
                        case "0": // CIM 0
                            return "CIM0 " + s.Substring(4, 3) + ", alias=" + s.Substring(7, 3);
                        case "1":
                            return "CIM1 " + s.Substring(4, 3) + ", alias=" + s.Substring(7, 3);
                        case "2":
                            return "CIM2 " + s.Substring(4, 3) + ", alias=" + s.Substring(7, 3);
                        case "3":
                            return "CIM3 " + s.Substring(4, 3) + ", alias=" + s.Substring(7, 3);
                        case "7": // RIM
                            return "RIM " + s.Substring(4, 3) + " " + s.Substring(7, 3);
                        case "8":
                            FT op = (FT)Convert.ToInt32(s.Substring(3, 4), 16);
                            switch (op)
                            {
                                case FT.FT_EVENT:
                                    return op.ToString() + " " + lookupv(s.Substring(11, 16));
                                case FT.FT_RFID:
                                    return op.ToString() + " " + s.Substring(11, 10);
                            }
                            return op.ToString();
                        case "E":
                            DAA dop = (DAA)Convert.ToInt32(s.Substring(11, 2), 16);
                            switch (dop)
                            {
                                case DAA.DAA_NSN:
                                    return dop.ToString() + " alias=" + s.Substring(7, 3)
                                        + ", " + lookupv(s.Substring(13));
                            }
                            return dop.ToString() + " " + s.Substring(4, 3) + " " + " " + s.Substring(7, 3)
                                + " " + s.Substring(13);
                    }
                }
            }
            catch { };
            return "";
        }

        private string lookupn(string n)
        {
            if (n.Substring(0, 4).ToUpper() == "MERG")
            {
                int i = n.IndexOf('/');
                if (i >= 0)
                {
                    return "0302" + Convert.ToInt32(n.Substring(4, i - 5)).ToString("X6")
                        + Convert.ToInt32(n.Substring(i + 1)).ToString("X2");
                }
            }
            try
            {
                return namedict[n];
            }
            catch
            {
                return "unkown name";
            }
        }

        //*********************************************************************************

        private static Semaphore waitinfo = new Semaphore(0, 1);
        private static bool nofnsend = false;

        private void backgroundWorker1_DoWork(object sender, DoWorkEventArgs e)
        {
            while (true)
            {
                // CAN input
                line += com.ReadExisting();
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
                log(cmd);
                FT sw = (FT)Convert.ToInt32(cmd.Substring(3, 4), 16);
                if (sw == FT.FT_PLOC)
                {
                    if (loco == (Convert.ToInt32(cmd.Substring(13, 4), 16) & 0x3FFF))
                    {
                        locosession = cmd.Substring(11, 2);
                        waitinfo.Release();
                    }
                }
                else if (sw == FT.FT_PLOCF && loco == (Convert.ToInt32(cmd.Substring(13, 4), 16) & 0x3FFF))
                {
                    locosession = cmd.Substring(11, 2);
                    nofnsend = true;
                    fns[0] = Convert.ToByte(cmd.Substring(17, 2), 16);
                    fns[1] = Convert.ToByte(cmd.Substring(19, 2), 16);
                    fns[2] = Convert.ToByte(cmd.Substring(21, 2), 16);
                    fns[3] = Convert.ToByte(cmd.Substring(23, 2), 16);
                    F0cb.Checked = (fns[0] & 0x10) != 0;
                    F1cb.Checked = (fns[0] & 0x01) != 0;
                    F2cb.Checked = (fns[0] & 0x02) != 0;
                    F3cb.Checked = (fns[0] & 0x04) != 0;
                    F4cb.Checked = (fns[0] & 0x08) != 0;
                    F5cb.Checked = (fns[1] & 0x01) != 0;
                    F6cb.Checked = (fns[1] & 0x02) != 0;
                    F7cb.Checked = (fns[1] & 0x04) != 0;
                    F8cb.Checked = (fns[1] & 0x08) != 0;
                    F9cb.Checked = (fns[1] & 0x10) != 0;
                    F10cb.Checked = (fns[1] & 0x20) != 0;
                    F11cb.Checked = (fns[1] & 0x40) != 0;
                    Thread.Sleep(20);
                    nofnsend = false;
                }
            }
        }

        //*********************************************************************************

        static int loco = 0;
        static byte[] fns = new byte[4];
        static string locosession = "00";
        static System.IO.Ports.SerialPort com;
        static string line = "";
        bool cv2 = false;
        bool cv3 = false;
        bool cv4 = false;
        bool cv5 = false;
        bool cv6 = false;
        bool cv15 = false;
        bool cv19 = false;
        bool cvgen = false;

        public Form1()
        {
            InitializeComponent();
            CheckForIllegalCrossThreadCalls = false;
            foreach (string s in SerialPort.GetPortNames())
                comPortTb.Items.Add(s);
            readdictionary();
        }

        private void log(string s)
        {
            if (InTB.Text.Length>500)
                InTB.Text = InTB.Text.Substring(InTB.Text.Length-500);
            InTB.Text = InTB.Text + "\r\n" + s + " = " + packettostring(s);
            InTB.Select(InTB.Text.Length, 0);
            InTB.ScrollToCaret();
            InTB.Refresh();
        }

        private void OpenLCB(FT v, string s)
        {
            string s2 = ":X1" + ((int)v).ToString("X4") + "001N" + s + ";";
            log(s2);
            try
            {
                com.WriteLine(":X1" + ((int)v).ToString("X4") + "001N" + s + ";");
            }
            catch
            {
                log("Com port not open");
            }
        }

        private void OpenBtn_Click(object sender, EventArgs e)
        {
            try
            {
                com = new SerialPort(comPortTb.Text, 115200);
                com.Open();
                log("Com port opened.");
                OpenLCB(FT.FT_INIT, "");
                backgroundWorker1.RunWorkerAsync();
            }
            catch
            {
                log("Failed to open Com port.");
            }
        }

        private void allocloco(int newloco)
        {
            OpenLCB(FT.FT_DSPD, locosession + "80"); // stop old loco
            OpenLCB(FT.FT_KLOC, locosession); // release old loco
            loco = newloco;
            OpenLCB(FT.FT_RLOC, loco.ToString("X4")); // allocate new loco
            if (!waitinfo.WaitOne(1000))
                log("No reply from command station.");
            else
                OpenLCB(FT.FT_STMOD, locosession+"00"); // set 128 step speed mode
        }

        private void AllocBtn_Click(object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            allocloco(Convert.ToInt16(LocoTB.Text));
            Cursor.Current = Cursors.Default;
        }

        private void ChangeBt_Click(object sender, EventArgs e)
        {
            int newadr = Convert.ToInt16(NewAdrTB.Text);
            if (newadr <= 127)
            {
                writecv(1, newadr);
                Thread.Sleep(200);
                writebitcv(29, 5, 0); // only works if not already 0
            }
            else
            {
                int xnewadr = newadr | 0xC000;
                writecv(17, xnewadr >> 8);
                Thread.Sleep(200);
                writecv(18, xnewadr & 0xFF); // also writes CV17
                Thread.Sleep(200);
                writebitcv(29, 5, 1); // only works if not already 1
            }
            allocloco(newadr);
            LocoTB.Text = newadr.ToString();
        }

        private void SpeedTB_ValueChanged(object sender, EventArgs e)
        {
            int s = Convert.ToInt16(SpeedTB.Value);
            trackBar1.Value = s;
            if (s == 0) // stop
                s = 0x80;
            else if (s < 0) { // rev
                s = ((-s)+1);
                if (s > 0x7F)
                    s = 0x7F;
            }
            else { // fwd
                s = (s+1);
                if (s>0x7f)
                    s = 0x7F;
                s |= 0x80;
            }
            OpenLCB(FT.FT_DSPD, locosession + s.ToString("X2"));
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

        private void StopBtn_Click(object sender, EventArgs e)
        {
            SpeedTB.Value = 0;
            trackBar1.Value = 0;
        }

        //*********************************************************************************
        // CV's
        //*********************************************************************************

        private void writecv(int cv, int v)
        {
            cv = 0xEC00 | (cv - 1); // long form CV write
            string ls;
            if (loco > 0 && loco <= 127)
            {
                ls = loco.ToString("X2");
                OpenLCB(FT.FT_RDCC4, "82" + ls + cv.ToString("X4") + v.ToString("X2"));
            }
            else
            {
                ls = (loco | 0xC000).ToString("X4");
                OpenLCB(FT.FT_RDCC5, "82" + ls + cv.ToString("X4") + v.ToString("X2"));
            }
        }

        private void writebitcv(int cv, int bit, int v)
        {
            cv = 0xE800 | (cv - 1); // long form CV write
            v = 0xF0 | (bit & 7) | (v<<3);
            string ls;
            if (loco > 0 && loco <= 127)
            {
                ls = loco.ToString("X2");
                OpenLCB(FT.FT_RDCC4, "82" + ls + cv.ToString("X4") + v.ToString("X2"));
            }
            else
            {
                ls = (loco | 0xC000).ToString("X4");
                OpenLCB(FT.FT_RDCC5, "82" + ls + cv.ToString("X4") + v.ToString("X2"));
            }
        }

        private void WriteBtn_Click(object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;

            if (cv2)
            {
                writecv(2, Convert.ToUInt16(StartTB.Text));
                cv2 = false;
                Thread.Sleep(200);
            }
            if (cv3)
            {
                writecv(3, Convert.ToUInt16(AccelTB.Text));
                cv3 = false;
                Thread.Sleep(200);
            }
            if (cv4)
            {
                writecv(4, Convert.ToUInt16(DecelTB.Text));
                cv4 = false;
                Thread.Sleep(200);
            }
            if (cv5)
            {
                writecv(5, Convert.ToUInt16(cv5TB.Text));
                cv5 = false;
                Thread.Sleep(200);
            }
            if (cv6)
            {
                writecv(6, Convert.ToUInt16(cv6TB.Text));
                cv6 = false;
                Thread.Sleep(200);
            }
            if (cv15)
            {
                writecv(15, Convert.ToUInt16(cv15tb.Text));
                cv15 = false;
                Thread.Sleep(200);
            }
            if (cv19)
            {
                writecv(19, Convert.ToUInt16(cv19TB.Text));
                cv19 = false;
                Thread.Sleep(200);
            }
            if (cvgen)
            {
                writecv(Convert.ToUInt16(CVTB.Text), Convert.ToUInt16(ValueTB.Text));
                cvgen = false;
            }
            Cursor.Current = Cursors.Default;
        }

        private void StartTB_TextChanged(object sender, EventArgs e)
        {
            cv2 = true;
        }

        private void AccelTB_TextChanged(object sender, EventArgs e)
        {
            cv3 = true;
        }

        private void DecelTB_TextChanged(object sender, EventArgs e)
        {
            cv4 = true;
        }

        private void ValueTB_TextChanged(object sender, EventArgs e)
        {
            cvgen = true;
        }

        private void cv5TB_TextChanged(object sender, EventArgs e)
        {
            cv5 = true;
        }

        private void cv6TB_TextChanged(object sender, EventArgs e)
        {
            cv6 = true;
        }

        private void cv15tb_TextChanged(object sender, EventArgs e)
        {
            cv15 = true;
        }

        private void cv19TB_TextChanged(object sender, EventArgs e)
        {
            cv19 = true;
        }

        //*********************************************************************************
        // FN's
        //*********************************************************************************

        private void F0cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F0cb.Checked)
                fns[0] |= 0x10;
            else
                fns[0] &= 0x0F;
            OpenLCB(FT.FT_DFUN, locosession + "01" + fns[0].ToString("X2"));
        }

        private void F1cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F1cb.Checked)
                fns[0] |= 0x01;
            else
                fns[0] &= 0x1E;
            OpenLCB(FT.FT_DFUN, locosession + "01" + fns[0].ToString("X2"));
        }

        private void F2cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F2cb.Checked)
                fns[0] |= 0x02;
            else
                fns[0] &= 0x1D;
            OpenLCB(FT.FT_DFUN, locosession + "01" + fns[0].ToString("X2"));
        }

        private void F3cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F3cb.Checked)
                fns[0] |= 0x04;
            else
                fns[0] &= 0x1B;
            OpenLCB(FT.FT_DFUN, locosession + "01" + fns[0].ToString("X2"));
        }

        private void F4cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F4cb.Checked)
                fns[0] |= 0x08;
            else
                fns[0] &= 0x17;
            OpenLCB(FT.FT_DFUN, locosession + "01" + fns[0].ToString("X2"));
        }

        private void F5cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F5cb.Checked)
                fns[1] |= 0x01;
            else
                fns[1] &= 0xFE;
            OpenLCB(FT.FT_DFUN, locosession + "02" + fns[1].ToString("X2"));
        }

        private void F6cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F6cb.Checked)
                fns[1] |= 0x02;
            else
                fns[1] &= 0xFD;
            OpenLCB(FT.FT_DFUN, locosession + "02" + fns[1].ToString("X2"));
        }

        private void F7cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F7cb.Checked)
                fns[1] |= 0x04;
            else
                fns[1] &= 0xFB;
            OpenLCB(FT.FT_DFUN, locosession + "02" + fns[1].ToString("X2"));
        }

        private void F8cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F8cb.Checked)
                fns[1] |= 0x08;
            else
                fns[1] &= 0xF7;
            OpenLCB(FT.FT_DFUN, locosession + "02" + fns[1].ToString("X2"));
        }

        private void F9cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F9cb.Checked)
                fns[1] |= 0x10;
            else
                fns[1] &= 0xEF;
            OpenLCB(FT.FT_DFUN, locosession + "03" + (fns[1] >> 4).ToString("X2"));
        }

        private void F10cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F10cb.Checked)
                fns[1] |= 0x20;
            else
                fns[1] &= 0xDF;
            OpenLCB(FT.FT_DFUN, locosession + "03" + (fns[1]>>4).ToString("X2"));
        }

        private void F11cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F11cb.Checked)
                fns[1] |= 0x40;
            else
                fns[1] &= 0xBF;
            OpenLCB(FT.FT_DFUN, locosession + "03" + (fns[1] >> 4).ToString("X2"));
        }

        private void F12cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F12cb.Checked)
                fns[1] |= 0x80;
            else
                fns[1] &= 0x7F;
            OpenLCB(FT.FT_DFUN, locosession + "03" + (fns[1] >> 4).ToString("X2"));
        }

        private void F13cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F13cb.Checked)
                fns[2] |= 0x01;
            else
                fns[2] &= 0xFE;
            OpenLCB(FT.FT_DFUN, locosession + "04" + fns[2].ToString("X2"));
        }

        private void F14cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F14cb.Checked)
                fns[2] |= 0x02;
            else
                fns[2] &= 0xFD;
            OpenLCB(FT.FT_DFUN, locosession + "04" + fns[2].ToString("X2"));
        }

        private void F15cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F15cb.Checked)
                fns[2] |= 0x04;
            else
                fns[2] &= 0xFB;
            OpenLCB(FT.FT_DFUN, locosession + "04" + fns[2].ToString("X2"));
        }

        private void F16cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F16cb.Checked)
                fns[2] |= 0x08;
            else
                fns[2] &= 0xF7;
            OpenLCB(FT.FT_DFUN, locosession + "04" + fns[2].ToString("X2"));
        }

        private void F17cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F17cb.Checked)
                fns[2] |= 0x10;
            else
                fns[2] &= 0xEF;
            OpenLCB(FT.FT_DFUN, locosession + "04" + fns[2].ToString("X2"));
        }

        private void F18cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F18cb.Checked)
                fns[2] |= 0x20;
            else
                fns[2] &= 0xDF;
            OpenLCB(FT.FT_DFUN, locosession + "04" + fns[2].ToString("X2"));
        }

        private void F19cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F19cb.Checked)
                fns[2] |= 0x40;
            else
                fns[2] &= 0xBF;
            OpenLCB(FT.FT_DFUN, locosession + "04" + fns[2].ToString("X2"));
        }

        private void F20cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F20cb.Checked)
                fns[2] |= 0x10;
            else
                fns[2] &= 0x7F;
            OpenLCB(FT.FT_DFUN, locosession + "04" + fns[2].ToString("X2"));
        }

        private void F21cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F21cb.Checked)
                fns[3] |= 0x01;
            else
                fns[3] &= 0xFE;
            OpenLCB(FT.FT_DFUN, locosession + "05" + fns[3].ToString("X2"));
        }

        private void F22cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F22cb.Checked)
                fns[3] |= 0x02;
            else
                fns[3] &= 0xFD;
            OpenLCB(FT.FT_DFUN, locosession + "05" + fns[3].ToString("X2"));
        }

        private void F23cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F23cb.Checked)
                fns[3] |= 0x04;
            else
                fns[3] &= 0xFB;
            OpenLCB(FT.FT_DFUN, locosession + "05" + fns[3].ToString("X2"));
        }

        private void F24cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F24cb.Checked)
                fns[3] |= 0x08;
            else
                fns[3] &= 0xF7;
            OpenLCB(FT.FT_DFUN, locosession + "05" + fns[3].ToString("X2"));
        }

        private void F25cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F25cb.Checked)
                fns[3] |= 0x10;
            else
                fns[3] &= 0xEF;
            OpenLCB(FT.FT_DFUN, locosession + "05" + fns[3].ToString("X2"));
        }

        private void F26cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F26cb.Checked)
                fns[3] |= 0x20;
            else
                fns[3] &= 0xDF;
            OpenLCB(FT.FT_DFUN, locosession + "05" + fns[3].ToString("X2"));
        }

        private void F27cb_CheckedChanged(object sender, EventArgs e)
        {
            if (nofnsend)
                return;
            if (F27cb.Checked)
                fns[3] |= 0x40;
            else
                fns[3] &= 0xBF;
            OpenLCB(FT.FT_DFUN, locosession + "05" + fns[3].ToString("X2"));
        }

   }
}
