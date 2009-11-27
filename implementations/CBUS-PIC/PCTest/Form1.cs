using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO.Ports;
using System.IO;
using System.Threading;
using System.Xml;

///
/// PCTest for testing and debugging MERGCBUS modules through CANUSB, CANRS or anything
///   showing up in Windows as a "COM" port.
///
/// Author(s): Mike Johnson
///            ...
///            ...
///            Gert Muller
///            
///

namespace PCTest
{
    public partial class Form1 : Form
    {
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
            FT_ACOF = 0x8010,   // Off
            FT_ACON = 0x8011,   // On
            FT_ASOF = 0x8012,   // Short Off
            FT_ASON = 0x8013,   // Short On
            FT_RFID = 0x8014,   // RFID tag

            // Track commands
            FT_TOF = 0x8020,   // Track Off, broadcast from CS
            FT_TON = 0x8021,   // Track On or Normal operation, broadcast from CS
            FT_ESTOP = 0x8022,   // Track Stopped (em. stop)
            FT_CSRESET = 0x8023,   // Command station Reset
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

        enum DAA
        {
            DAA_DATA = 0x00,      // up to 0F, 7 bytes of data sequence number in low 4 bits
            DAA_ACK = 0x10,      // ack with status
            // Loader
            DAA_UPGSTART = 0x11,      // enter loader
            DAA_UPGRESET = 0x12,      // start program
            DAA_UPGREAD = 0x13,      // read 64 bytes
            DAA_UPGADDR = 0x14,      // write 64 bytes
            // Events
            DAA_EVERASEH = 0x15,      // erase events, High 4 bytes
            DAA_EVERASEL = 0x16,      // erase events, Low 4 bytes
            DAA_EVREADH = 0x17,      // read events, High 4 bytes
            DAA_EVREADL = 0x18,      // read events, Low 4 bytes
            DAA_EVWRITEH = 0x19,      // write event, High 4 bytes
            DAA_EVWRITEL = 0x1A,      // write event, Low 4 bytes
            // Node variables
            DAA_NVRD = 0x1B,      // read
            DAA_NVSET = 0x1C,      // set
            DAA_NVANS = 0x1D,      // reply to read
            // Misc
            DAA_NSN = 0x1E       // Node serial number
        };


        // Data for NodeID's and Alias
        static SortedList<String, String> nodenumbers = new SortedList<String, String>();

        // Sparse array for the Intel hex file
        // Sorted list of 64 byte data blocks with address, in address order
        // To allow for initializing EEPROM the address is 3 bytes long.
        static SortedList<int, byte[]> memdata = new SortedList<int, byte[]>();

        static string serialLine = "";  // serial port input line
        static string log = "";         // message log
        static string dNN = "001";     // default node number
        static string canmsg;
        enum OPSTATE { IDLE, SENDING, MODULEID, USERID, NVCFGREAD, EVCFGREAD, CFGWRITE, NVREAD, EVREAD };
        static OPSTATE opstate = OPSTATE.IDLE;
        static int readingmodulestring = 0;
        static int readinguserstring = 0;
        static byte[] modulestr = new byte[70];
        static StreamWriter savefile;   // config save file
        static StreamReader readfile;   // config restore file
        static int nv;
        static int nb = 2;
        static int complete = 0;
        static string eventstr = "00000000000000000000";

        static String NID = "000000000001";
        static String NIDa = "001";

        // ack signal between background and foreground tasks
        static Semaphore ack = new Semaphore(0, 1);
        static int ackstatus = 0;

        // display options
        static bool optionFlashTaskBar = false;

        // first time serial port, so we can try to use the PCTestSettings.xml
        // after that, use the last used
        static bool firstTimeSerialPort = true;

        public Form1()
        {
            InitializeComponent();
            CheckForIllegalCrossThreadCalls = false;
            serialPort1.BaudRate = 115200; // 230400;
            serialPort1.Handshake = Handshake.None;

            // and since the port is not yet open
            enableCommButtons(false);
        }

        // convert a number to hex string

        public string hex(int a, int n)
        {
            switch (n)
            {
                case 1: return String.Format("{0:X1}", a & 0x0000000F);
                case 2: return String.Format("{0:X2}", a & 0x000000FF);
                case 3: return String.Format("{0:X3}", a & 0x00000FFF);
                case 4: return String.Format("{0:X4}", a & 0x0000FFFF);
                case 5: return String.Format("{0:X5}", a & 0x000FFFFF);
                case 6: return String.Format("{0:X6}", a & 0x00FFFFFF);
                case 7: return String.Format("{0:X7}", a & 0x0FFFFFFF);
                case 8: return String.Format("{0:X8}", a & 0xFFFFFFFF);
            }
            return "";
        }

        // savelog button clicked

        private void SaveBtn_Click(object sender, EventArgs e)
        {
            saveLogFileDialog.AddExtension = true;
            saveLogFileDialog.Filter = "Log files|*.log|All files|*.*";
            DialogResult res = saveLogFileDialog.ShowDialog();
            if (res.Equals(DialogResult.OK))
            {
                Stream file = saveLogFileDialog.OpenFile();
                displaylog();
                int l = log.Length;
                byte[] tmp = new byte[l];
                char[] t1 = log.ToCharArray();
                for (int i = 0; i < l; i++)
                {
                    tmp[i] = (byte)t1[i];
                }
                file.Write(tmp, 0, l);
                file.Close();
            }
        }

        // com port setup button clicked

        private void ComPortBtn_Click(object sender, EventArgs e)
        {
            ComPort cp = new ComPort();

            /// only load the xml data on first use
            if (firstTimeSerialPort == true)
            {
                XmlDocument doc = new XmlDocument();
                try
                {
                    doc.Load(@"PCTestSettings.xml");
                    foreach (XmlNode mnode in doc.SelectNodes("PCTest"))
                    {
                        try
                        {
                            string DefaultPort = mnode["PortName"].InnerText;
                            cp.ComPortStr = DefaultPort;
                        }
                        catch (System.NullReferenceException)
                        {
                            cp.ComPortStr = serialPort1.PortName;
                        }
                        try
                        {
                            string Baudrate = mnode["BaudRate"].InnerText;
                            cp.ComPortSpeed = Baudrate;
                        }
                        catch (System.NullReferenceException)
                        {
                            cp.ComPortSpeed = serialPort1.BaudRate.ToString();
                        }

                        try
                        {
                            string Handshake = mnode["Handshake"].InnerText;
                            cp.ComPortHandshake = Handshake;
                        }
                        catch (System.NullReferenceException)
                        {
                            cp.ComPortHandshake = serialPort1.Handshake.ToString();
                        }

                        try
                        {
                            string KeepLog = mnode["KeepLog"].InnerText;
                            if (KeepLog.ToUpper().ToString().Contains("TRUE"))
                                this.KeepLogopt.Checked = true;
                            else
                                this.KeepLogopt.Checked = false;
                        }
                        catch (System.NullReferenceException)
                        {
                            /// do nothing if not specified, keep status quo.
                        }
                        catch (System.ArgumentNullException)
                        {
                            /// do nothing if not specified, keep status quo.
                        }

                        try
                        {
                            string Flashing = mnode["FlashTaskBar"].InnerText;
                            if (Flashing.ToUpper().ToString().Contains("TRUE"))
                            {
                                optionFlashTaskBar = true;
                                this.FlashTaskBaropt.Checked = true;
                            }
                            else
                            {
                                optionFlashTaskBar = false;
                                this.FlashTaskBaropt.Checked = false;
                            }
                        }
                        catch (System.NullReferenceException)
                        {
                            /// do nothing if not specified, keep status quo.
                        }
                        catch (System.ArgumentNullException)
                        {
                            /// do nothing if not specified, keep status quo.
                        }
                    }
                }
                catch (System.IO.FileNotFoundException)
                {
                    cp.ComPortStr = serialPort1.PortName;
                    cp.ComPortSpeed = serialPort1.BaudRate.ToString();
                    cp.ComPortHandshake = serialPort1.Handshake.ToString();
                }
            }
            else
            {
                cp.ComPortStr = serialPort1.PortName;
                cp.ComPortSpeed = serialPort1.BaudRate.ToString();
                cp.ComPortHandshake = serialPort1.Handshake.ToString();
            }

            DialogResult res = cp.ShowDialog(this);
            if (res.Equals(DialogResult.OK))
            {
                if (serialPort1.IsOpen)
                {
                    serialPort1.Close();
                    enableCommButtons(false);
                    closeComPortBtn.Hide();
                }
                serialPort1.PortName = cp.ComPortStr;
                serialPort1.BaudRate = Convert.ToInt32(cp.ComPortSpeed);
                serialPort1.Handshake = (Handshake)Enum.Parse(typeof(Handshake), cp.ComPortHandshake);
                /// don't read the xml file again
                firstTimeSerialPort = false;
                try
                {
                    serialPort1.Open();

                    /// enable all the buttons if com is open
                    enableCommButtons(true);
                    closeComPortBtn.Show();
                    opstate = OPSTATE.IDLE;
                    readingmodulestring = 0;
                    readinguserstring = 0;

                    checkalias(NID, NIDa);

                    /// keep the log inbetween port close and open, if the 
                    /// checkbox is ticked
                    if (this.KeepLogopt.Checked)
                    {
                        /// don't clear the log, maybe insert a note in the log
                        /// that the port was re-opened
                    }
                    else
                    {
                        log = "";
                    }
                    displaylog();
                }
                catch (System.IO.IOException)
                {
                    /// show if port is not available or other exception
                    MessageBox.Show("Can not open " + cp.ComPortStr + "...",
                    "Serial port trouble", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                    /// and don't show the buttons if com failed
                    enableCommButtons(false);
                }
                catch (System.UnauthorizedAccessException)
                {
                    /// can't open a port that is already open somewhere else, 
                    /// or you don't have the correct rights
                    MessageBox.Show("Trouble accessing " + cp.ComPortStr + "..." +
                    "\nThe port might be in use by another program, " +
                    "\nor you might not have sufficient access.",
                    "Serial port not available", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                    /// and don't show the buttons if com failed
                    enableCommButtons(false);
                }
            }
        }

        private void checkalias(String NID, String NIDa)
        {
            sendmsg(":X1" + hex((int)FT.FT_INIT, 4) + NIDa + "N;");
            nodenumbers[NIDa] = NID;
        }

        private void enableCommButtons(bool enabled)
        {
            /// show or don't show the buttons that require com to be open
            this.EVreadBTN.Enabled = enabled;
            this.EVwriteBTN.Enabled = enabled;
            this.NVReadBtn.Enabled = enabled;
            this.NVwriteBTN.Enabled = enabled;
            this.RestoreConfigBtn.Enabled = enabled;
            this.ReadBtn.Enabled = enabled;
            this.SaveConfigBtn.Enabled = enabled;
            this.SendBtn.Enabled = enabled;
            this.SoftwareBtn.Enabled = enabled;
            this.WriteBtn.Enabled = enabled;
        }

        // update log string, only keep last 60000

        private void updatelog(string str)
        {
            // flash the taskbar once if option is selected
            if (optionFlashTaskBar == true)
            {
                FlashWindow.Flash(this, 1);
            }

            if (log.Length > 60000)
                log = log.Substring(log.Length - 60000) + str;
            else
                log += str;
        }

        // copy log string to display (only display the last 3000 characters

        private void displaylog()
        {
            if (log.Length > 20000)
            {
                LogSpace.Text = log.Substring(log.Length - 20000);
                // remove the first line if not starting with ":", i.e. don't allow a partial string on line 1
                // yes we should check for ">", but what if that was character 3001 which you just canned?
                LogSpace.Text = LogSpace.Text.Substring(LogSpace.Text.IndexOf(":"));
            }
            else
                LogSpace.Text = log;
            LogSpace.Select(LogSpace.Text.Length, 0);
            LogSpace.ScrollToCaret();
        }

        //**************************************************************************
        // Handle data from the serial port
        //**************************************************************************

        // assemble input from com port into lines

        private void serialPort1_DataReceived(object sender
          , SerialDataReceivedEventArgs e)
        {
            char c = ' ';
            while (serialPort1.BytesToRead > 0)
            {
                c = Convert.ToChar(serialPort1.ReadChar());
                serialLine += c;
                if (c == '\n')
                {
                    serialLine = serialLine.ToUpper();
                    updatelog(serialLine);
                    if (opstate == OPSTATE.SENDING)
                        handleinput();
                    else if (opstate == OPSTATE.NVCFGREAD)
                        NVCFGREAD();
                    else if (opstate == OPSTATE.EVCFGREAD)
                        EVCFGREAD();
                    else if (opstate == OPSTATE.CFGWRITE)
                        CfgWrite();
                    else if (opstate == OPSTATE.NVREAD)
                        NVRead();
                    else if (opstate == OPSTATE.EVREAD)
                        EVRead();
                    else if (readingmodulestring != 0 || readinguserstring != 0)
                        ReadModuleString();
                    else if (serialLine.Length >= 20 && serialLine.Contains(":X1E") && serialLine.Contains("N"+hex((int)DAA.DAA_NSN, 2)))
                    {   // node serial number
                        if (!NNtb.Items.Contains(serialLine.Substring(7, 3)))
                            NNtb.Items.Add(serialLine.Substring(7, 3));
                        nodenumbers[serialLine.Substring(7, 3)] = serialLine.Substring(13, 12);
                    }
                    serialLine = "";
                }
            }
            if (opstate != OPSTATE.SENDING)
            {
                displaylog();
            }
        }

        // handle input from the com port during software upgrade

        private void handleinput()
        {
            if (serialLine.StartsWith("ERROR")
              || serialLine.StartsWith("TOO LONG")
                //|| serialLine.StartsWith("-ECAN")
              || serialLine.StartsWith("-SERIAL"))
            {
                this.SendUpgrade.CancelAsync();
            }
            else if (serialLine.StartsWith(":X1E"))
            {
                if (serialLine.Length > 15 && serialLine.Substring(7, 6).Equals(dNN+"N"+hex((int)DAA.DAA_ACK, 2))) // ack cmd
                {
                    if (serialLine.Substring(13, 2).Equals("00"))
                    { // ack true
                        ackstatus = hv(serialLine[13], serialLine[14]);
                        ack.Release();
                    }
                    else
                        this.SendUpgrade.CancelAsync();
                }
            }
        }

        // handle input from the com port during module and user id string read

        private void ReadModuleString()
        {
            if (serialLine.StartsWith("ERROR")
              || serialLine.StartsWith("TOO LONG")
                // || serialLine.StartsWith("-ECAN")
              || serialLine.StartsWith("-SERIAL"))
            {
                readingmodulestring = 0;
                readinguserstring = 0;
                displaylog();
            }
            else if (serialLine.Length > 22 && serialLine.StartsWith(":X1E"))
            {
                if (serialLine.Substring(7, 5).Equals(dNN+"N0")) // ack cmd
                {
                    int offset = hv('0', serialLine[12]) * 7;
                    modulestr[offset] = hv(serialLine[13], serialLine[14]);
                    modulestr[offset + 1] = hv(serialLine[15], serialLine[16]);
                    modulestr[offset + 2] = hv(serialLine[17], serialLine[18]);
                    modulestr[offset + 3] = hv(serialLine[19], serialLine[20]);
                    modulestr[offset + 4] = hv(serialLine[21], serialLine[22]);
                    modulestr[offset + 5] = hv(serialLine[23], serialLine[24]);
                    modulestr[offset + 6] = hv(serialLine[25], serialLine[26]);
                    if (readingmodulestring != 0)
                    {
                        readingmodulestring--;
                        if (readingmodulestring == 0) // module string complete
                        {
                            NodeText.Text = "";
                            for (int i = 0; i < 64; i++)
                            {
                                if (modulestr[i] == 0)
                                    break;
                                else
                                    NodeText.Text += (char)modulestr[i];
                            }
                            readinguserstring = 10;
                            SendReadCmd(0x00C0); // start reading user id string
                            for (int i = 0; i < 70; i++)
                                modulestr[i] = 0;
                        }
                    }
                    else
                    {
                        readinguserstring--;
                        if (readinguserstring == 0) // user id string complete
                        {
                            UserText.Text = "";
                            for (int i = 0; i < 64; i++)
                            {
                                if (modulestr[i] == 0)
                                    break;
                                else
                                    UserText.Text += (char)modulestr[i];
                            }
                            displaylog();
                        }
                    }
                }
            }
        }

        // Send button click, sends a single packet from the nearby text box

        private void SendBtn_Click(object sender, EventArgs e)
        {
            sendmsg(SendText.Text);
        }

        // convert 2 hex characters to a number

        private byte hv(int a, int b)
        {
            if (a >= 'a') a = (byte)(a - 'a' + 10);
            else if (a >= 'A') a = (byte)(a - 'A' + 10);
            else a = (byte)(a - '0');
            if (b >= 'a') b = (byte)(b - 'a' + 10);
            else if (b >= 'A') b = (byte)(b - 'A' + 10);
            else b = (byte)(b - '0');
            return (byte)(a * 16 + b);
        }

        // send a CAN packet to the serial interface

        public bool sendmsg(string cmd)
        {
            updatelog(">" + cmd + "\r\n");
            // displaylog();
            try
            {
                serialPort1.Write(cmd);
            }
            catch (System.IO.IOException)
            {
                MessageBox.Show("Serial Port sendmsg error sending [" + cmd + "]",
                    "Serial port error", MessageBoxButtons.OK, MessageBoxIcon.Warning);

                return false;
            }
            catch (InvalidOperationException)
            {
                // oops
                MessageBox.Show("Serial Port sendmsg InvalidOperationException error sending [" + cmd + "]",
                    "Serial port error", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                return false;
            }

            return true;
        }

        // Store a data byte in the sorted list of 64 byte data blocks

        private void storebyte(int address, byte v)
        {
            int ra = address & ~63;
            int ro = address & 63;
            byte[] t;
            if (memdata.ContainsKey(ra))
            {
                t = memdata[ra];
                t[ro] = v;
                memdata[ra] = t;
            }
            else
            {
                t = new byte[64];
                for (int i = 0; i < 64; i++)
                    t[i] = 0xFF;
                t[ro] = v;
                memdata.Add(ra, t);
            }
        }

        // get a byte from the sorted list of 64 byte data blocks.

        private byte getbyte(int address)
        {
            int ra = address & ~63;
            int ro = address & 63;
            byte[] t;
            if (memdata.ContainsKey(ra))
            {
                t = memdata[ra];
                return t[ro];
            }
            else
                return 0xFF;
        }

        // prepare software download, read the Intel hex file
        /* http://en.wikipedia.org/wiki/.hex
             : is the colon that starts every Intel HEX record.
            ll is the record-length field that represents the number of data
               bytes (dd) in the record.
            aaaa is the address field that represents the starting address for
               subsequent data in the record.
            tt is the field that represents the HEX record type, which may be
               one of the following:
               00 - data record
               01 - end-of-file record
               02 - extended segment address record
               04 - extended linear address record
            dd is a data field that represents one byte of data. A record may have
               multiple data bytes. The number of data bytes in the record must
               match the number specified by the ll field.
            cc is the checksum field that represents the checksum of the record.
               The checksum is calculated by summing the values of all hexadecimal
               digit pairs in the record modulo 256 and taking the two's complement.
        */

        private void SoftwareBtn_Click(object sender, EventArgs e)
        {
            dNN = NNtb.Text.PadLeft(3, '0').Substring(0, 3);
            progressBar1.Value = 0;

            DialogResult res = openFileDialog1.ShowDialog();

            if (res.Equals(DialogResult.OK))
            {
                Stream file = openFileDialog1.OpenFile();
                int l = (int)file.Length;
                byte[] f = new byte[l];
                int i;
                int recordaddress = 0;
                // clear memory image
                memdata.Clear();

                // read intel hex file
                file.Read(f, 0, l);
                file.Close();

                i = 0;
                while (i < l)
                {
                    if ((char)(f[i]) == ':')
                    {   // :llaaaattdd...cc
                        i++;
                        // length
                        int recordlength = hv(f[i], f[i + 1]);
                        int recordchecksum = recordlength;
                        i += 2;
                        // address high
                        int t = hv(f[i], f[i + 1]);
                        i += 2;
                        recordchecksum += t;
                        recordaddress = (int)(((UInt32)recordaddress & 0xFFFF0000) | ((UInt32)t << 8));
                        // address low
                        t = hv(f[i], f[i + 1]);
                        i += 2;
                        recordchecksum += t;
                        recordaddress = recordaddress | t;
                        // type
                        int recordtype = hv(f[i], f[i + 1]);
                        recordchecksum += recordtype;
                        i += 2;
                        // calc checksum
                        for (int j = 0; j <= recordlength; j++)
                        {
                            recordchecksum += hv(f[i + j * 2], f[i + j * 2 + 1]);
                        }
                        if ((recordchecksum & 0xFF) != 0)
                        {
                            MessageBox.Show("File has a checksum error"
                              , openFileDialog1.FileName); // sumcheck error
                            return;
                        }

                        if (recordtype == 0) // data record
                        {
                            // data
                            for (int j = 0; j < recordlength; j++)
                            {
                                t = hv(f[i], f[i + 1]);
                                i += 2;
                                storebyte(recordaddress, (byte)t);
                                recordaddress++;
                            }
                        }
                        else if (recordtype == 1) // end of file
                            break;
                        else if (recordtype == 4) // high address bits
                        {
                            t = hv(f[i], f[i + 1]) * 256 + hv(f[i + 2], f[i + 3]);
                            i += 4;
                            recordaddress = (t << 16) | (recordaddress & 0xFFFF);
                        }
                    }
                    else i++;
                }

                // get module id string from the hex file
                string text = "";
                for (i = 0x1040; i < 0x1080; i++)
                {
                    if (getbyte(i) == 0 || getbyte(i) == 0xFF)
                        break;
                    else
                        text += (char)getbyte(i);
                }

                // display a message box about what will happen next
                res = MessageBox.Show("Sending " + text + " to NodeNumber " + dNN,
                    openFileDialog1.FileName, MessageBoxButtons.OKCancel);
                if (res.Equals(DialogResult.Cancel))
                    return;

                // check if a new program or just a data table
                for (i = 0; i < memdata.Count; i++)
                {
                    recordaddress = memdata.Keys[i];
                    if (recordaddress >= 0x1000 && recordaddress < 0x3000) // new program
                        storebyte(0x1027, 0xFF); // set "no valid program loaded" flag 
                }

                opstate = OPSTATE.SENDING;
                // use up any spare ack commands
                int n = ack.Release() + 1;
                while (n-- > 0)
                    ack.WaitOne();
                EnterLoader();   // send download start
                enableCommButtons(false);
                try
                {
                    this.SendUpgrade.RunWorkerAsync(); // start background task
                }
                catch (InvalidOperationException)
                {
                    MessageBox.Show("An error uprading NodeNumber " + dNN ); // invalid operation error
                    return;
                }
            }
        }

        // change of nodenumber in text box

        private void NNtb_TextChanged(object sender, EventArgs e)
        {
            dNN = NNtb.Text.PadLeft(3, '0').Substring(0, 3);
        }

        // background task to send an upgrade

        private void SendUpgrade_DoWork(object sender, DoWorkEventArgs e)
        {
            BackgroundWorker bw = sender as BackgroundWorker;
            e.Result = SendUpgradeWork(bw);
            if (bw.CancellationPending)
                e.Cancel = true;
        }

        private int SendUpgradeWork(BackgroundWorker bw)
        {
            int address;
            string canmsg;
            Thread.Sleep(1);
            ack.WaitOne(); // wait for act to enter loader cmd
            if (ackstatus != 0)
            {
                enableCommButtons(true);
                return 0;
            }

            this.progressBar1.Maximum = memdata.Count;
            this.progressBar1.Step = 1;
            for (int i = 0; i < memdata.Count; i++)
            {
                this.progressBar1.Value = i;

                if (bw.CancellationPending)
                    break;
                address = memdata.Keys[i];
                if (address < 0x1000) // don't overwrite the loader
                    continue;
                byte[] t = memdata.Values[i];
                SendWriteCmd(address);  // send 3 byte address of block
                Thread.Sleep(1);        // allows foreground task to get any input
                for (int j = 0; j < 10; j++)
                {   // send 9 data packets with 7 bytes and 1 with 1 byte.
                    canmsg = ":X1E"+dNN+NIDa+"N";
                    canmsg += hex((int)DAA.DAA_DATA | j, 2);
                    canmsg += hex(t[address & 63], 2); // 7 bytes data
                    if (j != 9)
                    {
                        canmsg += hex(t[(address + 1) & 63], 2);
                        canmsg += hex(t[(address + 2) & 63], 2);
                        canmsg += hex(t[(address + 3) & 63], 2);
                        canmsg += hex(t[(address + 4) & 63], 2);
                        canmsg += hex(t[(address + 5) & 63], 2);
                        canmsg += hex(t[(address + 6) & 63], 2);
                    }
                    canmsg += ";";
                    sendmsg(canmsg);
                    Thread.Sleep(1);
                    address += 7;
                }
                // wait for ack from module to signal write completed
                Thread.Sleep(1);
                ack.WaitOne();
                if (ackstatus != 0)
                {
                    enableCommButtons(true);
                    return 0;
                }
            }
            // if everything is OK, send the reset packet
            if (!bw.CancellationPending)
            {
                Reset();
                updatelog("Upgrade Done\r\n");
            }
            else
            {
                updatelog("Upgrade Error\r\n");
            }
            enableCommButtons(true);
            displaylog();
            opstate = OPSTATE.IDLE;
            return 0;
        }

        //**************************************************************************
        // Read and Write Module and User ID strings
        //**************************************************************************

        // Read Button click. Read module id string and user id string from a module

        private void ReadBtn_Click(object sender, EventArgs e)
        {
            dNN = NNtb.Text.PadLeft(3, '0').Substring(0, 3);
            UserText.Text = "";
            NodeText.Text = "";
            readingmodulestring = 10;
            SendReadCmd(0x1040);
            for (int i = 0; i < 70; i++)
                modulestr[i] = 0;
        }

        // Write button clicked. Write user id string  to a module

        private void WriteBtn_Click(object sender, EventArgs e)
        {
            dNN = NNtb.Text.PadLeft(3, '0').Substring(0, 3);
            for (int i = 0; i < 70; i++)
                modulestr[i] = 0;

            for (int i = 0; i < 64 && i < UserText.Text.Length; i++)
                modulestr[i] = (byte)UserText.Text[i];
            SendWriteCmd(0x0000C0);
            this.WriteUserId.RunWorkerAsync();
        }

        // Background task to write user id string to a module

        private void WriteUser_DoWork(object sender, DoWorkEventArgs e)
        {
            BackgroundWorker bw = sender as BackgroundWorker;
            e.Result = WriteUserIdWork(bw);
        }

        private int WriteUserIdWork(BackgroundWorker bw)
        {
            string canmsg;

            for (int j = 0, offset = 0; j < 10; j++, offset+=7)
            {
                canmsg = ":X1E" + dNN + NIDa + "N";
                canmsg += hex((int)DAA.DAA_DATA | j, 2);
                canmsg += hex(modulestr[offset], 2); // 7 bytes data
                if (j != 9)
                {
                    canmsg += hex(modulestr[offset + 1], 2);
                    canmsg += hex(modulestr[offset + 2], 2);
                    canmsg += hex(modulestr[offset + 3], 2);
                    canmsg += hex(modulestr[offset + 4], 2);
                    canmsg += hex(modulestr[offset + 5], 2);
                    canmsg += hex(modulestr[offset + 6], 2);
                }
                canmsg += ";";
                sendmsg(canmsg);
                Thread.Sleep(1);
            }
            return 0;
        }

        private void NNtb_SelectedIndexChanged(object sender, EventArgs e)
        {
            dNN = NNtb.Text.PadLeft(3, '0').Substring(0, 3);
            NIDtext.Text = nodenumbers[dNN];
            UserText.Text = "";
            NodeText.Text = "";
            readingmodulestring = 10;
            SendReadCmd(0x1040);
            for (int i = 0; i < 70; i++)
                modulestr[i] = 0;
        }

        //**************************************************************************
        // Save module configuration
        //**************************************************************************

        private void SaveConfigBtn_Click(object sender, EventArgs e)
        {
            dNN = NNtb.Text.PadLeft(3, '0').Substring(0, 3);
            saveLogFileDialog.AddExtension = true;
            saveLogFileDialog.Filter = "Config files|*.cfg|All files|*.*";
            DialogResult res = saveLogFileDialog.ShowDialog();
            if (res.Equals(DialogResult.OK))
            {
                savefile = new StreamWriter(saveLogFileDialog.FileName);
                savefile.WriteLine("NodeNumber=" + String.Format("{0:X4}", dNN));
                nv = 0;
                opstate = OPSTATE.NVCFGREAD;
                SendNvReadCmd(hex(nv,2)); // send the first read command
            }
        }

        // called to handle one serial input line

        private void NVCFGREAD()
        {
            if (serialLine.StartsWith("ERROR")
              || serialLine.StartsWith("TOO LONG")
                // || serialLine.StartsWith("-ECAN")
              || serialLine.StartsWith("-SERIAL"))
            {
                opstate = OPSTATE.IDLE;
                updatelog("Aborted due to error.");
                displaylog();
            }
            else if (serialLine.Length > 13 && serialLine.StartsWith(":X1E")
                && serialLine.Substring(7, 3).Equals(dNN))
            {
                if (serialLine.Substring(10, 3).Equals("N" + hex((int)DAA.DAA_NVANS,2))) // nvans
                {
                    savefile.WriteLine("NV" + serialLine.Substring(13, 4));
                    nv++;
                    SendNvReadCmd(hex(nv,2));
                }
                else if (serialLine.Substring(10, 5).Equals("N" + hex((int)DAA.DAA_ACK, 2)+"03")) // no more data
                {
                    opstate = OPSTATE.EVCFGREAD;
                    nv = 0;
                    SendEvReadCmd("0000000000000000", nv);
                }
            }
        }

        private void EVCFGREAD()
        {
            if (serialLine.StartsWith("ERROR")
              || serialLine.StartsWith("TOO LONG")
                // || serialLine.StartsWith("-ECAN")
              || serialLine.StartsWith("-SERIAL"))
            {
                opstate = OPSTATE.IDLE;
                updatelog("Aborted due to error.");
                displaylog();
            }
            else if (serialLine.Length > 17 && serialLine.StartsWith(":X1E")
                && serialLine.Substring(7, 3).Equals(dNN))
            {
                if (serialLine.Substring(10, 5).Equals("N" + hex((int)DAA.DAA_ACK, 2)+"03")) // no more data
                {
                    opstate = OPSTATE.IDLE;
                    updatelog("Saved.");
                    displaylog();
                    savefile.WriteLine("End");
                    savefile.Close();
                }
                else if (serialLine.Substring(10, 3).Equals("N" + hex((int)DAA.DAA_EVWRITEH,2))) // EVCFGWRITE
                {
                    eventstr = serialLine.Substring(13, 8) + eventstr.Substring(8);
                    complete |= 0x4;
                }
                else if (serialLine.Substring(10, 3).Equals("N" + hex((int)DAA.DAA_EVWRITEL,2))) // EVCFGWRITE
                {
                    eventstr = eventstr.Substring(0, 8) + serialLine.Substring(13, 8) + eventstr.Substring(16);
                    complete |= 0x2;
                    nb = hv(serialLine[21], serialLine[22]);
                }
                else if (serialLine.Substring(10, 3).Equals("N" + hex((int)DAA.DAA_DATA,2))) // evdata
                {
                    eventstr = eventstr.Substring(0, 16) + serialLine.Substring(13, nb * 2);
                    complete |= 0x1;
                }
                if (complete==7) {
                    savefile.WriteLine("EV" + eventstr);
                    nv++;
                    SendEvReadCmd("00000000", nv);
                    complete = 0;
                }
            }
        }

        //**************************************************************************
        // Packet commands
        //**************************************************************************

        // Send 1 packet - 64 byte block read cmd

        private void SendReadCmd(long address)
        { 
            canmsg = ":X1E" + dNN + NIDa + "N";
            canmsg += hex((int)DAA.DAA_UPGREAD, 2);    // opc
            canmsg += hex((int)address, 6); // address
            canmsg += ";";
            sendmsg(canmsg);
        }

        // Send 1 packet - 64 byte block write cmd

        private void SendWriteCmd(long address)
        {
            // address CAN packet
            canmsg = ":X1E" + dNN + NIDa + "N";
            canmsg += hex((int)DAA.DAA_UPGADDR, 2);      // opc
            canmsg += hex((int)address, 6);
            canmsg += ";";
            sendmsg(canmsg);
        }

        // Send 1 packet - module enter loader cmd

        private void EnterLoader()
        {
            canmsg = ":X1E" + dNN + NIDa + "N";
            canmsg += hex((int)DAA.DAA_UPGSTART, 2);      // opc
            canmsg += ";";
            sendmsg(canmsg);
        }

        // Send 1 packet - send module reset cmd

        private void Reset()
        {
            // send reset packet
            canmsg = ":X1E" + dNN + NIDa + "N";
            canmsg += hex((int)DAA.DAA_UPGRESET, 2);      // opc
            canmsg += ";";
            sendmsg(canmsg);
        }

        private void SendNvReadCmd(string nv)
        {
            canmsg = ":X1E" + dNN + NIDa + "N";
            canmsg += hex((int)DAA.DAA_NVRD, 2);      // opc
            canmsg += nv;                       // NV index
            canmsg += ";";
            sendmsg(canmsg);
        }

        private void SendNvWriteCmd(string s)
        {
            canmsg = ":X1E" + dNN + NIDa + "N";
            canmsg += hex((int)DAA.DAA_NVSET, 2);      // opc
            canmsg += s;                        // NV index and value
            canmsg += ";";
            sendmsg(canmsg);
        }

        private void SendEvReadCmd(String en, int ev)
        {
            canmsg = ":X1E" + dNN + NIDa + "N";
            canmsg += hex((int)DAA.DAA_EVREADH, 2);      // opc
            canmsg += en.Substring(0,8);                       // EV number
            canmsg += ";";
            sendmsg(canmsg);
            canmsg = ":X1E" + dNN + NIDa + "N";
            canmsg += hex((int)DAA.DAA_EVREADL, 2);      // opc
            canmsg += en.Substring(8, 8);                       // EV number
            canmsg += hex(ev, 2);                 // EV index
            canmsg += ";";
            sendmsg(canmsg);
        }

        private void SendEvEraseCmd(String en)
        {
            canmsg = ":X1E" + dNN + NIDa + "N";
            canmsg += hex((int)DAA.DAA_EVERASEH, 2);      // opc
            canmsg += en.Substring(0, 8);                       // EV number
            canmsg += ";";
            sendmsg(canmsg);
            canmsg = ":X1E" + dNN + NIDa + "N";
            canmsg += hex((int)DAA.DAA_EVERASEL, 2);      // opc
            canmsg += en.Substring(8, 8);                       // EV number
            canmsg += ";";
            sendmsg(canmsg);
        }

        private void SendEvWriteCmd(string s)
        {
            canmsg = ":X1E" + dNN + NIDa + "N";
            canmsg += hex((int)DAA.DAA_EVWRITEH, 2);      // opc
            canmsg += s.Substring(0, 8);                    // EV number
            canmsg += ";";
            sendmsg(canmsg);
            canmsg = ":X1E" + dNN + NIDa + "N";
            canmsg += hex((int)DAA.DAA_EVWRITEL, 2);      // opc
            canmsg += s.Substring(8, 8);                    // EV number
            canmsg += hex((s.Length - 16)/2, 2);              // number of bytes
            canmsg += ";";
            sendmsg(canmsg);
            s = s.Substring(16);
            int offset = 0;
            while (s.Length > 0)
            {
                canmsg = ":X1E" + dNN + NIDa + "N";
                canmsg += hex((int)DAA.DAA_DATA, 2);           // opc
                canmsg += hex(offset, 2);                 // offset
                offset += 7;
                canmsg += (s + "000000000000").Substring(0, 14); // 7 bytes of data
                canmsg += ";";
                sendmsg(canmsg);
                if (s.Length < 14)
                    s = "";
                else
                    s = s.Substring(14);
            }
        }

        //**************************************************************************
        // Restore module configuration
        //**************************************************************************

        private void RestoreConfigBtn_Click(object sender, EventArgs e)
        {
            dNN = NNtb.Text.PadLeft(3, '0').Substring(0, 3);
            openFileDialog1.AddExtension = true;
            openFileDialog1.Filter = "Config files|*.cfg|All files|*.*";
            DialogResult res = openFileDialog1.ShowDialog();
            if (res.Equals(DialogResult.OK))
            {
                readfile = new StreamReader(openFileDialog1.FileName);
                string line = readfile.ReadLine(); // skip node number line
                opstate = OPSTATE.CFGWRITE;
                SendEvEraseCmd("0000000000000000");
            }
        }

        private void CfgWrite()
        {
            if (serialLine.StartsWith("ERROR")
              || serialLine.StartsWith("TOO LONG")
                // || serialLine.StartsWith("-ECAN")
              || serialLine.StartsWith("-SERIAL"))
            {
                opstate = OPSTATE.IDLE;
                updatelog("Aborted due to error.");
                displaylog();
            }
            else if (serialLine.Length > 17 && serialLine.StartsWith(":X1E")
                && serialLine.Substring(7, 3).Equals(dNN))
            {
                if (serialLine.Substring(10, 5).Equals("N" + hex((int)DAA.DAA_ACK ,2) + "00")) // OK
                {
                    string line = readfile.ReadLine();
                    if (line.StartsWith("NV"))
                        SendNvWriteCmd(line.Substring(2));
                    else if (line.StartsWith("EV"))
                        SendEvWriteCmd(line.Substring(2));
                    else if (line.StartsWith("End"))
                    {
                        opstate = OPSTATE.IDLE;
                        updatelog("Restored.");
                        displaylog();
                        return;
                    }
                    else
                    {
                        opstate = OPSTATE.IDLE;
                        updatelog("Error in file.");
                        displaylog();
                        return;
                    }
                }
                else if (serialLine.Substring(10, 5).Equals("N" + hex((int)DAA.DAA_ACK ,2) + "04")) // no space
                {
                    opstate = OPSTATE.IDLE;
                    updatelog("Aborted due to no space left.");
                    displaylog();
                }
            }
        }

        //**************************************************************************
        // Read node variable
        //**************************************************************************

        private void NVReadBtn_Click(object sender, EventArgs e)
        {
            int index = Convert.ToInt16(NVindextb.Text);
            if (index < 0 || index > 255)
            {
                NVindextb.Text = "0";
                return;
            }
            opstate = OPSTATE.NVREAD;
            SendNvReadCmd(hex(index, 2));
        }

        private void NVRead()
        {
            if (serialLine.StartsWith("ERROR")
              || serialLine.StartsWith("TOO LONG")
                // || serialLine.StartsWith("-ECAN")
              || serialLine.StartsWith("-SERIAL"))
            {
                opstate = OPSTATE.IDLE;
                updatelog("Aborted due to error.");
                displaylog();
            }
            else if (serialLine.Length > 17 && serialLine.StartsWith(":X1E")
                && serialLine.Substring(7, 3).Equals(dNN))
            {
                if (serialLine.Substring(10, 3).Equals("N97" + hex((int)DAA.DAA_DATA,2))) // data
                {
                    NVvaluetb.Text = serialLine.Substring(15, 2);
                    opstate = OPSTATE.IDLE;
                    displaylog();
                    return;
                }
                else if (serialLine.Substring(10, 3).Equals("N97" + hex((int)DAA.DAA_ACK, 2))) // ack
                {
                    NVvaluetb.Text = "";
                    opstate = OPSTATE.IDLE;
                    displaylog();
                }
            }
        }

        //**************************************************************************
        // Write node variable
        //**************************************************************************

        private void NVwriteBTN_Click(object sender, EventArgs e)
        {
            int index = Convert.ToInt16(NVindextb.Text);
            if (index < 0 || index > 255)
            {
                NVindextb.Text = "0";
                return;
            }
            SendNvWriteCmd(hex(index, 2) + NVvaluetb.Text.PadLeft(2, '0'));
        }

        //**************************************************************************
        // Write event
        //**************************************************************************

        private void EVwriteBTN_Click(object sender, EventArgs e)
        {
            SendEvWriteCmd(EventNumbertb.Text + EventActiontb.Text);
        }

        //**************************************************************************
        // Read event
        //**************************************************************************

        private void EVreadBTN_Click(object sender, EventArgs e)
        {
            int index = Convert.ToInt16(EventIndextb.Text);
            if (index < 0 || index > 255)
            {
                EventIndextb.Text = "0";
                return;
            }
            opstate = OPSTATE.EVREAD;
            SendEvReadCmd(EventNumbertb.Text.PadLeft(16, '0'), index);
        }

        private void EVRead()
        {
            if (serialLine.StartsWith("ERROR")
              || serialLine.StartsWith("TOO LONG")
                // || serialLine.StartsWith("-ECAN")
              || serialLine.StartsWith("-SERIAL"))
            {
                opstate = OPSTATE.IDLE;
                updatelog("Aborted due to error.");
                displaylog();
            }
            else if (serialLine.Length > 17 && serialLine.StartsWith(":X1E")
                && serialLine.Substring(7, 4).Equals(dNN+"N"))
            {
                if (serialLine.Substring(11, 2).Equals(hex((int)DAA.DAA_EVWRITEH, 2))) // EVWRITE
                    EventNumbertb.Text = serialLine.Substring(13, 8) + EventNumbertb.Text.Substring(8, 8);
                else if (serialLine.Substring(11, 2).Equals(hex((int)DAA.DAA_EVWRITEL, 2))) // EVWRITE
                    EventNumbertb.Text = EventNumbertb.Text.Substring(0, 8) + serialLine.Substring(13, 8);
                else if (serialLine.Substring(11, 2).Equals(hex((int)DAA.DAA_DATA, 2))) // EVDATA
                {
                    EventActiontb.Text = serialLine.Substring(13, 14);
                    opstate = OPSTATE.IDLE;
                    displaylog();
                }
                else if (serialLine.Substring(11, 2).Equals(hex((int)DAA.DAA_ACK, 2))) // ack
                {
                    EventNumbertb.Text = "";
                    EventActiontb.Text = "";
                    opstate = OPSTATE.IDLE;
                    displaylog();
                }
            }
        }

        /// <summary>
        ///  Allows you to close the port without closing the application.
        ///  Now you can disconnect or reset the CAN_USB without the 
        ///  application crashing on you since the port was open when you
        ///  removed it.
        ///  
        ///  The CLOSE button will also dissappear, ginving you some idea if the
        ///  port is opened or not.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void closeComPortBtn_Click(object sender, EventArgs e)
        {
            /// only do attempt if the port is open
            if (serialPort1.IsOpen)
                try
                {
                    serialPort1.Close();
                    enableCommButtons(false);
                    /// and don't show the close button when closed
                    closeComPortBtn.Hide();
                }
                catch (IOException)
                {
                    /// maybe warn if something bad happened
                }
        }

        //**************************************************************************
        // Display option checkboxes
        //**************************************************************************

        private void FlashTaskBaropt_CheckedChanged(object sender, EventArgs e)
        {
            optionFlashTaskBar = (FlashTaskBaropt.CheckState == CheckState.Checked);
        }

        private void GetNidsBtn_Click(object sender, EventArgs e)
        {
            sendmsg(":X1"+hex((int)FT.FT_VNSN, 4)+NIDa+"N;");
        }

        private void WriteNidBtn_Click(object sender, EventArgs e)
        {
            int i, j;
            NIDtext.Text = NIDtext.Text.PadLeft(12,'0').Substring(0,12);
            for (i = 0, j = 11; i < 6; i++, j -= 2)
                modulestr[i] = hv((byte)NIDtext.Text[j - 1], (byte)NIDtext.Text[j]);
            for (i = 6; i < 64; i++)
                modulestr[i] = 0xFF;
            SendWriteCmd(0x000040);
            this.WriteUserId.RunWorkerAsync();
        }
    }
}
