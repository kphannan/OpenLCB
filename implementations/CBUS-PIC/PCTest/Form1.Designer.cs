﻿namespace PCTest
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
            this.LogSpace = new System.Windows.Forms.TextBox();
            this.serialPort1 = new System.IO.Ports.SerialPort(this.components);
            this.ComPortBtn = new System.Windows.Forms.Button();
            this.saveLogFileDialog = new System.Windows.Forms.SaveFileDialog();
            this.SaveBtn = new System.Windows.Forms.Button();
            this.SendBtn = new System.Windows.Forms.Button();
            this.SendText = new System.Windows.Forms.TextBox();
            this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.SendUpgrade = new System.ComponentModel.BackgroundWorker();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.EraseEVBtn = new System.Windows.Forms.Button();
            this.DefaultBtn = new System.Windows.Forms.Button();
            this.EraseAllBtn = new System.Windows.Forms.Button();
            this.label8 = new System.Windows.Forms.Label();
            this.BootText = new System.Windows.Forms.TextBox();
            this.progressBar1 = new System.Windows.Forms.ProgressBar();
            this.label7 = new System.Windows.Forms.Label();
            this.NIDtext = new System.Windows.Forms.TextBox();
            this.GetNidsBtn = new System.Windows.Forms.Button();
            this.EVwriteBTN = new System.Windows.Forms.Button();
            this.EVreadBTN = new System.Windows.Forms.Button();
            this.EventActiontb = new System.Windows.Forms.TextBox();
            this.EventNumbertb = new System.Windows.Forms.TextBox();
            this.WriteBtn = new System.Windows.Forms.Button();
            this.ReadBtn = new System.Windows.Forms.Button();
            this.EventIndextb = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.RestoreConfigBtn = new System.Windows.Forms.Button();
            this.SaveConfigBtn = new System.Windows.Forms.Button();
            this.NNtb = new System.Windows.Forms.ComboBox();
            this.label4 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.SoftwareBtn = new System.Windows.Forms.Button();
            this.UserText = new System.Windows.Forms.TextBox();
            this.NodeText = new System.Windows.Forms.TextBox();
            this.WriteUserId = new System.ComponentModel.BackgroundWorker();
            this.closeComPortBtn = new System.Windows.Forms.Button();
            this.KeepLogopt = new System.Windows.Forms.CheckBox();
            this.label9 = new System.Windows.Forms.Label();
            this.PE_index = new System.Windows.Forms.TextBox();
            this.PE_nodeidtxt = new System.Windows.Forms.TextBox();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.rebootbtn = new System.Windows.Forms.Button();
            this.byte6txt = new System.Windows.Forms.TextBox();
            this.byte5txt = new System.Windows.Forms.TextBox();
            this.byte4txt = new System.Windows.Forms.TextBox();
            this.byte3txt = new System.Windows.Forms.TextBox();
            this.byte2txt = new System.Windows.Forms.TextBox();
            this.byte1txt = new System.Windows.Forms.TextBox();
            this.label15 = new System.Windows.Forms.Label();
            this.NIDTxt2 = new System.Windows.Forms.TextBox();
            this.label16 = new System.Windows.Forms.Label();
            this.label18 = new System.Windows.Forms.Label();
            this.membertxt = new System.Windows.Forms.TextBox();
            this.WriteNidBtn = new System.Windows.Forms.Button();
            this.tabPage3 = new System.Windows.Forms.TabPage();
            this.tabPage6 = new System.Windows.Forms.TabPage();
            this.PE_erasebtn = new System.Windows.Forms.Button();
            this.PE_writebtn = new System.Windows.Forms.Button();
            this.PE_readbtn = new System.Windows.Forms.Button();
            this.PE_eventtxt = new System.Windows.Forms.TextBox();
            this.tabPage5 = new System.Windows.Forms.TabPage();
            this.label5 = new System.Windows.Forms.Label();
            this.NVindextb = new System.Windows.Forms.TextBox();
            this.NVvaluetb = new System.Windows.Forms.TextBox();
            this.NVReadBtn = new System.Windows.Forms.Button();
            this.NVwriteBTN = new System.Windows.Forms.Button();
            this.label10 = new System.Windows.Forms.Label();
            this.label11 = new System.Windows.Forms.Label();
            this.label12 = new System.Windows.Forms.Label();
            this.label13 = new System.Windows.Forms.Label();
            this.groupBox1.SuspendLayout();
            this.tabControl1.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.tabPage2.SuspendLayout();
            this.tabPage3.SuspendLayout();
            this.tabPage6.SuspendLayout();
            this.tabPage5.SuspendLayout();
            this.SuspendLayout();
            // 
            // LogSpace
            // 
            this.LogSpace.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.LogSpace.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(128)));
            this.LogSpace.Location = new System.Drawing.Point(448, 14);
            this.LogSpace.Multiline = true;
            this.LogSpace.Name = "LogSpace";
            this.LogSpace.ReadOnly = true;
            this.LogSpace.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.LogSpace.Size = new System.Drawing.Size(216, 358);
            this.LogSpace.TabIndex = 0;
            this.LogSpace.TabStop = false;
            // 
            // serialPort1
            // 
            this.serialPort1.DataReceived += new System.IO.Ports.SerialDataReceivedEventHandler(this.serialPort1_DataReceived);
            // 
            // ComPortBtn
            // 
            this.ComPortBtn.Location = new System.Drawing.Point(12, 12);
            this.ComPortBtn.Name = "ComPortBtn";
            this.ComPortBtn.Size = new System.Drawing.Size(93, 23);
            this.ComPortBtn.TabIndex = 1;
            this.ComPortBtn.Text = "ComPort ...";
            this.ComPortBtn.UseVisualStyleBackColor = true;
            this.ComPortBtn.Click += new System.EventHandler(this.ComPortBtn_Click);
            // 
            // SaveBtn
            // 
            this.SaveBtn.Location = new System.Drawing.Point(327, 351);
            this.SaveBtn.Name = "SaveBtn";
            this.SaveBtn.Size = new System.Drawing.Size(93, 23);
            this.SaveBtn.TabIndex = 3;
            this.SaveBtn.Text = "SaveLog ...";
            this.SaveBtn.UseVisualStyleBackColor = true;
            this.SaveBtn.Click += new System.EventHandler(this.SaveBtn_Click);
            // 
            // SendBtn
            // 
            this.SendBtn.Location = new System.Drawing.Point(315, 13);
            this.SendBtn.Name = "SendBtn";
            this.SendBtn.Size = new System.Drawing.Size(93, 23);
            this.SendBtn.TabIndex = 2;
            this.SendBtn.Text = "Send";
            this.SendBtn.UseVisualStyleBackColor = true;
            this.SendBtn.Click += new System.EventHandler(this.SendBtn_Click);
            // 
            // SendText
            // 
            this.SendText.AcceptsReturn = true;
            this.SendText.Location = new System.Drawing.Point(78, 13);
            this.SendText.Name = "SendText";
            this.SendText.Size = new System.Drawing.Size(231, 20);
            this.SendText.TabIndex = 1;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(6, 16);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(66, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Text to send";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(6, 9);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(55, 13);
            this.label2.TabIndex = 0;
            this.label2.Text = "NodeAlias";
            // 
            // SendUpgrade
            // 
            this.SendUpgrade.WorkerSupportsCancellation = true;
            this.SendUpgrade.DoWork += new System.ComponentModel.DoWorkEventHandler(this.SendUpgrade_DoWork);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.SendText);
            this.groupBox1.Controls.Add(this.SendBtn);
            this.groupBox1.Location = new System.Drawing.Point(12, 287);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(426, 45);
            this.groupBox1.TabIndex = 5;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Send Cmd";
            // 
            // EraseEVBtn
            // 
            this.EraseEVBtn.Location = new System.Drawing.Point(297, 97);
            this.EraseEVBtn.Name = "EraseEVBtn";
            this.EraseEVBtn.Size = new System.Drawing.Size(93, 23);
            this.EraseEVBtn.TabIndex = 32;
            this.EraseEVBtn.Text = "EraseEV";
            this.EraseEVBtn.UseVisualStyleBackColor = true;
            this.EraseEVBtn.Click += new System.EventHandler(this.EraseEVBtn_Click);
            // 
            // DefaultBtn
            // 
            this.DefaultBtn.Location = new System.Drawing.Point(318, 145);
            this.DefaultBtn.Name = "DefaultBtn";
            this.DefaultBtn.Size = new System.Drawing.Size(75, 23);
            this.DefaultBtn.TabIndex = 31;
            this.DefaultBtn.Text = "Set Default";
            this.DefaultBtn.UseVisualStyleBackColor = true;
            this.DefaultBtn.Click += new System.EventHandler(this.DefaultBtn_Click);
            // 
            // EraseAllBtn
            // 
            this.EraseAllBtn.Enabled = false;
            this.EraseAllBtn.Location = new System.Drawing.Point(297, 143);
            this.EraseAllBtn.Name = "EraseAllBtn";
            this.EraseAllBtn.Size = new System.Drawing.Size(93, 23);
            this.EraseAllBtn.TabIndex = 30;
            this.EraseAllBtn.Text = "Erase All Ev\'s";
            this.EraseAllBtn.UseVisualStyleBackColor = true;
            this.EraseAllBtn.Click += new System.EventHandler(this.EraseAllBtn_Click);
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(9, 42);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(45, 13);
            this.label8.TabIndex = 28;
            this.label8.Text = "Boot Str";
            // 
            // BootText
            // 
            this.BootText.Location = new System.Drawing.Point(81, 39);
            this.BootText.Name = "BootText";
            this.BootText.ReadOnly = true;
            this.BootText.Size = new System.Drawing.Size(323, 20);
            this.BootText.TabIndex = 29;
            this.BootText.TabStop = false;
            // 
            // progressBar1
            // 
            this.progressBar1.Location = new System.Drawing.Point(212, 162);
            this.progressBar1.Name = "progressBar1";
            this.progressBar1.Size = new System.Drawing.Size(93, 23);
            this.progressBar1.TabIndex = 27;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(207, 9);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(47, 13);
            this.label7.TabIndex = 25;
            this.label7.Text = "Node ID";
            // 
            // NIDtext
            // 
            this.NIDtext.Location = new System.Drawing.Point(263, 7);
            this.NIDtext.Name = "NIDtext";
            this.NIDtext.ReadOnly = true;
            this.NIDtext.Size = new System.Drawing.Size(122, 20);
            this.NIDtext.TabIndex = 24;
            // 
            // GetNidsBtn
            // 
            this.GetNidsBtn.Enabled = false;
            this.GetNidsBtn.Location = new System.Drawing.Point(163, 12);
            this.GetNidsBtn.Name = "GetNidsBtn";
            this.GetNidsBtn.Size = new System.Drawing.Size(84, 23);
            this.GetNidsBtn.TabIndex = 23;
            this.GetNidsBtn.Text = "Get NID\'s";
            this.GetNidsBtn.UseVisualStyleBackColor = true;
            this.GetNidsBtn.Click += new System.EventHandler(this.GetNidsBtn_Click);
            // 
            // EVwriteBTN
            // 
            this.EVwriteBTN.Location = new System.Drawing.Point(297, 63);
            this.EVwriteBTN.Name = "EVwriteBTN";
            this.EVwriteBTN.Size = new System.Drawing.Size(93, 23);
            this.EVwriteBTN.TabIndex = 18;
            this.EVwriteBTN.Text = "WriteEV";
            this.EVwriteBTN.UseVisualStyleBackColor = true;
            this.EVwriteBTN.Click += new System.EventHandler(this.EVwriteBTN_Click);
            // 
            // EVreadBTN
            // 
            this.EVreadBTN.Location = new System.Drawing.Point(297, 20);
            this.EVreadBTN.Name = "EVreadBTN";
            this.EVreadBTN.Size = new System.Drawing.Size(93, 23);
            this.EVreadBTN.TabIndex = 17;
            this.EVreadBTN.Text = "ReadEV";
            this.EVreadBTN.UseVisualStyleBackColor = true;
            this.EVreadBTN.Click += new System.EventHandler(this.EVreadBTN_Click);
            // 
            // EventActiontb
            // 
            this.EventActiontb.Location = new System.Drawing.Point(107, 97);
            this.EventActiontb.Name = "EventActiontb";
            this.EventActiontb.Size = new System.Drawing.Size(154, 20);
            this.EventActiontb.TabIndex = 16;
            // 
            // EventNumbertb
            // 
            this.EventNumbertb.Location = new System.Drawing.Point(107, 52);
            this.EventNumbertb.Name = "EventNumbertb";
            this.EventNumbertb.Size = new System.Drawing.Size(154, 20);
            this.EventNumbertb.TabIndex = 15;
            // 
            // WriteBtn
            // 
            this.WriteBtn.Location = new System.Drawing.Point(311, 133);
            this.WriteBtn.Name = "WriteBtn";
            this.WriteBtn.Size = new System.Drawing.Size(93, 23);
            this.WriteBtn.TabIndex = 3;
            this.WriteBtn.Text = "Write User Str";
            this.WriteBtn.UseVisualStyleBackColor = true;
            this.WriteBtn.Click += new System.EventHandler(this.WriteBtn_Click);
            // 
            // ReadBtn
            // 
            this.ReadBtn.Location = new System.Drawing.Point(14, 133);
            this.ReadBtn.Name = "ReadBtn";
            this.ReadBtn.Size = new System.Drawing.Size(93, 23);
            this.ReadBtn.TabIndex = 2;
            this.ReadBtn.Text = "Read Info";
            this.ReadBtn.UseVisualStyleBackColor = true;
            this.ReadBtn.Click += new System.EventHandler(this.ReadBtn_Click);
            // 
            // EventIndextb
            // 
            this.EventIndextb.Location = new System.Drawing.Point(107, 17);
            this.EventIndextb.Name = "EventIndextb";
            this.EventIndextb.Size = new System.Drawing.Size(34, 20);
            this.EventIndextb.TabIndex = 14;
            this.EventIndextb.Text = "0";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(35, 20);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(33, 13);
            this.label6.TabIndex = 13;
            this.label6.Text = "Index";
            // 
            // RestoreConfigBtn
            // 
            this.RestoreConfigBtn.Location = new System.Drawing.Point(311, 162);
            this.RestoreConfigBtn.Name = "RestoreConfigBtn";
            this.RestoreConfigBtn.Size = new System.Drawing.Size(93, 23);
            this.RestoreConfigBtn.TabIndex = 22;
            this.RestoreConfigBtn.Text = "Restore Config";
            this.RestoreConfigBtn.UseVisualStyleBackColor = true;
            this.RestoreConfigBtn.Click += new System.EventHandler(this.RestoreConfigBtn_Click);
            // 
            // SaveConfigBtn
            // 
            this.SaveConfigBtn.Location = new System.Drawing.Point(14, 162);
            this.SaveConfigBtn.Name = "SaveConfigBtn";
            this.SaveConfigBtn.Size = new System.Drawing.Size(93, 23);
            this.SaveConfigBtn.TabIndex = 21;
            this.SaveConfigBtn.Text = "Save Config";
            this.SaveConfigBtn.UseVisualStyleBackColor = true;
            this.SaveConfigBtn.Click += new System.EventHandler(this.SaveConfigBtn_Click);
            // 
            // NNtb
            // 
            this.NNtb.FormattingEnabled = true;
            this.NNtb.Location = new System.Drawing.Point(81, 6);
            this.NNtb.Name = "NNtb";
            this.NNtb.Size = new System.Drawing.Size(58, 21);
            this.NNtb.Sorted = true;
            this.NNtb.TabIndex = 1;
            this.NNtb.SelectedIndexChanged += new System.EventHandler(this.NNtb_SelectedIndexChanged);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(9, 95);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(45, 13);
            this.label4.TabIndex = 6;
            this.label4.Text = "User Str";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(9, 68);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(58, 13);
            this.label3.TabIndex = 4;
            this.label3.Text = "Module Str";
            // 
            // SoftwareBtn
            // 
            this.SoftwareBtn.Location = new System.Drawing.Point(113, 162);
            this.SoftwareBtn.Name = "SoftwareBtn";
            this.SoftwareBtn.Size = new System.Drawing.Size(93, 23);
            this.SoftwareBtn.TabIndex = 19;
            this.SoftwareBtn.Text = "Upgrade ...";
            this.SoftwareBtn.UseVisualStyleBackColor = true;
            this.SoftwareBtn.Click += new System.EventHandler(this.SoftwareBtn_Click);
            // 
            // UserText
            // 
            this.UserText.Location = new System.Drawing.Point(81, 92);
            this.UserText.Name = "UserText";
            this.UserText.Size = new System.Drawing.Size(323, 20);
            this.UserText.TabIndex = 7;
            // 
            // NodeText
            // 
            this.NodeText.Location = new System.Drawing.Point(81, 65);
            this.NodeText.Name = "NodeText";
            this.NodeText.ReadOnly = true;
            this.NodeText.Size = new System.Drawing.Size(323, 20);
            this.NodeText.TabIndex = 5;
            this.NodeText.TabStop = false;
            // 
            // WriteUserId
            // 
            this.WriteUserId.WorkerSupportsCancellation = true;
            this.WriteUserId.DoWork += new System.ComponentModel.DoWorkEventHandler(this.WriteUser_DoWork);
            // 
            // closeComPortBtn
            // 
            this.closeComPortBtn.Location = new System.Drawing.Point(111, 12);
            this.closeComPortBtn.Name = "closeComPortBtn";
            this.closeComPortBtn.Size = new System.Drawing.Size(46, 23);
            this.closeComPortBtn.TabIndex = 2;
            this.closeComPortBtn.Text = "Close";
            this.closeComPortBtn.UseVisualStyleBackColor = true;
            this.closeComPortBtn.Visible = false;
            this.closeComPortBtn.Click += new System.EventHandler(this.closeComPortBtn_Click);
            // 
            // KeepLogopt
            // 
            this.KeepLogopt.AutoSize = true;
            this.KeepLogopt.Location = new System.Drawing.Point(28, 355);
            this.KeepLogopt.Name = "KeepLogopt";
            this.KeepLogopt.Size = new System.Drawing.Size(68, 17);
            this.KeepLogopt.TabIndex = 7;
            this.KeepLogopt.Text = "Keep log";
            this.KeepLogopt.UseVisualStyleBackColor = true;
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(8, 30);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(33, 13);
            this.label9.TabIndex = 33;
            this.label9.Text = "Index";
            // 
            // PE_index
            // 
            this.PE_index.Location = new System.Drawing.Point(74, 27);
            this.PE_index.Name = "PE_index";
            this.PE_index.Size = new System.Drawing.Size(67, 20);
            this.PE_index.TabIndex = 34;
            this.PE_index.Text = "0";
            // 
            // PE_nodeidtxt
            // 
            this.PE_nodeidtxt.Location = new System.Drawing.Point(74, 64);
            this.PE_nodeidtxt.Name = "PE_nodeidtxt";
            this.PE_nodeidtxt.Size = new System.Drawing.Size(132, 20);
            this.PE_nodeidtxt.TabIndex = 35;
            this.PE_nodeidtxt.Text = "000000000000";
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Controls.Add(this.tabPage2);
            this.tabControl1.Controls.Add(this.tabPage3);
            this.tabControl1.Controls.Add(this.tabPage6);
            this.tabControl1.Controls.Add(this.tabPage5);
            this.tabControl1.Location = new System.Drawing.Point(12, 50);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(430, 231);
            this.tabControl1.TabIndex = 36;
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.ReadBtn);
            this.tabPage1.Controls.Add(this.RestoreConfigBtn);
            this.tabPage1.Controls.Add(this.SoftwareBtn);
            this.tabPage1.Controls.Add(this.progressBar1);
            this.tabPage1.Controls.Add(this.NNtb);
            this.tabPage1.Controls.Add(this.SaveConfigBtn);
            this.tabPage1.Controls.Add(this.label2);
            this.tabPage1.Controls.Add(this.NIDtext);
            this.tabPage1.Controls.Add(this.label7);
            this.tabPage1.Controls.Add(this.NodeText);
            this.tabPage1.Controls.Add(this.label8);
            this.tabPage1.Controls.Add(this.UserText);
            this.tabPage1.Controls.Add(this.BootText);
            this.tabPage1.Controls.Add(this.label3);
            this.tabPage1.Controls.Add(this.WriteBtn);
            this.tabPage1.Controls.Add(this.label4);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(422, 205);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Node";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.rebootbtn);
            this.tabPage2.Controls.Add(this.byte6txt);
            this.tabPage2.Controls.Add(this.byte5txt);
            this.tabPage2.Controls.Add(this.byte4txt);
            this.tabPage2.Controls.Add(this.byte3txt);
            this.tabPage2.Controls.Add(this.byte2txt);
            this.tabPage2.Controls.Add(this.byte1txt);
            this.tabPage2.Controls.Add(this.label15);
            this.tabPage2.Controls.Add(this.NIDTxt2);
            this.tabPage2.Controls.Add(this.label16);
            this.tabPage2.Controls.Add(this.label18);
            this.tabPage2.Controls.Add(this.membertxt);
            this.tabPage2.Controls.Add(this.WriteNidBtn);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage2.Size = new System.Drawing.Size(422, 205);
            this.tabPage2.TabIndex = 1;
            this.tabPage2.Text = "Node ID";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // rebootbtn
            // 
            this.rebootbtn.Location = new System.Drawing.Point(156, 135);
            this.rebootbtn.Name = "rebootbtn";
            this.rebootbtn.Size = new System.Drawing.Size(75, 23);
            this.rebootbtn.TabIndex = 68;
            this.rebootbtn.Text = "Reboot";
            this.rebootbtn.UseVisualStyleBackColor = true;
            this.rebootbtn.Click += new System.EventHandler(this.rebootbtn_Click);
            // 
            // byte6txt
            // 
            this.byte6txt.Location = new System.Drawing.Point(329, 50);
            this.byte6txt.Name = "byte6txt";
            this.byte6txt.Size = new System.Drawing.Size(45, 20);
            this.byte6txt.TabIndex = 67;
            this.byte6txt.Validating += new System.ComponentModel.CancelEventHandler(this.byte6txt_Validating);
            // 
            // byte5txt
            // 
            this.byte5txt.Location = new System.Drawing.Point(278, 50);
            this.byte5txt.Name = "byte5txt";
            this.byte5txt.Size = new System.Drawing.Size(45, 20);
            this.byte5txt.TabIndex = 66;
            this.byte5txt.Validating += new System.ComponentModel.CancelEventHandler(this.byte5txt_Validating);
            // 
            // byte4txt
            // 
            this.byte4txt.Location = new System.Drawing.Point(227, 50);
            this.byte4txt.Name = "byte4txt";
            this.byte4txt.Size = new System.Drawing.Size(45, 20);
            this.byte4txt.TabIndex = 65;
            this.byte4txt.Validating += new System.ComponentModel.CancelEventHandler(this.byte4txt_Validating);
            // 
            // byte3txt
            // 
            this.byte3txt.Location = new System.Drawing.Point(176, 50);
            this.byte3txt.Name = "byte3txt";
            this.byte3txt.Size = new System.Drawing.Size(45, 20);
            this.byte3txt.TabIndex = 64;
            this.byte3txt.Validating += new System.ComponentModel.CancelEventHandler(this.byte3txt_Validating);
            // 
            // byte2txt
            // 
            this.byte2txt.Location = new System.Drawing.Point(125, 50);
            this.byte2txt.Name = "byte2txt";
            this.byte2txt.Size = new System.Drawing.Size(45, 20);
            this.byte2txt.TabIndex = 63;
            this.byte2txt.Validating += new System.ComponentModel.CancelEventHandler(this.byte2txt_Validating);
            // 
            // byte1txt
            // 
            this.byte1txt.Location = new System.Drawing.Point(74, 50);
            this.byte1txt.Name = "byte1txt";
            this.byte1txt.Size = new System.Drawing.Size(45, 20);
            this.byte1txt.TabIndex = 62;
            this.byte1txt.Validating += new System.ComponentModel.CancelEventHandler(this.byte1txt_Validating);
            // 
            // label15
            // 
            this.label15.AutoSize = true;
            this.label15.Location = new System.Drawing.Point(21, 50);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(27, 13);
            this.label15.TabIndex = 61;
            this.label15.Text = "Dec";
            // 
            // NIDTxt2
            // 
            this.NIDTxt2.Location = new System.Drawing.Point(74, 17);
            this.NIDTxt2.Name = "NIDTxt2";
            this.NIDTxt2.Size = new System.Drawing.Size(301, 20);
            this.NIDTxt2.TabIndex = 59;
            this.NIDTxt2.Validating += new System.ComponentModel.CancelEventHandler(this.NIDTxt2_Validating);
            // 
            // label16
            // 
            this.label16.AutoSize = true;
            this.label16.Location = new System.Drawing.Point(21, 20);
            this.label16.Name = "label16";
            this.label16.Size = new System.Drawing.Size(26, 13);
            this.label16.TabIndex = 60;
            this.label16.Text = "Hex";
            // 
            // label18
            // 
            this.label18.AutoSize = true;
            this.label18.Location = new System.Drawing.Point(71, 85);
            this.label18.Name = "label18";
            this.label18.Size = new System.Drawing.Size(84, 13);
            this.label18.TabIndex = 57;
            this.label18.Text = "Membership No.";
            // 
            // membertxt
            // 
            this.membertxt.Location = new System.Drawing.Point(176, 82);
            this.membertxt.Name = "membertxt";
            this.membertxt.Size = new System.Drawing.Size(147, 20);
            this.membertxt.TabIndex = 54;
            this.membertxt.Validating += new System.ComponentModel.CancelEventHandler(this.membertxt_TextChanged);
            // 
            // WriteNidBtn
            // 
            this.WriteNidBtn.Enabled = false;
            this.WriteNidBtn.Location = new System.Drawing.Point(69, 135);
            this.WriteNidBtn.Name = "WriteNidBtn";
            this.WriteNidBtn.Size = new System.Drawing.Size(72, 23);
            this.WriteNidBtn.TabIndex = 53;
            this.WriteNidBtn.Text = "Write NID";
            this.WriteNidBtn.UseVisualStyleBackColor = true;
            this.WriteNidBtn.Click += new System.EventHandler(this.WriteNidBtn_Click);
            // 
            // tabPage3
            // 
            this.tabPage3.Controls.Add(this.label13);
            this.tabPage3.Controls.Add(this.label12);
            this.tabPage3.Controls.Add(this.label6);
            this.tabPage3.Controls.Add(this.EventIndextb);
            this.tabPage3.Controls.Add(this.EventNumbertb);
            this.tabPage3.Controls.Add(this.EraseEVBtn);
            this.tabPage3.Controls.Add(this.EraseAllBtn);
            this.tabPage3.Controls.Add(this.EventActiontb);
            this.tabPage3.Controls.Add(this.EVreadBTN);
            this.tabPage3.Controls.Add(this.EVwriteBTN);
            this.tabPage3.Location = new System.Drawing.Point(4, 22);
            this.tabPage3.Name = "tabPage3";
            this.tabPage3.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage3.Size = new System.Drawing.Size(422, 205);
            this.tabPage3.TabIndex = 2;
            this.tabPage3.Text = "Consumer Events";
            this.tabPage3.UseVisualStyleBackColor = true;
            // 
            // tabPage6
            // 
            this.tabPage6.Controls.Add(this.label11);
            this.tabPage6.Controls.Add(this.label10);
            this.tabPage6.Controls.Add(this.PE_erasebtn);
            this.tabPage6.Controls.Add(this.PE_writebtn);
            this.tabPage6.Controls.Add(this.PE_readbtn);
            this.tabPage6.Controls.Add(this.PE_eventtxt);
            this.tabPage6.Controls.Add(this.DefaultBtn);
            this.tabPage6.Controls.Add(this.PE_nodeidtxt);
            this.tabPage6.Controls.Add(this.label9);
            this.tabPage6.Controls.Add(this.PE_index);
            this.tabPage6.Location = new System.Drawing.Point(4, 22);
            this.tabPage6.Name = "tabPage6";
            this.tabPage6.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage6.Size = new System.Drawing.Size(422, 205);
            this.tabPage6.TabIndex = 3;
            this.tabPage6.Text = "Producer Events";
            this.tabPage6.UseVisualStyleBackColor = true;
            // 
            // PE_erasebtn
            // 
            this.PE_erasebtn.Location = new System.Drawing.Point(237, 145);
            this.PE_erasebtn.Name = "PE_erasebtn";
            this.PE_erasebtn.Size = new System.Drawing.Size(75, 23);
            this.PE_erasebtn.TabIndex = 40;
            this.PE_erasebtn.Text = "Erase";
            this.PE_erasebtn.UseVisualStyleBackColor = true;
            this.PE_erasebtn.Click += new System.EventHandler(this.PE_erasebtn_Click);
            // 
            // PE_writebtn
            // 
            this.PE_writebtn.Location = new System.Drawing.Point(156, 145);
            this.PE_writebtn.Name = "PE_writebtn";
            this.PE_writebtn.Size = new System.Drawing.Size(75, 23);
            this.PE_writebtn.TabIndex = 39;
            this.PE_writebtn.Text = "Write";
            this.PE_writebtn.UseVisualStyleBackColor = true;
            this.PE_writebtn.Click += new System.EventHandler(this.PE_writebtn_Click);
            // 
            // PE_readbtn
            // 
            this.PE_readbtn.Location = new System.Drawing.Point(74, 145);
            this.PE_readbtn.Name = "PE_readbtn";
            this.PE_readbtn.Size = new System.Drawing.Size(75, 23);
            this.PE_readbtn.TabIndex = 38;
            this.PE_readbtn.Text = "Read";
            this.PE_readbtn.UseVisualStyleBackColor = true;
            this.PE_readbtn.Click += new System.EventHandler(this.PE_readbtn_Click);
            // 
            // PE_eventtxt
            // 
            this.PE_eventtxt.Location = new System.Drawing.Point(74, 90);
            this.PE_eventtxt.Name = "PE_eventtxt";
            this.PE_eventtxt.Size = new System.Drawing.Size(67, 20);
            this.PE_eventtxt.TabIndex = 36;
            this.PE_eventtxt.Text = "0000";
            // 
            // tabPage5
            // 
            this.tabPage5.Controls.Add(this.label5);
            this.tabPage5.Controls.Add(this.NVindextb);
            this.tabPage5.Controls.Add(this.NVvaluetb);
            this.tabPage5.Controls.Add(this.NVReadBtn);
            this.tabPage5.Controls.Add(this.NVwriteBTN);
            this.tabPage5.Location = new System.Drawing.Point(4, 22);
            this.tabPage5.Name = "tabPage5";
            this.tabPage5.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage5.Size = new System.Drawing.Size(422, 205);
            this.tabPage5.TabIndex = 5;
            this.tabPage5.Text = "Node Variables";
            this.tabPage5.UseVisualStyleBackColor = true;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(28, 35);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(71, 13);
            this.label5.TabIndex = 13;
            this.label5.Text = "NodeVariable";
            // 
            // NVindextb
            // 
            this.NVindextb.Location = new System.Drawing.Point(117, 32);
            this.NVindextb.Name = "NVindextb";
            this.NVindextb.Size = new System.Drawing.Size(34, 20);
            this.NVindextb.TabIndex = 14;
            this.NVindextb.Text = "0";
            // 
            // NVvaluetb
            // 
            this.NVvaluetb.Location = new System.Drawing.Point(168, 32);
            this.NVvaluetb.Name = "NVvaluetb";
            this.NVvaluetb.Size = new System.Drawing.Size(41, 20);
            this.NVvaluetb.TabIndex = 15;
            // 
            // NVReadBtn
            // 
            this.NVReadBtn.Location = new System.Drawing.Point(117, 116);
            this.NVReadBtn.Name = "NVReadBtn";
            this.NVReadBtn.Size = new System.Drawing.Size(93, 23);
            this.NVReadBtn.TabIndex = 16;
            this.NVReadBtn.Text = "ReadNV";
            this.NVReadBtn.UseVisualStyleBackColor = true;
            this.NVReadBtn.Click += new System.EventHandler(this.NVReadBtn_Click);
            // 
            // NVwriteBTN
            // 
            this.NVwriteBTN.Location = new System.Drawing.Point(216, 116);
            this.NVwriteBTN.Name = "NVwriteBTN";
            this.NVwriteBTN.Size = new System.Drawing.Size(93, 23);
            this.NVwriteBTN.TabIndex = 17;
            this.NVwriteBTN.Text = "WriteNV";
            this.NVwriteBTN.UseVisualStyleBackColor = true;
            this.NVwriteBTN.Click += new System.EventHandler(this.NVwriteBTN_Click);
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(9, 67);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(65, 13);
            this.label10.TabIndex = 41;
            this.label10.Text = "NodeID part";
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(9, 93);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(54, 13);
            this.label11.TabIndex = 42;
            this.label11.Text = "Index part";
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(38, 59);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(44, 13);
            this.label12.TabIndex = 33;
            this.label12.Text = "NodeID";
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Location = new System.Drawing.Point(38, 97);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(37, 13);
            this.label13.TabIndex = 34;
            this.label13.Text = "Action";
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(676, 385);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.KeepLogopt);
            this.Controls.Add(this.closeComPortBtn);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.SaveBtn);
            this.Controls.Add(this.ComPortBtn);
            this.Controls.Add(this.LogSpace);
            this.Controls.Add(this.GetNidsBtn);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MinimumSize = new System.Drawing.Size(684, 403);
            this.Name = "Form1";
            this.Text = "OpenLCB PCTest 0.1";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.tabControl1.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.tabPage1.PerformLayout();
            this.tabPage2.ResumeLayout(false);
            this.tabPage2.PerformLayout();
            this.tabPage3.ResumeLayout(false);
            this.tabPage3.PerformLayout();
            this.tabPage6.ResumeLayout(false);
            this.tabPage6.PerformLayout();
            this.tabPage5.ResumeLayout(false);
            this.tabPage5.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox LogSpace;
        private System.IO.Ports.SerialPort serialPort1;
        private System.Windows.Forms.Button ComPortBtn;
        private System.Windows.Forms.SaveFileDialog saveLogFileDialog;
        private System.Windows.Forms.Button SaveBtn;
        private System.Windows.Forms.Button SendBtn;
        private System.Windows.Forms.TextBox SendText;
        private System.Windows.Forms.OpenFileDialog openFileDialog1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.ComponentModel.BackgroundWorker SendUpgrade;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.TextBox UserText;
        private System.Windows.Forms.TextBox NodeText;
        private System.Windows.Forms.Button ReadBtn;
        private System.Windows.Forms.Button SoftwareBtn;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Button WriteBtn;
        private System.ComponentModel.BackgroundWorker WriteUserId;
        private System.Windows.Forms.ComboBox NNtb;
        private System.Windows.Forms.Button SaveConfigBtn;
        private System.Windows.Forms.Button RestoreConfigBtn;
        private System.Windows.Forms.Button EVwriteBTN;
        private System.Windows.Forms.Button EVreadBTN;
        private System.Windows.Forms.TextBox EventActiontb;
        private System.Windows.Forms.TextBox EventNumbertb;
        private System.Windows.Forms.TextBox EventIndextb;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Button closeComPortBtn;
        private System.Windows.Forms.CheckBox KeepLogopt;
        private System.Windows.Forms.Button GetNidsBtn;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox NIDtext;
        private System.Windows.Forms.ProgressBar progressBar1;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox BootText;
        private System.Windows.Forms.Button EraseAllBtn;
        private System.Windows.Forms.Button DefaultBtn;
        private System.Windows.Forms.Button EraseEVBtn;
        private System.Windows.Forms.TextBox PE_nodeidtxt;
        private System.Windows.Forms.TextBox PE_index;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.TabPage tabPage2;
        private System.Windows.Forms.TabPage tabPage3;
        private System.Windows.Forms.TabPage tabPage6;
        private System.Windows.Forms.Button PE_readbtn;
        private System.Windows.Forms.TextBox PE_eventtxt;
        private System.Windows.Forms.Button PE_erasebtn;
        private System.Windows.Forms.Button PE_writebtn;
        private System.Windows.Forms.TextBox byte6txt;
        private System.Windows.Forms.TextBox byte5txt;
        private System.Windows.Forms.TextBox byte4txt;
        private System.Windows.Forms.TextBox byte3txt;
        private System.Windows.Forms.TextBox byte2txt;
        private System.Windows.Forms.TextBox byte1txt;
        private System.Windows.Forms.Label label15;
        private System.Windows.Forms.TextBox NIDTxt2;
        private System.Windows.Forms.Label label16;
        private System.Windows.Forms.Label label18;
        private System.Windows.Forms.TextBox membertxt;
        private System.Windows.Forms.Button WriteNidBtn;
        private System.Windows.Forms.TabPage tabPage5;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.TextBox NVindextb;
        private System.Windows.Forms.TextBox NVvaluetb;
        private System.Windows.Forms.Button NVReadBtn;
        private System.Windows.Forms.Button NVwriteBTN;
        private System.Windows.Forms.Button rebootbtn;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.Label label12;

    }
}

