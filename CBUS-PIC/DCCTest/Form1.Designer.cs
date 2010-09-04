namespace ProgramOnMain
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
            this.label1 = new System.Windows.Forms.Label();
            this.LocoTB = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.SpeedTB = new System.Windows.Forms.NumericUpDown();
            this.label3 = new System.Windows.Forms.Label();
            this.CVTB = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.ValueTB = new System.Windows.Forms.TextBox();
            this.WriteBtn = new System.Windows.Forms.Button();
            this.StopBtn = new System.Windows.Forms.Button();
            this.AllocBtn = new System.Windows.Forms.Button();
            this.InTB = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.StartTB = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.AccelTB = new System.Windows.Forms.TextBox();
            this.DecelTB = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.ChangeBt = new System.Windows.Forms.Button();
            this.NewAdrTB = new System.Windows.Forms.TextBox();
            this.label8 = new System.Windows.Forms.Label();
            this.label9 = new System.Windows.Forms.Label();
            this.cv5TB = new System.Windows.Forms.TextBox();
            this.label10 = new System.Windows.Forms.Label();
            this.cv6TB = new System.Windows.Forms.TextBox();
            this.label11 = new System.Windows.Forms.Label();
            this.cv19TB = new System.Windows.Forms.TextBox();
            this.label12 = new System.Windows.Forms.Label();
            this.OpenBtn = new System.Windows.Forms.Button();
            this.comPortTb = new System.Windows.Forms.ComboBox();
            this.trackBar1 = new System.Windows.Forms.TrackBar();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.label13 = new System.Windows.Forms.Label();
            this.label14 = new System.Windows.Forms.Label();
            this.F0cb = new System.Windows.Forms.CheckBox();
            this.F1cb = new System.Windows.Forms.CheckBox();
            this.F2cb = new System.Windows.Forms.CheckBox();
            this.F3cb = new System.Windows.Forms.CheckBox();
            this.F6cb = new System.Windows.Forms.CheckBox();
            this.F5cb = new System.Windows.Forms.CheckBox();
            this.F4cb = new System.Windows.Forms.CheckBox();
            this.F7cb = new System.Windows.Forms.CheckBox();
            this.F11cb = new System.Windows.Forms.CheckBox();
            this.F10cb = new System.Windows.Forms.CheckBox();
            this.F9cb = new System.Windows.Forms.CheckBox();
            this.F8cb = new System.Windows.Forms.CheckBox();
            this.backgroundWorker1 = new System.ComponentModel.BackgroundWorker();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.F20cb = new System.Windows.Forms.CheckBox();
            this.F18cb = new System.Windows.Forms.CheckBox();
            this.F19cb = new System.Windows.Forms.CheckBox();
            this.F17cb = new System.Windows.Forms.CheckBox();
            this.F15cb = new System.Windows.Forms.CheckBox();
            this.F16cb = new System.Windows.Forms.CheckBox();
            this.F14cb = new System.Windows.Forms.CheckBox();
            this.F12cb = new System.Windows.Forms.CheckBox();
            this.F13cb = new System.Windows.Forms.CheckBox();
            this.F26cb = new System.Windows.Forms.CheckBox();
            this.F27cb = new System.Windows.Forms.CheckBox();
            this.F25cb = new System.Windows.Forms.CheckBox();
            this.F24cb = new System.Windows.Forms.CheckBox();
            this.F22cb = new System.Windows.Forms.CheckBox();
            this.F23cb = new System.Windows.Forms.CheckBox();
            this.F21cb = new System.Windows.Forms.CheckBox();
            this.label15 = new System.Windows.Forms.Label();
            this.label16 = new System.Windows.Forms.Label();
            this.cv15tb = new System.Windows.Forms.TextBox();
            ((System.ComponentModel.ISupportInitialize)(this.SpeedTB)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.trackBar1)).BeginInit();
            this.groupBox2.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(22, 50);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(72, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Loco Address";
            // 
            // LocoTB
            // 
            this.LocoTB.Location = new System.Drawing.Point(134, 47);
            this.LocoTB.Name = "LocoTB";
            this.LocoTB.Size = new System.Drawing.Size(100, 20);
            this.LocoTB.TabIndex = 1;
            this.LocoTB.Text = "3";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(22, 114);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(38, 13);
            this.label2.TabIndex = 2;
            this.label2.Text = "Speed";
            // 
            // SpeedTB
            // 
            this.SpeedTB.Location = new System.Drawing.Point(134, 112);
            this.SpeedTB.Maximum = new decimal(new int[] {
            127,
            0,
            0,
            0});
            this.SpeedTB.Minimum = new decimal(new int[] {
            127,
            0,
            0,
            -2147483648});
            this.SpeedTB.Name = "SpeedTB";
            this.SpeedTB.Size = new System.Drawing.Size(100, 20);
            this.SpeedTB.TabIndex = 3;
            this.SpeedTB.ValueChanged += new System.EventHandler(this.SpeedTB_ValueChanged);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(13, 207);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(61, 13);
            this.label3.TabIndex = 4;
            this.label3.Tag = "";
            this.label3.Text = "CV Number";
            // 
            // CVTB
            // 
            this.CVTB.Location = new System.Drawing.Point(135, 201);
            this.CVTB.Name = "CVTB";
            this.CVTB.Size = new System.Drawing.Size(100, 20);
            this.CVTB.TabIndex = 5;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(13, 231);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(34, 13);
            this.label4.TabIndex = 6;
            this.label4.Text = "Value";
            // 
            // ValueTB
            // 
            this.ValueTB.Location = new System.Drawing.Point(135, 228);
            this.ValueTB.Name = "ValueTB";
            this.ValueTB.Size = new System.Drawing.Size(100, 20);
            this.ValueTB.TabIndex = 7;
            this.ValueTB.TextChanged += new System.EventHandler(this.ValueTB_TextChanged);
            // 
            // WriteBtn
            // 
            this.WriteBtn.Location = new System.Drawing.Point(135, 254);
            this.WriteBtn.Name = "WriteBtn";
            this.WriteBtn.Size = new System.Drawing.Size(75, 23);
            this.WriteBtn.TabIndex = 8;
            this.WriteBtn.Text = "Write CV\'s";
            this.WriteBtn.UseVisualStyleBackColor = true;
            this.WriteBtn.Click += new System.EventHandler(this.WriteBtn_Click);
            // 
            // StopBtn
            // 
            this.StopBtn.Location = new System.Drawing.Point(264, 109);
            this.StopBtn.Name = "StopBtn";
            this.StopBtn.Size = new System.Drawing.Size(75, 23);
            this.StopBtn.TabIndex = 9;
            this.StopBtn.Text = "Stop";
            this.StopBtn.UseVisualStyleBackColor = true;
            this.StopBtn.Click += new System.EventHandler(this.StopBtn_Click);
            // 
            // AllocBtn
            // 
            this.AllocBtn.Location = new System.Drawing.Point(264, 45);
            this.AllocBtn.Name = "AllocBtn";
            this.AllocBtn.Size = new System.Drawing.Size(75, 23);
            this.AllocBtn.TabIndex = 10;
            this.AllocBtn.Text = "Allocate";
            this.AllocBtn.UseVisualStyleBackColor = true;
            this.AllocBtn.Click += new System.EventHandler(this.AllocBtn_Click);
            // 
            // InTB
            // 
            this.InTB.Location = new System.Drawing.Point(24, 324);
            this.InTB.Multiline = true;
            this.InTB.Name = "InTB";
            this.InTB.ReadOnly = true;
            this.InTB.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.InTB.Size = new System.Drawing.Size(596, 75);
            this.InTB.TabIndex = 12;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(13, 22);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(64, 13);
            this.label5.TabIndex = 13;
            this.label5.Text = "Vstart CV#2";
            // 
            // StartTB
            // 
            this.StartTB.Location = new System.Drawing.Point(135, 19);
            this.StartTB.Name = "StartTB";
            this.StartTB.Size = new System.Drawing.Size(100, 20);
            this.StartTB.TabIndex = 14;
            this.StartTB.TextChanged += new System.EventHandler(this.StartTB_TextChanged);
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(13, 48);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(90, 13);
            this.label6.TabIndex = 15;
            this.label6.Text = "Accel Rate CV#3";
            // 
            // AccelTB
            // 
            this.AccelTB.Location = new System.Drawing.Point(135, 45);
            this.AccelTB.Name = "AccelTB";
            this.AccelTB.Size = new System.Drawing.Size(100, 20);
            this.AccelTB.TabIndex = 16;
            this.AccelTB.TextChanged += new System.EventHandler(this.AccelTB_TextChanged);
            // 
            // DecelTB
            // 
            this.DecelTB.Location = new System.Drawing.Point(135, 71);
            this.DecelTB.Name = "DecelTB";
            this.DecelTB.Size = new System.Drawing.Size(100, 20);
            this.DecelTB.TabIndex = 17;
            this.DecelTB.TextChanged += new System.EventHandler(this.DecelTB_TextChanged);
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(12, 74);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(91, 13);
            this.label7.TabIndex = 18;
            this.label7.Text = "Decel Rate CV#4";
            // 
            // ChangeBt
            // 
            this.ChangeBt.Location = new System.Drawing.Point(264, 74);
            this.ChangeBt.Name = "ChangeBt";
            this.ChangeBt.Size = new System.Drawing.Size(75, 23);
            this.ChangeBt.TabIndex = 21;
            this.ChangeBt.Text = "Change";
            this.ChangeBt.UseVisualStyleBackColor = true;
            this.ChangeBt.Click += new System.EventHandler(this.ChangeBt_Click);
            // 
            // NewAdrTB
            // 
            this.NewAdrTB.Location = new System.Drawing.Point(134, 76);
            this.NewAdrTB.Name = "NewAdrTB";
            this.NewAdrTB.Size = new System.Drawing.Size(100, 20);
            this.NewAdrTB.TabIndex = 20;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(21, 79);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(70, 13);
            this.label8.TabIndex = 19;
            this.label8.Text = "New Address";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(13, 100);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(64, 13);
            this.label9.TabIndex = 23;
            this.label9.Text = "Vhigh CV#5";
            // 
            // cv5TB
            // 
            this.cv5TB.Location = new System.Drawing.Point(135, 97);
            this.cv5TB.Name = "cv5TB";
            this.cv5TB.Size = new System.Drawing.Size(100, 20);
            this.cv5TB.TabIndex = 22;
            this.cv5TB.TextChanged += new System.EventHandler(this.cv5TB_TextChanged);
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(13, 126);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(60, 13);
            this.label10.TabIndex = 25;
            this.label10.Text = "Vmid CV#6";
            // 
            // cv6TB
            // 
            this.cv6TB.Location = new System.Drawing.Point(135, 123);
            this.cv6TB.Name = "cv6TB";
            this.cv6TB.Size = new System.Drawing.Size(100, 20);
            this.cv6TB.TabIndex = 24;
            this.cv6TB.TextChanged += new System.EventHandler(this.cv6TB_TextChanged);
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(13, 178);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(96, 13);
            this.label11.TabIndex = 27;
            this.label11.Text = "Consist Adr CV#19";
            // 
            // cv19TB
            // 
            this.cv19TB.Location = new System.Drawing.Point(135, 175);
            this.cv19TB.Name = "cv19TB";
            this.cv19TB.Size = new System.Drawing.Size(100, 20);
            this.cv19TB.TabIndex = 26;
            this.cv19TB.TextChanged += new System.EventHandler(this.cv19TB_TextChanged);
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(22, 19);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(50, 13);
            this.label12.TabIndex = 28;
            this.label12.Text = "Com Port";
            // 
            // OpenBtn
            // 
            this.OpenBtn.Location = new System.Drawing.Point(264, 14);
            this.OpenBtn.Name = "OpenBtn";
            this.OpenBtn.Size = new System.Drawing.Size(75, 23);
            this.OpenBtn.TabIndex = 30;
            this.OpenBtn.Text = "Open";
            this.OpenBtn.UseVisualStyleBackColor = true;
            this.OpenBtn.Click += new System.EventHandler(this.OpenBtn_Click);
            // 
            // comPortTb
            // 
            this.comPortTb.FormattingEnabled = true;
            this.comPortTb.Location = new System.Drawing.Point(134, 15);
            this.comPortTb.Name = "comPortTb";
            this.comPortTb.Size = new System.Drawing.Size(100, 21);
            this.comPortTb.TabIndex = 31;
            // 
            // trackBar1
            // 
            this.trackBar1.Location = new System.Drawing.Point(24, 138);
            this.trackBar1.Maximum = 130;
            this.trackBar1.Minimum = -130;
            this.trackBar1.Name = "trackBar1";
            this.trackBar1.Size = new System.Drawing.Size(318, 45);
            this.trackBar1.TabIndex = 32;
            this.trackBar1.TickFrequency = 10;
            this.trackBar1.Scroll += new System.EventHandler(this.trackBar1_Scroll);
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.cv15tb);
            this.groupBox2.Controls.Add(this.label16);
            this.groupBox2.Controls.Add(this.AccelTB);
            this.groupBox2.Controls.Add(this.label3);
            this.groupBox2.Controls.Add(this.CVTB);
            this.groupBox2.Controls.Add(this.label4);
            this.groupBox2.Controls.Add(this.label11);
            this.groupBox2.Controls.Add(this.ValueTB);
            this.groupBox2.Controls.Add(this.cv19TB);
            this.groupBox2.Controls.Add(this.WriteBtn);
            this.groupBox2.Controls.Add(this.label10);
            this.groupBox2.Controls.Add(this.label5);
            this.groupBox2.Controls.Add(this.cv6TB);
            this.groupBox2.Controls.Add(this.StartTB);
            this.groupBox2.Controls.Add(this.label9);
            this.groupBox2.Controls.Add(this.label6);
            this.groupBox2.Controls.Add(this.cv5TB);
            this.groupBox2.Controls.Add(this.DecelTB);
            this.groupBox2.Controls.Add(this.label7);
            this.groupBox2.Location = new System.Drawing.Point(368, 12);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(252, 297);
            this.groupBox2.TabIndex = 33;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "CV\'s";
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Location = new System.Drawing.Point(32, 170);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(27, 13);
            this.label13.TabIndex = 34;
            this.label13.Text = "Rev";
            // 
            // label14
            // 
            this.label14.AutoSize = true;
            this.label14.Location = new System.Drawing.Point(312, 170);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(27, 13);
            this.label14.TabIndex = 35;
            this.label14.Text = "Fwd";
            // 
            // F0cb
            // 
            this.F0cb.AutoSize = true;
            this.F0cb.Location = new System.Drawing.Point(12, 21);
            this.F0cb.Name = "F0cb";
            this.F0cb.Size = new System.Drawing.Size(38, 17);
            this.F0cb.TabIndex = 36;
            this.F0cb.Text = "F0";
            this.F0cb.UseVisualStyleBackColor = true;
            this.F0cb.CheckedChanged += new System.EventHandler(this.F0cb_CheckedChanged);
            // 
            // F1cb
            // 
            this.F1cb.AutoSize = true;
            this.F1cb.Location = new System.Drawing.Point(12, 44);
            this.F1cb.Name = "F1cb";
            this.F1cb.Size = new System.Drawing.Size(38, 17);
            this.F1cb.TabIndex = 37;
            this.F1cb.Text = "F1";
            this.F1cb.UseVisualStyleBackColor = true;
            this.F1cb.CheckedChanged += new System.EventHandler(this.F1cb_CheckedChanged);
            // 
            // F2cb
            // 
            this.F2cb.AutoSize = true;
            this.F2cb.Location = new System.Drawing.Point(12, 67);
            this.F2cb.Name = "F2cb";
            this.F2cb.Size = new System.Drawing.Size(38, 17);
            this.F2cb.TabIndex = 38;
            this.F2cb.Text = "F2";
            this.F2cb.UseVisualStyleBackColor = true;
            this.F2cb.CheckedChanged += new System.EventHandler(this.F2cb_CheckedChanged);
            // 
            // F3cb
            // 
            this.F3cb.AutoSize = true;
            this.F3cb.Location = new System.Drawing.Point(12, 90);
            this.F3cb.Name = "F3cb";
            this.F3cb.Size = new System.Drawing.Size(38, 17);
            this.F3cb.TabIndex = 39;
            this.F3cb.Text = "F3";
            this.F3cb.UseVisualStyleBackColor = true;
            this.F3cb.CheckedChanged += new System.EventHandler(this.F3cb_CheckedChanged);
            // 
            // F6cb
            // 
            this.F6cb.AutoSize = true;
            this.F6cb.Location = new System.Drawing.Point(55, 67);
            this.F6cb.Name = "F6cb";
            this.F6cb.Size = new System.Drawing.Size(38, 17);
            this.F6cb.TabIndex = 42;
            this.F6cb.Text = "F6";
            this.F6cb.UseVisualStyleBackColor = true;
            this.F6cb.CheckedChanged += new System.EventHandler(this.F6cb_CheckedChanged);
            // 
            // F5cb
            // 
            this.F5cb.AutoSize = true;
            this.F5cb.Location = new System.Drawing.Point(55, 44);
            this.F5cb.Name = "F5cb";
            this.F5cb.Size = new System.Drawing.Size(38, 17);
            this.F5cb.TabIndex = 41;
            this.F5cb.Text = "F5";
            this.F5cb.UseVisualStyleBackColor = true;
            this.F5cb.CheckedChanged += new System.EventHandler(this.F5cb_CheckedChanged);
            // 
            // F4cb
            // 
            this.F4cb.AutoSize = true;
            this.F4cb.Location = new System.Drawing.Point(55, 21);
            this.F4cb.Name = "F4cb";
            this.F4cb.Size = new System.Drawing.Size(38, 17);
            this.F4cb.TabIndex = 40;
            this.F4cb.Text = "F4";
            this.F4cb.UseVisualStyleBackColor = true;
            this.F4cb.CheckedChanged += new System.EventHandler(this.F4cb_CheckedChanged);
            // 
            // F7cb
            // 
            this.F7cb.AutoSize = true;
            this.F7cb.Location = new System.Drawing.Point(55, 90);
            this.F7cb.Name = "F7cb";
            this.F7cb.Size = new System.Drawing.Size(38, 17);
            this.F7cb.TabIndex = 43;
            this.F7cb.Text = "F7";
            this.F7cb.UseVisualStyleBackColor = true;
            this.F7cb.CheckedChanged += new System.EventHandler(this.F7cb_CheckedChanged);
            // 
            // F11cb
            // 
            this.F11cb.AutoSize = true;
            this.F11cb.Location = new System.Drawing.Point(98, 90);
            this.F11cb.Name = "F11cb";
            this.F11cb.Size = new System.Drawing.Size(44, 17);
            this.F11cb.TabIndex = 47;
            this.F11cb.Text = "F11";
            this.F11cb.UseVisualStyleBackColor = true;
            this.F11cb.CheckedChanged += new System.EventHandler(this.F11cb_CheckedChanged);
            // 
            // F10cb
            // 
            this.F10cb.AutoSize = true;
            this.F10cb.Location = new System.Drawing.Point(98, 67);
            this.F10cb.Name = "F10cb";
            this.F10cb.Size = new System.Drawing.Size(44, 17);
            this.F10cb.TabIndex = 46;
            this.F10cb.Text = "F10";
            this.F10cb.UseVisualStyleBackColor = true;
            this.F10cb.CheckedChanged += new System.EventHandler(this.F10cb_CheckedChanged);
            // 
            // F9cb
            // 
            this.F9cb.AutoSize = true;
            this.F9cb.Location = new System.Drawing.Point(98, 44);
            this.F9cb.Name = "F9cb";
            this.F9cb.Size = new System.Drawing.Size(38, 17);
            this.F9cb.TabIndex = 45;
            this.F9cb.Text = "F9";
            this.F9cb.UseVisualStyleBackColor = true;
            this.F9cb.CheckedChanged += new System.EventHandler(this.F9cb_CheckedChanged);
            // 
            // F8cb
            // 
            this.F8cb.AutoSize = true;
            this.F8cb.Location = new System.Drawing.Point(98, 21);
            this.F8cb.Name = "F8cb";
            this.F8cb.Size = new System.Drawing.Size(38, 17);
            this.F8cb.TabIndex = 44;
            this.F8cb.Text = "F8";
            this.F8cb.UseVisualStyleBackColor = true;
            this.F8cb.CheckedChanged += new System.EventHandler(this.F8cb_CheckedChanged);
            // 
            // backgroundWorker1
            // 
            this.backgroundWorker1.DoWork += new System.ComponentModel.DoWorkEventHandler(this.backgroundWorker1_DoWork);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.F20cb);
            this.groupBox1.Controls.Add(this.F18cb);
            this.groupBox1.Controls.Add(this.F19cb);
            this.groupBox1.Controls.Add(this.F17cb);
            this.groupBox1.Controls.Add(this.F15cb);
            this.groupBox1.Controls.Add(this.F16cb);
            this.groupBox1.Controls.Add(this.F14cb);
            this.groupBox1.Controls.Add(this.F12cb);
            this.groupBox1.Controls.Add(this.F13cb);
            this.groupBox1.Controls.Add(this.F2cb);
            this.groupBox1.Controls.Add(this.F3cb);
            this.groupBox1.Controls.Add(this.F1cb);
            this.groupBox1.Controls.Add(this.F0cb);
            this.groupBox1.Controls.Add(this.F5cb);
            this.groupBox1.Controls.Add(this.F7cb);
            this.groupBox1.Controls.Add(this.F4cb);
            this.groupBox1.Controls.Add(this.F8cb);
            this.groupBox1.Controls.Add(this.F6cb);
            this.groupBox1.Controls.Add(this.F11cb);
            this.groupBox1.Controls.Add(this.F9cb);
            this.groupBox1.Controls.Add(this.F10cb);
            this.groupBox1.Controls.Add(this.F26cb);
            this.groupBox1.Controls.Add(this.F27cb);
            this.groupBox1.Controls.Add(this.F25cb);
            this.groupBox1.Controls.Add(this.F24cb);
            this.groupBox1.Controls.Add(this.F22cb);
            this.groupBox1.Controls.Add(this.F23cb);
            this.groupBox1.Controls.Add(this.F21cb);
            this.groupBox1.Location = new System.Drawing.Point(12, 193);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(341, 116);
            this.groupBox1.TabIndex = 48;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "FN\'s";
            // 
            // F20cb
            // 
            this.F20cb.AutoSize = true;
            this.F20cb.Location = new System.Drawing.Point(239, 21);
            this.F20cb.Name = "F20cb";
            this.F20cb.Size = new System.Drawing.Size(44, 17);
            this.F20cb.TabIndex = 56;
            this.F20cb.Text = "F20";
            this.F20cb.UseVisualStyleBackColor = true;
            this.F20cb.CheckedChanged += new System.EventHandler(this.F20cb_CheckedChanged);
            // 
            // F18cb
            // 
            this.F18cb.AutoSize = true;
            this.F18cb.Location = new System.Drawing.Point(191, 67);
            this.F18cb.Name = "F18cb";
            this.F18cb.Size = new System.Drawing.Size(44, 17);
            this.F18cb.TabIndex = 54;
            this.F18cb.Text = "F18";
            this.F18cb.UseVisualStyleBackColor = true;
            this.F18cb.CheckedChanged += new System.EventHandler(this.F18cb_CheckedChanged);
            // 
            // F19cb
            // 
            this.F19cb.AutoSize = true;
            this.F19cb.Location = new System.Drawing.Point(191, 90);
            this.F19cb.Name = "F19cb";
            this.F19cb.Size = new System.Drawing.Size(44, 17);
            this.F19cb.TabIndex = 55;
            this.F19cb.Text = "F19";
            this.F19cb.UseVisualStyleBackColor = true;
            this.F19cb.CheckedChanged += new System.EventHandler(this.F19cb_CheckedChanged);
            // 
            // F17cb
            // 
            this.F17cb.AutoSize = true;
            this.F17cb.Location = new System.Drawing.Point(190, 44);
            this.F17cb.Name = "F17cb";
            this.F17cb.Size = new System.Drawing.Size(44, 17);
            this.F17cb.TabIndex = 53;
            this.F17cb.Text = "F17";
            this.F17cb.UseVisualStyleBackColor = true;
            this.F17cb.CheckedChanged += new System.EventHandler(this.F17cb_CheckedChanged);
            // 
            // F15cb
            // 
            this.F15cb.AutoSize = true;
            this.F15cb.Location = new System.Drawing.Point(141, 90);
            this.F15cb.Name = "F15cb";
            this.F15cb.Size = new System.Drawing.Size(44, 17);
            this.F15cb.TabIndex = 51;
            this.F15cb.Text = "F15";
            this.F15cb.UseVisualStyleBackColor = true;
            this.F15cb.CheckedChanged += new System.EventHandler(this.F15cb_CheckedChanged);
            // 
            // F16cb
            // 
            this.F16cb.AutoSize = true;
            this.F16cb.Location = new System.Drawing.Point(190, 21);
            this.F16cb.Name = "F16cb";
            this.F16cb.Size = new System.Drawing.Size(44, 17);
            this.F16cb.TabIndex = 52;
            this.F16cb.Text = "F16";
            this.F16cb.UseVisualStyleBackColor = true;
            this.F16cb.CheckedChanged += new System.EventHandler(this.F16cb_CheckedChanged);
            // 
            // F14cb
            // 
            this.F14cb.AutoSize = true;
            this.F14cb.Location = new System.Drawing.Point(141, 67);
            this.F14cb.Name = "F14cb";
            this.F14cb.Size = new System.Drawing.Size(44, 17);
            this.F14cb.TabIndex = 50;
            this.F14cb.Text = "F14";
            this.F14cb.UseVisualStyleBackColor = true;
            this.F14cb.CheckedChanged += new System.EventHandler(this.F14cb_CheckedChanged);
            // 
            // F12cb
            // 
            this.F12cb.AutoSize = true;
            this.F12cb.Location = new System.Drawing.Point(141, 21);
            this.F12cb.Name = "F12cb";
            this.F12cb.Size = new System.Drawing.Size(44, 17);
            this.F12cb.TabIndex = 48;
            this.F12cb.Text = "F12";
            this.F12cb.UseVisualStyleBackColor = true;
            this.F12cb.CheckedChanged += new System.EventHandler(this.F12cb_CheckedChanged);
            // 
            // F13cb
            // 
            this.F13cb.AutoSize = true;
            this.F13cb.Location = new System.Drawing.Point(141, 44);
            this.F13cb.Name = "F13cb";
            this.F13cb.Size = new System.Drawing.Size(44, 17);
            this.F13cb.TabIndex = 49;
            this.F13cb.Text = "F13";
            this.F13cb.UseVisualStyleBackColor = true;
            this.F13cb.CheckedChanged += new System.EventHandler(this.F13cb_CheckedChanged);
            // 
            // F26cb
            // 
            this.F26cb.AutoSize = true;
            this.F26cb.Location = new System.Drawing.Point(288, 67);
            this.F26cb.Name = "F26cb";
            this.F26cb.Size = new System.Drawing.Size(44, 17);
            this.F26cb.TabIndex = 62;
            this.F26cb.Text = "F26";
            this.F26cb.UseVisualStyleBackColor = true;
            this.F26cb.CheckedChanged += new System.EventHandler(this.F26cb_CheckedChanged);
            // 
            // F27cb
            // 
            this.F27cb.AutoSize = true;
            this.F27cb.Location = new System.Drawing.Point(288, 90);
            this.F27cb.Name = "F27cb";
            this.F27cb.Size = new System.Drawing.Size(44, 17);
            this.F27cb.TabIndex = 63;
            this.F27cb.Text = "F27";
            this.F27cb.UseVisualStyleBackColor = true;
            this.F27cb.CheckedChanged += new System.EventHandler(this.F27cb_CheckedChanged);
            // 
            // F25cb
            // 
            this.F25cb.AutoSize = true;
            this.F25cb.Location = new System.Drawing.Point(288, 44);
            this.F25cb.Name = "F25cb";
            this.F25cb.Size = new System.Drawing.Size(44, 17);
            this.F25cb.TabIndex = 61;
            this.F25cb.Text = "F25";
            this.F25cb.UseVisualStyleBackColor = true;
            this.F25cb.CheckedChanged += new System.EventHandler(this.F25cb_CheckedChanged);
            // 
            // F24cb
            // 
            this.F24cb.AutoSize = true;
            this.F24cb.Location = new System.Drawing.Point(288, 21);
            this.F24cb.Name = "F24cb";
            this.F24cb.Size = new System.Drawing.Size(44, 17);
            this.F24cb.TabIndex = 60;
            this.F24cb.Text = "F24";
            this.F24cb.UseVisualStyleBackColor = true;
            this.F24cb.CheckedChanged += new System.EventHandler(this.F24cb_CheckedChanged);
            // 
            // F22cb
            // 
            this.F22cb.AutoSize = true;
            this.F22cb.Location = new System.Drawing.Point(239, 67);
            this.F22cb.Name = "F22cb";
            this.F22cb.Size = new System.Drawing.Size(44, 17);
            this.F22cb.TabIndex = 58;
            this.F22cb.Text = "F22";
            this.F22cb.UseVisualStyleBackColor = true;
            this.F22cb.CheckedChanged += new System.EventHandler(this.F22cb_CheckedChanged);
            // 
            // F23cb
            // 
            this.F23cb.AutoSize = true;
            this.F23cb.Location = new System.Drawing.Point(239, 90);
            this.F23cb.Name = "F23cb";
            this.F23cb.Size = new System.Drawing.Size(44, 17);
            this.F23cb.TabIndex = 59;
            this.F23cb.Text = "F23";
            this.F23cb.UseVisualStyleBackColor = true;
            this.F23cb.CheckedChanged += new System.EventHandler(this.F23cb_CheckedChanged);
            // 
            // F21cb
            // 
            this.F21cb.AutoSize = true;
            this.F21cb.Location = new System.Drawing.Point(239, 44);
            this.F21cb.Name = "F21cb";
            this.F21cb.Size = new System.Drawing.Size(44, 17);
            this.F21cb.TabIndex = 57;
            this.F21cb.Text = "F21";
            this.F21cb.UseVisualStyleBackColor = true;
            this.F21cb.CheckedChanged += new System.EventHandler(this.F21cb_CheckedChanged);
            // 
            // label15
            // 
            this.label15.AutoSize = true;
            this.label15.Location = new System.Drawing.Point(168, 170);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(29, 13);
            this.label15.TabIndex = 49;
            this.label15.Text = "Stop";
            // 
            // label16
            // 
            this.label16.AutoSize = true;
            this.label16.Location = new System.Drawing.Point(13, 152);
            this.label16.Name = "label16";
            this.label16.Size = new System.Drawing.Size(107, 13);
            this.label16.TabIndex = 28;
            this.label16.Text = "Decoder lock CV#15";
            // 
            // cv15tb
            // 
            this.cv15tb.Location = new System.Drawing.Point(135, 149);
            this.cv15tb.Name = "cv15tb";
            this.cv15tb.Size = new System.Drawing.Size(100, 20);
            this.cv15tb.TabIndex = 29;
            this.cv15tb.TextChanged += new System.EventHandler(this.cv15tb_TextChanged);
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(635, 409);
            this.Controls.Add(this.label15);
            this.Controls.Add(this.label14);
            this.Controls.Add(this.label13);
            this.Controls.Add(this.comPortTb);
            this.Controls.Add(this.OpenBtn);
            this.Controls.Add(this.label12);
            this.Controls.Add(this.InTB);
            this.Controls.Add(this.AllocBtn);
            this.Controls.Add(this.StopBtn);
            this.Controls.Add(this.SpeedTB);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.LocoTB);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.trackBar1);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.ChangeBt);
            this.Controls.Add(this.NewAdrTB);
            this.Controls.Add(this.groupBox1);
            this.Name = "Form1";
            this.Text = "DCC Test";
            ((System.ComponentModel.ISupportInitialize)(this.SpeedTB)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.trackBar1)).EndInit();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox LocoTB;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.NumericUpDown SpeedTB;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.TextBox CVTB;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox ValueTB;
        private System.Windows.Forms.Button WriteBtn;
        private System.Windows.Forms.Button StopBtn;
        private System.Windows.Forms.Button AllocBtn;
        private System.Windows.Forms.TextBox InTB;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.TextBox StartTB;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox AccelTB;
        private System.Windows.Forms.TextBox DecelTB;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Button ChangeBt;
        private System.Windows.Forms.TextBox NewAdrTB;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.TextBox cv5TB;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.TextBox cv6TB;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.TextBox cv19TB;
        private System.Windows.Forms.Label label12;
        private System.Windows.Forms.Button OpenBtn;
        private System.Windows.Forms.ComboBox comPortTb;
        private System.Windows.Forms.TrackBar trackBar1;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.Label label14;
        private System.Windows.Forms.CheckBox F0cb;
        private System.Windows.Forms.CheckBox F1cb;
        private System.Windows.Forms.CheckBox F2cb;
        private System.Windows.Forms.CheckBox F3cb;
        private System.Windows.Forms.CheckBox F4cb;
        private System.Windows.Forms.CheckBox F5cb;
        private System.Windows.Forms.CheckBox F6cb;
        private System.Windows.Forms.CheckBox F7cb;
        private System.Windows.Forms.CheckBox F8cb;
        private System.Windows.Forms.CheckBox F9cb;
        private System.Windows.Forms.CheckBox F10cb;
        private System.Windows.Forms.CheckBox F11cb;
        private System.Windows.Forms.CheckBox F12cb;
        private System.Windows.Forms.CheckBox F13cb;
        private System.Windows.Forms.CheckBox F14cb;
        private System.Windows.Forms.CheckBox F15cb;
        private System.Windows.Forms.CheckBox F16cb;
        private System.Windows.Forms.CheckBox F17cb;
        private System.Windows.Forms.CheckBox F18cb;
        private System.Windows.Forms.CheckBox F19cb;
        private System.Windows.Forms.CheckBox F20cb;
        private System.Windows.Forms.CheckBox F21cb;
        private System.Windows.Forms.CheckBox F22cb;
        private System.Windows.Forms.CheckBox F23cb;
        private System.Windows.Forms.CheckBox F24cb;
        private System.Windows.Forms.CheckBox F25cb;
        private System.Windows.Forms.CheckBox F26cb;
        private System.Windows.Forms.CheckBox F27cb;
        private System.ComponentModel.BackgroundWorker backgroundWorker1;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Label label15;
        private System.Windows.Forms.TextBox cv15tb;
        private System.Windows.Forms.Label label16;
    }
}

