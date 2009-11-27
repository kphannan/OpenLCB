namespace PCTest
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
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.progressBar1 = new System.Windows.Forms.ProgressBar();
            this.WriteNidBtn = new System.Windows.Forms.Button();
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
            this.NVvaluetb = new System.Windows.Forms.TextBox();
            this.NVwriteBTN = new System.Windows.Forms.Button();
            this.NVReadBtn = new System.Windows.Forms.Button();
            this.NVindextb = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.RestoreConfigBtn = new System.Windows.Forms.Button();
            this.SaveConfigBtn = new System.Windows.Forms.Button();
            this.NNtb = new System.Windows.Forms.ComboBox();
            this.label4 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.SoftwareBtn = new System.Windows.Forms.Button();
            this.UserText = new System.Windows.Forms.TextBox();
            this.NodeText = new System.Windows.Forms.TextBox();
            this.WriteUserId = new System.ComponentModel.BackgroundWorker();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.FlashTaskBaropt = new System.Windows.Forms.CheckBox();
            this.closeComPortBtn = new System.Windows.Forms.Button();
            this.KeepLogopt = new System.Windows.Forms.CheckBox();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.SuspendLayout();
            // 
            // LogSpace
            // 
            this.LogSpace.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.LogSpace.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(128)));
            this.LogSpace.Location = new System.Drawing.Point(475, 12);
            this.LogSpace.Multiline = true;
            this.LogSpace.Name = "LogSpace";
            this.LogSpace.ReadOnly = true;
            this.LogSpace.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.LogSpace.Size = new System.Drawing.Size(217, 402);
            this.LogSpace.TabIndex = 0;
            this.LogSpace.TabStop = false;
            // 
            // serialPort1
            // 
            this.serialPort1.DataReceived += new System.IO.Ports.SerialDataReceivedEventHandler(this.serialPort1_DataReceived);
            // 
            // ComPortBtn
            // 
            this.ComPortBtn.Location = new System.Drawing.Point(169, 338);
            this.ComPortBtn.Name = "ComPortBtn";
            this.ComPortBtn.Size = new System.Drawing.Size(93, 23);
            this.ComPortBtn.TabIndex = 1;
            this.ComPortBtn.Text = "ComPort ...";
            this.ComPortBtn.UseVisualStyleBackColor = true;
            this.ComPortBtn.Click += new System.EventHandler(this.ComPortBtn_Click);
            // 
            // SaveBtn
            // 
            this.SaveBtn.Location = new System.Drawing.Point(370, 338);
            this.SaveBtn.Name = "SaveBtn";
            this.SaveBtn.Size = new System.Drawing.Size(93, 23);
            this.SaveBtn.TabIndex = 3;
            this.SaveBtn.Text = "SaveLog ...";
            this.SaveBtn.UseVisualStyleBackColor = true;
            this.SaveBtn.Click += new System.EventHandler(this.SaveBtn_Click);
            // 
            // SendBtn
            // 
            this.SendBtn.Location = new System.Drawing.Point(358, 13);
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
            this.SendText.Size = new System.Drawing.Size(274, 20);
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
            this.label2.Location = new System.Drawing.Point(6, 22);
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
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(457, 45);
            this.groupBox1.TabIndex = 5;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Send Cmd";
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.progressBar1);
            this.groupBox2.Controls.Add(this.WriteNidBtn);
            this.groupBox2.Controls.Add(this.label7);
            this.groupBox2.Controls.Add(this.NIDtext);
            this.groupBox2.Controls.Add(this.GetNidsBtn);
            this.groupBox2.Controls.Add(this.EVwriteBTN);
            this.groupBox2.Controls.Add(this.EVreadBTN);
            this.groupBox2.Controls.Add(this.EventActiontb);
            this.groupBox2.Controls.Add(this.EventNumbertb);
            this.groupBox2.Controls.Add(this.WriteBtn);
            this.groupBox2.Controls.Add(this.ReadBtn);
            this.groupBox2.Controls.Add(this.EventIndextb);
            this.groupBox2.Controls.Add(this.label6);
            this.groupBox2.Controls.Add(this.NVvaluetb);
            this.groupBox2.Controls.Add(this.NVwriteBTN);
            this.groupBox2.Controls.Add(this.NVReadBtn);
            this.groupBox2.Controls.Add(this.NVindextb);
            this.groupBox2.Controls.Add(this.label5);
            this.groupBox2.Controls.Add(this.RestoreConfigBtn);
            this.groupBox2.Controls.Add(this.SaveConfigBtn);
            this.groupBox2.Controls.Add(this.NNtb);
            this.groupBox2.Controls.Add(this.label4);
            this.groupBox2.Controls.Add(this.label3);
            this.groupBox2.Controls.Add(this.SoftwareBtn);
            this.groupBox2.Controls.Add(this.UserText);
            this.groupBox2.Controls.Add(this.NodeText);
            this.groupBox2.Controls.Add(this.label2);
            this.groupBox2.Location = new System.Drawing.Point(12, 63);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(457, 255);
            this.groupBox2.TabIndex = 6;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Configure Node";
            // 
            // progressBar1
            // 
            this.progressBar1.Location = new System.Drawing.Point(258, 220);
            this.progressBar1.Name = "progressBar1";
            this.progressBar1.Size = new System.Drawing.Size(93, 23);
            this.progressBar1.TabIndex = 27;
            // 
            // WriteNidBtn
            // 
            this.WriteNidBtn.Location = new System.Drawing.Point(379, 19);
            this.WriteNidBtn.Name = "WriteNidBtn";
            this.WriteNidBtn.Size = new System.Drawing.Size(72, 23);
            this.WriteNidBtn.TabIndex = 26;
            this.WriteNidBtn.Text = "Write NID";
            this.WriteNidBtn.UseVisualStyleBackColor = true;
            this.WriteNidBtn.Click += new System.EventHandler(this.WriteNidBtn_Click);
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(142, 22);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(26, 13);
            this.label7.TabIndex = 25;
            this.label7.Text = "NID";
            // 
            // NIDtext
            // 
            this.NIDtext.Location = new System.Drawing.Point(174, 19);
            this.NIDtext.Name = "NIDtext";
            this.NIDtext.Size = new System.Drawing.Size(109, 20);
            this.NIDtext.TabIndex = 24;
            // 
            // GetNidsBtn
            // 
            this.GetNidsBtn.Location = new System.Drawing.Point(289, 19);
            this.GetNidsBtn.Name = "GetNidsBtn";
            this.GetNidsBtn.Size = new System.Drawing.Size(84, 23);
            this.GetNidsBtn.TabIndex = 23;
            this.GetNidsBtn.Text = "Get NID\'s";
            this.GetNidsBtn.UseVisualStyleBackColor = true;
            this.GetNidsBtn.Click += new System.EventHandler(this.GetNidsBtn_Click);
            // 
            // EVwriteBTN
            // 
            this.EVwriteBTN.Location = new System.Drawing.Point(358, 191);
            this.EVwriteBTN.Name = "EVwriteBTN";
            this.EVwriteBTN.Size = new System.Drawing.Size(93, 23);
            this.EVwriteBTN.TabIndex = 18;
            this.EVwriteBTN.Text = "WriteEV";
            this.EVwriteBTN.UseVisualStyleBackColor = true;
            this.EVwriteBTN.Click += new System.EventHandler(this.EVwriteBTN_Click);
            // 
            // EVreadBTN
            // 
            this.EVreadBTN.Location = new System.Drawing.Point(258, 191);
            this.EVreadBTN.Name = "EVreadBTN";
            this.EVreadBTN.Size = new System.Drawing.Size(93, 23);
            this.EVreadBTN.TabIndex = 17;
            this.EVreadBTN.Text = "ReadEV";
            this.EVreadBTN.UseVisualStyleBackColor = true;
            this.EVreadBTN.Click += new System.EventHandler(this.EVreadBTN_Click);
            // 
            // EventActiontb
            // 
            this.EventActiontb.Location = new System.Drawing.Point(258, 164);
            this.EventActiontb.Name = "EventActiontb";
            this.EventActiontb.Size = new System.Drawing.Size(193, 20);
            this.EventActiontb.TabIndex = 16;
            // 
            // EventNumbertb
            // 
            this.EventNumbertb.Location = new System.Drawing.Point(118, 164);
            this.EventNumbertb.Name = "EventNumbertb";
            this.EventNumbertb.Size = new System.Drawing.Size(132, 20);
            this.EventNumbertb.TabIndex = 15;
            // 
            // WriteBtn
            // 
            this.WriteBtn.Location = new System.Drawing.Point(358, 101);
            this.WriteBtn.Name = "WriteBtn";
            this.WriteBtn.Size = new System.Drawing.Size(93, 23);
            this.WriteBtn.TabIndex = 3;
            this.WriteBtn.Text = "Write User Str";
            this.WriteBtn.UseVisualStyleBackColor = true;
            this.WriteBtn.Click += new System.EventHandler(this.WriteBtn_Click);
            // 
            // ReadBtn
            // 
            this.ReadBtn.Location = new System.Drawing.Point(258, 101);
            this.ReadBtn.Name = "ReadBtn";
            this.ReadBtn.Size = new System.Drawing.Size(93, 23);
            this.ReadBtn.TabIndex = 2;
            this.ReadBtn.Text = "Read Info";
            this.ReadBtn.UseVisualStyleBackColor = true;
            this.ReadBtn.Click += new System.EventHandler(this.ReadBtn_Click);
            // 
            // EventIndextb
            // 
            this.EventIndextb.Location = new System.Drawing.Point(78, 164);
            this.EventIndextb.Name = "EventIndextb";
            this.EventIndextb.Size = new System.Drawing.Size(34, 20);
            this.EventIndextb.TabIndex = 14;
            this.EventIndextb.Text = "0";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(6, 167);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(35, 13);
            this.label6.TabIndex = 13;
            this.label6.Text = "Event";
            // 
            // NVvaluetb
            // 
            this.NVvaluetb.Location = new System.Drawing.Point(118, 138);
            this.NVvaluetb.Name = "NVvaluetb";
            this.NVvaluetb.Size = new System.Drawing.Size(41, 20);
            this.NVvaluetb.TabIndex = 10;
            // 
            // NVwriteBTN
            // 
            this.NVwriteBTN.Location = new System.Drawing.Point(358, 136);
            this.NVwriteBTN.Name = "NVwriteBTN";
            this.NVwriteBTN.Size = new System.Drawing.Size(93, 23);
            this.NVwriteBTN.TabIndex = 12;
            this.NVwriteBTN.Text = "WriteNV";
            this.NVwriteBTN.UseVisualStyleBackColor = true;
            this.NVwriteBTN.Click += new System.EventHandler(this.NVwriteBTN_Click);
            // 
            // NVReadBtn
            // 
            this.NVReadBtn.Location = new System.Drawing.Point(259, 136);
            this.NVReadBtn.Name = "NVReadBtn";
            this.NVReadBtn.Size = new System.Drawing.Size(93, 23);
            this.NVReadBtn.TabIndex = 11;
            this.NVReadBtn.Text = "ReadNV";
            this.NVReadBtn.UseVisualStyleBackColor = true;
            this.NVReadBtn.Click += new System.EventHandler(this.NVReadBtn_Click);
            // 
            // NVindextb
            // 
            this.NVindextb.Location = new System.Drawing.Point(78, 138);
            this.NVindextb.Name = "NVindextb";
            this.NVindextb.Size = new System.Drawing.Size(34, 20);
            this.NVindextb.TabIndex = 9;
            this.NVindextb.Text = "0";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(6, 142);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(71, 13);
            this.label5.TabIndex = 8;
            this.label5.Text = "NodeVariable";
            // 
            // RestoreConfigBtn
            // 
            this.RestoreConfigBtn.Location = new System.Drawing.Point(358, 220);
            this.RestoreConfigBtn.Name = "RestoreConfigBtn";
            this.RestoreConfigBtn.Size = new System.Drawing.Size(93, 23);
            this.RestoreConfigBtn.TabIndex = 22;
            this.RestoreConfigBtn.Text = "Restore Config";
            this.RestoreConfigBtn.UseVisualStyleBackColor = true;
            this.RestoreConfigBtn.Click += new System.EventHandler(this.RestoreConfigBtn_Click);
            // 
            // SaveConfigBtn
            // 
            this.SaveConfigBtn.Location = new System.Drawing.Point(58, 220);
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
            this.NNtb.Location = new System.Drawing.Point(78, 19);
            this.NNtb.Name = "NNtb";
            this.NNtb.Size = new System.Drawing.Size(58, 21);
            this.NNtb.Sorted = true;
            this.NNtb.TabIndex = 1;
            this.NNtb.SelectedIndexChanged += new System.EventHandler(this.NNtb_SelectedIndexChanged);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(6, 78);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(45, 13);
            this.label4.TabIndex = 6;
            this.label4.Text = "User Str";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(6, 51);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(58, 13);
            this.label3.TabIndex = 4;
            this.label3.Text = "Module Str";
            // 
            // SoftwareBtn
            // 
            this.SoftwareBtn.Location = new System.Drawing.Point(157, 220);
            this.SoftwareBtn.Name = "SoftwareBtn";
            this.SoftwareBtn.Size = new System.Drawing.Size(93, 23);
            this.SoftwareBtn.TabIndex = 19;
            this.SoftwareBtn.Text = "Upgrade ...";
            this.SoftwareBtn.UseVisualStyleBackColor = true;
            this.SoftwareBtn.Click += new System.EventHandler(this.SoftwareBtn_Click);
            // 
            // UserText
            // 
            this.UserText.Location = new System.Drawing.Point(78, 75);
            this.UserText.Name = "UserText";
            this.UserText.Size = new System.Drawing.Size(373, 20);
            this.UserText.TabIndex = 7;
            // 
            // NodeText
            // 
            this.NodeText.Location = new System.Drawing.Point(78, 48);
            this.NodeText.Name = "NodeText";
            this.NodeText.ReadOnly = true;
            this.NodeText.Size = new System.Drawing.Size(373, 20);
            this.NodeText.TabIndex = 5;
            this.NodeText.TabStop = false;
            // 
            // WriteUserId
            // 
            this.WriteUserId.WorkerSupportsCancellation = true;
            this.WriteUserId.DoWork += new System.ComponentModel.DoWorkEventHandler(this.WriteUser_DoWork);
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.FlashTaskBaropt);
            this.groupBox4.Location = new System.Drawing.Point(12, 367);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(457, 47);
            this.groupBox4.TabIndex = 4;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Display options";
            // 
            // FlashTaskBaropt
            // 
            this.FlashTaskBaropt.AutoSize = true;
            this.FlashTaskBaropt.Location = new System.Drawing.Point(12, 19);
            this.FlashTaskBaropt.Name = "FlashTaskBaropt";
            this.FlashTaskBaropt.Size = new System.Drawing.Size(94, 17);
            this.FlashTaskBaropt.TabIndex = 2;
            this.FlashTaskBaropt.Text = "Flash TaskBar";
            this.FlashTaskBaropt.UseVisualStyleBackColor = true;
            this.FlashTaskBaropt.CheckedChanged += new System.EventHandler(this.FlashTaskBaropt_CheckedChanged);
            // 
            // closeComPortBtn
            // 
            this.closeComPortBtn.Location = new System.Drawing.Point(270, 338);
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
            this.KeepLogopt.Location = new System.Drawing.Point(24, 342);
            this.KeepLogopt.Name = "KeepLogopt";
            this.KeepLogopt.Size = new System.Drawing.Size(68, 17);
            this.KeepLogopt.TabIndex = 7;
            this.KeepLogopt.Text = "Keep log";
            this.KeepLogopt.UseVisualStyleBackColor = true;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(704, 426);
            this.Controls.Add(this.KeepLogopt);
            this.Controls.Add(this.closeComPortBtn);
            this.Controls.Add(this.groupBox4);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.SaveBtn);
            this.Controls.Add(this.ComPortBtn);
            this.Controls.Add(this.LogSpace);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MinimumSize = new System.Drawing.Size(684, 403);
            this.Name = "Form1";
            this.Text = "PCTest 1.0.0.2";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.groupBox4.ResumeLayout(false);
            this.groupBox4.PerformLayout();
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
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.TextBox UserText;
        private System.Windows.Forms.TextBox NodeText;
        private System.Windows.Forms.Button ReadBtn;
        private System.Windows.Forms.Button SoftwareBtn;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Button WriteBtn;
        private System.ComponentModel.BackgroundWorker WriteUserId;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.ComboBox NNtb;
        private System.Windows.Forms.Button SaveConfigBtn;
        private System.Windows.Forms.Button RestoreConfigBtn;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.TextBox NVvaluetb;
        private System.Windows.Forms.Button NVwriteBTN;
        private System.Windows.Forms.Button NVReadBtn;
        private System.Windows.Forms.TextBox NVindextb;
        private System.Windows.Forms.Button EVwriteBTN;
        private System.Windows.Forms.Button EVreadBTN;
        private System.Windows.Forms.TextBox EventActiontb;
        private System.Windows.Forms.TextBox EventNumbertb;
        private System.Windows.Forms.TextBox EventIndextb;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Button closeComPortBtn;
        private System.Windows.Forms.CheckBox KeepLogopt;
        private System.Windows.Forms.CheckBox FlashTaskBaropt;
        private System.Windows.Forms.Button GetNidsBtn;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox NIDtext;
        private System.Windows.Forms.Button WriteNidBtn;
        private System.Windows.Forms.ProgressBar progressBar1;

    }
}

