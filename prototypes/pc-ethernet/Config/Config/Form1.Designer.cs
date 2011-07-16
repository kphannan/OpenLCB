namespace Config
{
    partial class Config
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Config));
            this.LogTB = new System.Windows.Forms.TextBox();
            this.NodeIdsBtn = new System.Windows.Forms.Button();
            this.SelectNodeCB = new System.Windows.Forms.ComboBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.SegmentsTB = new System.Windows.Forms.ComboBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.RebootBtn = new System.Windows.Forms.Button();
            this.DefaultBtn = new System.Windows.Forms.Button();
            this.DeleteBtn = new System.Windows.Forms.Button();
            this.WriteAllBtn = new System.Windows.Forms.Button();
            this.ReadAllBtn = new System.Windows.Forms.Button();
            this.WriteBtn = new System.Windows.Forms.Button();
            this.ReadBtn = new System.Windows.Forms.Button();
            this.LogCB = new System.Windows.Forms.CheckBox();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // LogTB
            // 
            this.LogTB.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.LogTB.Location = new System.Drawing.Point(485, 0);
            this.LogTB.Multiline = true;
            this.LogTB.Name = "LogTB";
            this.LogTB.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.LogTB.Size = new System.Drawing.Size(324, 488);
            this.LogTB.TabIndex = 0;
            // 
            // NodeIdsBtn
            // 
            this.NodeIdsBtn.Location = new System.Drawing.Point(13, 13);
            this.NodeIdsBtn.Name = "NodeIdsBtn";
            this.NodeIdsBtn.Size = new System.Drawing.Size(83, 23);
            this.NodeIdsBtn.TabIndex = 1;
            this.NodeIdsBtn.Text = "Get Node Ids";
            this.NodeIdsBtn.UseVisualStyleBackColor = true;
            this.NodeIdsBtn.Click += new System.EventHandler(this.NodeIdsBtn_Click);
            // 
            // SelectNodeCB
            // 
            this.SelectNodeCB.FormattingEnabled = true;
            this.SelectNodeCB.Location = new System.Drawing.Point(145, 15);
            this.SelectNodeCB.Name = "SelectNodeCB";
            this.SelectNodeCB.Size = new System.Drawing.Size(221, 21);
            this.SelectNodeCB.TabIndex = 2;
            this.SelectNodeCB.SelectedIndexChanged += new System.EventHandler(this.SelectNodeCB_SelectedIndexChanged);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(102, 18);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(37, 13);
            this.label1.TabIndex = 3;
            this.label1.Text = "Select";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(24, 163);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(52, 13);
            this.label6.TabIndex = 12;
            this.label6.Text = "Segment:";
            // 
            // SegmentsTB
            // 
            this.SegmentsTB.FormattingEnabled = true;
            this.SegmentsTB.Location = new System.Drawing.Point(163, 160);
            this.SegmentsTB.Name = "SegmentsTB";
            this.SegmentsTB.Size = new System.Drawing.Size(181, 21);
            this.SegmentsTB.TabIndex = 13;
            this.SegmentsTB.SelectedIndexChanged += new System.EventHandler(this.SegmentsTB_SelectedIndexChanged);
            // 
            // groupBox1
            // 
            this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)));
            this.groupBox1.Controls.Add(this.RebootBtn);
            this.groupBox1.Controls.Add(this.DefaultBtn);
            this.groupBox1.Controls.Add(this.DeleteBtn);
            this.groupBox1.Controls.Add(this.WriteAllBtn);
            this.groupBox1.Controls.Add(this.ReadAllBtn);
            this.groupBox1.Controls.Add(this.WriteBtn);
            this.groupBox1.Controls.Add(this.ReadBtn);
            this.groupBox1.Location = new System.Drawing.Point(13, 179);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(452, 297);
            this.groupBox1.TabIndex = 14;
            this.groupBox1.TabStop = false;
            // 
            // RebootBtn
            // 
            this.RebootBtn.Location = new System.Drawing.Point(371, 193);
            this.RebootBtn.Name = "RebootBtn";
            this.RebootBtn.Size = new System.Drawing.Size(75, 23);
            this.RebootBtn.TabIndex = 6;
            this.RebootBtn.Text = "Reboot";
            this.RebootBtn.UseVisualStyleBackColor = true;
            this.RebootBtn.Click += new System.EventHandler(this.RebootBtn_Click);
            // 
            // DefaultBtn
            // 
            this.DefaultBtn.Location = new System.Drawing.Point(371, 164);
            this.DefaultBtn.Name = "DefaultBtn";
            this.DefaultBtn.Size = new System.Drawing.Size(75, 23);
            this.DefaultBtn.TabIndex = 5;
            this.DefaultBtn.Text = "Set default";
            this.DefaultBtn.UseVisualStyleBackColor = true;
            this.DefaultBtn.Click += new System.EventHandler(this.DefaultBtn_Click);
            // 
            // DeleteBtn
            // 
            this.DeleteBtn.Location = new System.Drawing.Point(371, 135);
            this.DeleteBtn.Name = "DeleteBtn";
            this.DeleteBtn.Size = new System.Drawing.Size(75, 23);
            this.DeleteBtn.TabIndex = 4;
            this.DeleteBtn.Text = "Delete";
            this.DeleteBtn.UseVisualStyleBackColor = true;
            // 
            // WriteAllBtn
            // 
            this.WriteAllBtn.Location = new System.Drawing.Point(371, 106);
            this.WriteAllBtn.Name = "WriteAllBtn";
            this.WriteAllBtn.Size = new System.Drawing.Size(75, 23);
            this.WriteAllBtn.TabIndex = 3;
            this.WriteAllBtn.Text = "Write All";
            this.WriteAllBtn.UseVisualStyleBackColor = true;
            // 
            // ReadAllBtn
            // 
            this.ReadAllBtn.Location = new System.Drawing.Point(371, 77);
            this.ReadAllBtn.Name = "ReadAllBtn";
            this.ReadAllBtn.Size = new System.Drawing.Size(75, 23);
            this.ReadAllBtn.TabIndex = 2;
            this.ReadAllBtn.Text = "Read All";
            this.ReadAllBtn.UseVisualStyleBackColor = true;
            // 
            // WriteBtn
            // 
            this.WriteBtn.Location = new System.Drawing.Point(371, 48);
            this.WriteBtn.Name = "WriteBtn";
            this.WriteBtn.Size = new System.Drawing.Size(75, 23);
            this.WriteBtn.TabIndex = 1;
            this.WriteBtn.Text = "Write";
            this.WriteBtn.UseVisualStyleBackColor = true;
            this.WriteBtn.Click += new System.EventHandler(this.WriteBtn_Click);
            // 
            // ReadBtn
            // 
            this.ReadBtn.Location = new System.Drawing.Point(371, 19);
            this.ReadBtn.Name = "ReadBtn";
            this.ReadBtn.Size = new System.Drawing.Size(75, 23);
            this.ReadBtn.TabIndex = 0;
            this.ReadBtn.Text = "Read";
            this.ReadBtn.UseVisualStyleBackColor = true;
            this.ReadBtn.Click += new System.EventHandler(this.ReadBtn_Click);
            // 
            // LogCB
            // 
            this.LogCB.AutoSize = true;
            this.LogCB.Location = new System.Drawing.Point(393, 17);
            this.LogCB.Name = "LogCB";
            this.LogCB.Size = new System.Drawing.Size(66, 17);
            this.LogCB.TabIndex = 15;
            this.LogCB.Text = "Data log";
            this.LogCB.UseVisualStyleBackColor = true;
            // 
            // Config
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(809, 488);
            this.Controls.Add(this.LogCB);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.SegmentsTB);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.SelectNodeCB);
            this.Controls.Add(this.NodeIdsBtn);
            this.Controls.Add(this.LogTB);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "Config";
            this.Text = "Config";
            this.groupBox1.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox LogTB;
        private System.Windows.Forms.Button NodeIdsBtn;
        private System.Windows.Forms.ComboBox SelectNodeCB;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.ComboBox SegmentsTB;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Button RebootBtn;
        private System.Windows.Forms.Button DefaultBtn;
        private System.Windows.Forms.Button DeleteBtn;
        private System.Windows.Forms.Button WriteAllBtn;
        private System.Windows.Forms.Button ReadAllBtn;
        private System.Windows.Forms.Button WriteBtn;
        private System.Windows.Forms.Button ReadBtn;
        private System.Windows.Forms.CheckBox LogCB;
    }
}

