﻿namespace OlcbSvr
{
    partial class OlcbSvr
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(OlcbSvr));
            this.LogTB = new System.Windows.Forms.TextBox();
            this.RangeFromTB = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.GroupBox = new System.Windows.Forms.ComboBox();
            this.label17 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.byte6txt = new System.Windows.Forms.TextBox();
            this.label18 = new System.Windows.Forms.Label();
            this.membertxt = new System.Windows.Forms.TextBox();
            this.LogCB = new System.Windows.Forms.CheckBox();
            this.NodeBtn = new System.Windows.Forms.Button();
            this.EventBtn = new System.Windows.Forms.Button();
            this.label2 = new System.Windows.Forms.Label();
            this.MaxConTB = new System.Windows.Forms.TextBox();
            this.FilterCB = new System.Windows.Forms.CheckBox();
            this.SuspendLayout();
            // 
            // LogTB
            // 
            this.LogTB.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.LogTB.Location = new System.Drawing.Point(0, 68);
            this.LogTB.Multiline = true;
            this.LogTB.Name = "LogTB";
            this.LogTB.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.LogTB.Size = new System.Drawing.Size(534, 202);
            this.LogTB.TabIndex = 3;
            // 
            // RangeFromTB
            // 
            this.RangeFromTB.Location = new System.Drawing.Point(428, 12);
            this.RangeFromTB.Name = "RangeFromTB";
            this.RangeFromTB.Size = new System.Drawing.Size(87, 20);
            this.RangeFromTB.TabIndex = 5;
            this.RangeFromTB.Validating += new System.ComponentModel.CancelEventHandler(this.RangeFromTB_Validating);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(371, 15);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(51, 13);
            this.label1.TabIndex = 6;
            this.label1.Text = "Hex Start";
            // 
            // GroupBox
            // 
            this.GroupBox.FormattingEnabled = true;
            this.GroupBox.Location = new System.Drawing.Point(54, 12);
            this.GroupBox.Name = "GroupBox";
            this.GroupBox.Size = new System.Drawing.Size(87, 21);
            this.GroupBox.TabIndex = 82;
            this.GroupBox.SelectedIndexChanged += new System.EventHandler(this.GroupBox_SelectedIndexChanged);
            // 
            // label17
            // 
            this.label17.AutoSize = true;
            this.label17.Location = new System.Drawing.Point(277, 15);
            this.label17.Name = "label17";
            this.label17.Size = new System.Drawing.Size(37, 13);
            this.label17.TabIndex = 81;
            this.label17.Text = "/ Start";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(147, 15);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(45, 13);
            this.label7.TabIndex = 80;
            this.label7.Text = "Member";
            // 
            // byte6txt
            // 
            this.byte6txt.Location = new System.Drawing.Point(320, 12);
            this.byte6txt.Name = "byte6txt";
            this.byte6txt.Size = new System.Drawing.Size(45, 20);
            this.byte6txt.TabIndex = 79;
            this.byte6txt.Validating += new System.ComponentModel.CancelEventHandler(this.byte6txt_Validating);
            // 
            // label18
            // 
            this.label18.AutoSize = true;
            this.label18.Location = new System.Drawing.Point(12, 15);
            this.label18.Name = "label18";
            this.label18.Size = new System.Drawing.Size(36, 13);
            this.label18.TabIndex = 76;
            this.label18.Text = "Group";
            // 
            // membertxt
            // 
            this.membertxt.Location = new System.Drawing.Point(198, 12);
            this.membertxt.Name = "membertxt";
            this.membertxt.Size = new System.Drawing.Size(73, 20);
            this.membertxt.TabIndex = 75;
            this.membertxt.Validating += new System.ComponentModel.CancelEventHandler(this.membertxt_Validating);
            // 
            // LogCB
            // 
            this.LogCB.AutoSize = true;
            this.LogCB.Location = new System.Drawing.Point(374, 43);
            this.LogCB.Name = "LogCB";
            this.LogCB.Size = new System.Drawing.Size(66, 17);
            this.LogCB.TabIndex = 83;
            this.LogCB.Text = "Data log";
            this.LogCB.UseVisualStyleBackColor = true;
            // 
            // NodeBtn
            // 
            this.NodeBtn.Location = new System.Drawing.Point(198, 39);
            this.NodeBtn.Name = "NodeBtn";
            this.NodeBtn.Size = new System.Drawing.Size(75, 23);
            this.NodeBtn.TabIndex = 84;
            this.NodeBtn.Text = "Node Ids";
            this.NodeBtn.UseVisualStyleBackColor = true;
            this.NodeBtn.Click += new System.EventHandler(this.NodeBtn_Click);
            // 
            // EventBtn
            // 
            this.EventBtn.Location = new System.Drawing.Point(280, 39);
            this.EventBtn.Name = "EventBtn";
            this.EventBtn.Size = new System.Drawing.Size(75, 23);
            this.EventBtn.TabIndex = 85;
            this.EventBtn.Text = "Events";
            this.EventBtn.UseVisualStyleBackColor = true;
            this.EventBtn.Click += new System.EventHandler(this.EventBtn_Click);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(13, 44);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(88, 13);
            this.label2.TabIndex = 87;
            this.label2.Text = "Max connections";
            // 
            // MaxConTB
            // 
            this.MaxConTB.Location = new System.Drawing.Point(107, 41);
            this.MaxConTB.Name = "MaxConTB";
            this.MaxConTB.ReadOnly = true;
            this.MaxConTB.Size = new System.Drawing.Size(52, 20);
            this.MaxConTB.TabIndex = 86;
            // 
            // FilterCB
            // 
            this.FilterCB.AutoSize = true;
            this.FilterCB.Location = new System.Drawing.Point(450, 43);
            this.FilterCB.Name = "FilterCB";
            this.FilterCB.Size = new System.Drawing.Size(65, 17);
            this.FilterCB.TabIndex = 88;
            this.FilterCB.Text = "No Filter";
            this.FilterCB.UseVisualStyleBackColor = true;
            // 
            // OlcbSvr
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(534, 269);
            this.Controls.Add(this.FilterCB);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.MaxConTB);
            this.Controls.Add(this.EventBtn);
            this.Controls.Add(this.NodeBtn);
            this.Controls.Add(this.LogCB);
            this.Controls.Add(this.GroupBox);
            this.Controls.Add(this.label17);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.byte6txt);
            this.Controls.Add(this.label18);
            this.Controls.Add(this.membertxt);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.RangeFromTB);
            this.Controls.Add(this.LogTB);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "OlcbSvr";
            this.Text = "OpenLCB Server";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.Server_FormClosing);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox LogTB;
        private System.Windows.Forms.TextBox RangeFromTB;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ComboBox GroupBox;
        private System.Windows.Forms.Label label17;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox byte6txt;
        private System.Windows.Forms.Label label18;
        private System.Windows.Forms.TextBox membertxt;
        private System.Windows.Forms.CheckBox LogCB;
        private System.Windows.Forms.Button NodeBtn;
        private System.Windows.Forms.Button EventBtn;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox MaxConTB;
        private System.Windows.Forms.CheckBox FilterCB;
    }
}

