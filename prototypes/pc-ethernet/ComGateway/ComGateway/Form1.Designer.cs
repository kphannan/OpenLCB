namespace ComGateway
{
    partial class ComGateway
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ComGateway));
            this.label1 = new System.Windows.Forms.Label();
            this.ComCB = new System.Windows.Forms.ComboBox();
            this.LogTB = new System.Windows.Forms.TextBox();
            this.LogCB = new System.Windows.Forms.CheckBox();
            this.AliasBtn = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(12, 9);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(28, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Com";
            // 
            // ComCB
            // 
            this.ComCB.FormattingEnabled = true;
            this.ComCB.Location = new System.Drawing.Point(46, 6);
            this.ComCB.Name = "ComCB";
            this.ComCB.Size = new System.Drawing.Size(100, 21);
            this.ComCB.TabIndex = 1;
            this.ComCB.DropDown += new System.EventHandler(this.ComCB_DropDown);
            this.ComCB.SelectedIndexChanged += new System.EventHandler(this.ComCB_SelectedIndexChanged);
            // 
            // LogTB
            // 
            this.LogTB.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.LogTB.Location = new System.Drawing.Point(-1, 33);
            this.LogTB.Multiline = true;
            this.LogTB.Name = "LogTB";
            this.LogTB.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.LogTB.Size = new System.Drawing.Size(405, 115);
            this.LogTB.TabIndex = 2;
            // 
            // LogCB
            // 
            this.LogCB.AutoSize = true;
            this.LogCB.Location = new System.Drawing.Point(326, 8);
            this.LogCB.Name = "LogCB";
            this.LogCB.Size = new System.Drawing.Size(66, 17);
            this.LogCB.TabIndex = 3;
            this.LogCB.Text = "Data log";
            this.LogCB.UseVisualStyleBackColor = true;
            // 
            // AliasBtn
            // 
            this.AliasBtn.Location = new System.Drawing.Point(220, 4);
            this.AliasBtn.Name = "AliasBtn";
            this.AliasBtn.Size = new System.Drawing.Size(75, 23);
            this.AliasBtn.TabIndex = 4;
            this.AliasBtn.Text = "Alias Table";
            this.AliasBtn.UseVisualStyleBackColor = true;
            this.AliasBtn.Click += new System.EventHandler(this.AliasBtn_Click);
            // 
            // ComGateway
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(404, 149);
            this.Controls.Add(this.AliasBtn);
            this.Controls.Add(this.LogCB);
            this.Controls.Add(this.LogTB);
            this.Controls.Add(this.ComCB);
            this.Controls.Add(this.label1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "ComGateway";
            this.Text = "ComGateway";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.ComGateway_FormClosing);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ComboBox ComCB;
        private System.Windows.Forms.TextBox LogTB;
        private System.Windows.Forms.CheckBox LogCB;
        private System.Windows.Forms.Button AliasBtn;
    }
}

