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
            // ComGateway
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(404, 149);
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
    }
}

