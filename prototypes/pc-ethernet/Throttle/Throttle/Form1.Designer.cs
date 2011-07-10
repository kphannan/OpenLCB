namespace Throttle
{
    partial class Throttle
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Throttle));
            this.trackBar1 = new System.Windows.Forms.TrackBar();
            this.label1 = new System.Windows.Forms.Label();
            this.LocoTB = new System.Windows.Forms.TextBox();
            this.StopBtn = new System.Windows.Forms.Button();
            this.SpeedTB = new System.Windows.Forms.NumericUpDown();
            this.label2 = new System.Windows.Forms.Label();
            this.label14 = new System.Windows.Forms.Label();
            this.label13 = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.trackBar1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.SpeedTB)).BeginInit();
            this.SuspendLayout();
            // 
            // trackBar1
            // 
            this.trackBar1.Location = new System.Drawing.Point(12, 64);
            this.trackBar1.Maximum = 126;
            this.trackBar1.Minimum = -126;
            this.trackBar1.Name = "trackBar1";
            this.trackBar1.Orientation = System.Windows.Forms.Orientation.Vertical;
            this.trackBar1.Size = new System.Drawing.Size(45, 318);
            this.trackBar1.TabIndex = 33;
            this.trackBar1.TickFrequency = 10;
            this.trackBar1.Scroll += new System.EventHandler(this.trackBar1_Scroll);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(9, 15);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(45, 13);
            this.label1.TabIndex = 34;
            this.label1.Text = "Address";
            // 
            // LocoTB
            // 
            this.LocoTB.Location = new System.Drawing.Point(60, 12);
            this.LocoTB.Name = "LocoTB";
            this.LocoTB.Size = new System.Drawing.Size(66, 20);
            this.LocoTB.TabIndex = 35;
            this.LocoTB.Text = "3";
            // 
            // StopBtn
            // 
            this.StopBtn.Location = new System.Drawing.Point(49, 211);
            this.StopBtn.Name = "StopBtn";
            this.StopBtn.Size = new System.Drawing.Size(58, 23);
            this.StopBtn.TabIndex = 36;
            this.StopBtn.Text = "Stop";
            this.StopBtn.UseVisualStyleBackColor = true;
            this.StopBtn.Click += new System.EventHandler(this.StopBtn_Click);
            // 
            // SpeedTB
            // 
            this.SpeedTB.Location = new System.Drawing.Point(60, 38);
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
            this.SpeedTB.Size = new System.Drawing.Size(66, 20);
            this.SpeedTB.TabIndex = 37;
            this.SpeedTB.ValueChanged += new System.EventHandler(this.SpeedTB_ValueChanged);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(9, 40);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(38, 13);
            this.label2.TabIndex = 38;
            this.label2.Text = "Speed";
            // 
            // label14
            // 
            this.label14.AutoSize = true;
            this.label14.Location = new System.Drawing.Point(46, 71);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(27, 13);
            this.label14.TabIndex = 39;
            this.label14.Text = "Fwd";
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Location = new System.Drawing.Point(46, 360);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(27, 13);
            this.label13.TabIndex = 40;
            this.label13.Text = "Rev";
            // 
            // Throttle
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(139, 386);
            this.Controls.Add(this.label13);
            this.Controls.Add(this.label14);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.SpeedTB);
            this.Controls.Add(this.StopBtn);
            this.Controls.Add(this.LocoTB);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.trackBar1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "Throttle";
            this.Text = "Throttle";
            ((System.ComponentModel.ISupportInitialize)(this.trackBar1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.SpeedTB)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TrackBar trackBar1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox LocoTB;
        private System.Windows.Forms.Button StopBtn;
        private System.Windows.Forms.NumericUpDown SpeedTB;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label14;
        private System.Windows.Forms.Label label13;
    }
}

