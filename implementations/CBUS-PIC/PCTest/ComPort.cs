using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Reflection;
using System.Windows.Forms;
using System.IO.Ports;

namespace PCTest
{
    partial class ComPort : Form
    {
        public string ComPortStr;
        public string ComPortHandshake;
        public string ComPortSpeed;

        public ComPort()
        {
            InitializeComponent();
            foreach (string s in SerialPort.GetPortNames()) 
                comboBox1.Items.Add(s);
            
            string[] speeds = { "9600", "19200", "38400", "57600", "115200", "230400" };
            foreach (string speed in speeds)
                comboBox2.Items.Add(speed);
            foreach (string s in Enum.GetNames(typeof(Handshake)))
                comboBox3.Items.Add(s);
        }

        #region Assembly Attribute Accessors

        #endregion

        private void OKBtn_Click(object sender, EventArgs e)
        {
            ComPortStr = comboBox1.Text;
            ComPortSpeed = comboBox2.Text;
            ComPortHandshake = comboBox3.Text;
            this.Close();
            this.DialogResult = DialogResult.OK;
        }

        private void CancelBtn_Click(object sender, EventArgs e)
        {
            this.Close();
            this.DialogResult = DialogResult.Cancel;
        }

        private void ComPort_Shown(object sender, EventArgs e)
        {
            comboBox1.Text = ComPortStr;
            comboBox2.Text = ComPortSpeed;
            comboBox3.Text = ComPortHandshake;
        }

    }
}
