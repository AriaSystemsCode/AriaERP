using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Aria.Configuration.Server
{
    public partial class TextEditorForm : Form
    {
        public TextEditorForm(string text)
        {
            InitializeComponent();

            this.richTextBox1.Text = text;
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Clipboard.SetText(this.richTextBox1.Text);
        }

        private void button1_Click(object sender, EventArgs e)
        {
            this.Close();
        }
    }
}