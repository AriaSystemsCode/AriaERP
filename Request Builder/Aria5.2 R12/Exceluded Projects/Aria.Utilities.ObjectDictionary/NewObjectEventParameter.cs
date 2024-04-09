using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Aria.Utilities.ObjectDictionary
{
    public partial class NewObjectEventParameter : Form
    {
        public NewObjectEventParameter()
        {
            InitializeComponent();
        }

        private void cmdOK_Click(object sender, EventArgs e)
        {
            if (txtEventParameter.Text.Trim().Length == 0)
            {
                txtEventParameter.Text = txtEventParameter.Text.Trim();
            }
        }
    }
}