using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Aria.Utilities.ObjectDictionary
{
    public partial class NewObjectMethod : Form
    {
        public NewObjectMethod()
        {
            InitializeComponent();
        }

        private void cmdOK_Click(object sender, EventArgs e)
        {
            if (txtMethod.Text.Trim().Length == 0)
            {
                txtMethod.Text = txtMethod.Text.Trim();
            }
        }
    }
}