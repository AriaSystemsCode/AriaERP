using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Aria.Utilities.ObjectDictionary
{
    public partial class NewObjectRevision : Form
    {
        public NewObjectRevision()
        {
            InitializeComponent();
        }

        private void cmdOK_Click(object sender, EventArgs e)
        {
            if (txtRevision.Text.Trim().Length == 0)
            {
                txtRevision.Text = txtRevision.Text.Trim();
            }
        }
    }
}