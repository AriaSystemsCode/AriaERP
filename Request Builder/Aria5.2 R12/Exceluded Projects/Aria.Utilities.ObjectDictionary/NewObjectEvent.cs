using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Aria.Utilities.ObjectDictionary
{
    public partial class NewObjectEvent : Form
    {
        public NewObjectEvent()
        {
            InitializeComponent();
        }

        private void cmdOK_Click(object sender, EventArgs e)
        {
            if (txtEvent.Text.Trim().Length == 0)
            {
                txtEvent.Text = txtEvent.Text.Trim();
            }
        }
    }
}