using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Aria.Utilities.ObjectDictionary
{
    public partial class NewObjectMethodParameter : Form
    {
        public NewObjectMethodParameter()
        {
            InitializeComponent();
        }

        private void cmdOK_Click(object sender, EventArgs e)
        {
            if (txtMethodParameter.Text.Trim().Length == 0)
            {
                txtMethodParameter.Text = txtMethodParameter.Text.Trim();
            }
        }
    }
}