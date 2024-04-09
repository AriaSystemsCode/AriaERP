using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Aria.DataTypes.ObjectDictionary;

namespace Aria.Utilities.ObjectDictionary
{
    public partial class NewObject : Form
    {
        public NewObject()
        {
            InitializeComponent();
        }

        private void NewObject_Load(object sender, EventArgs e)
        {
            cboType.Items.Add(AriaObjectTypes.Data);
            cboType.Items.Add(AriaObjectTypes.Framework);
            cboType.Items.Add(AriaObjectTypes.OptionGrid);
            cboType.Items.Add(AriaObjectTypes.Package);
            cboType.Items.Add(AriaObjectTypes.Presenatation);
            cboType.Items.Add(AriaObjectTypes.RelatedData);
            cboType.Items.Add(AriaObjectTypes.Report);
            cboType.Items.Add(AriaObjectTypes.Server);
            cboType.Items.Add(AriaObjectTypes.Utility);
            cboType.SelectedIndex = 0;
        }

        private void cmdOK_Click(object sender, EventArgs e)
        {
            if (txtName.Text.Trim().Length == 0)
            {
                txtName.Text = txtName.Text.Trim();
            }
        }
    }
}