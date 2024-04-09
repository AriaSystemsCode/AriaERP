using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Aria.DataTypes;

namespace Aria.Utilities.ObjectDictionary
{
    public partial class NewObjectProperty : Form
    {
        public NewObjectProperty()
        {
            InitializeComponent();
        }

        private void NewObjectProperty_Load(object sender, EventArgs e)
        {
            cboType.Items.Add(AriaDataTypes.AriaDataObjectPointer);
            cboType.Items.Add(AriaDataTypes.AriaDictionaryDefinedObject);
            cboType.Items.Add(AriaDataTypes.AriaOption);
            cboType.Items.Add(AriaDataTypes.AriaOptionGridXmlDataSet);
            cboType.Items.Add(AriaDataTypes.AriaField);
            cboType.Items.Add(AriaDataTypes.AriaRelatedField);
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