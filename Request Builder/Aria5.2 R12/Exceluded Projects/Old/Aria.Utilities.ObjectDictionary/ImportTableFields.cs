using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Aria.Data;

namespace Aria.Utilities.ObjectDictionary
{
    public partial class ImportTableFields : Form
    {
        private string clientId;
        public ImportTableFields()
        {
            InitializeComponent();
        }


        private void ImportTableFields_Load(object sender, EventArgs e)
        {
            cboTables.Items.Clear();

            AriaDbCommand command = new AriaDbCommand("Select * from sydfiles order by cfile_nam", new AriaDbConnection("Aria", ""), Aria.Environment.AriaDatabaseTypes.Aria27SystemFiles, clientId);
            DataTable table = command.GetDataTable();

            for (int index = 0; index < table.Rows.Count; index++)
            {
                cboTables.Items.Add(table.Rows[index]["cfile_nam"].ToString() + " - " + table.Rows[index]["cFile_ttl"].ToString());
            }

            cboTables.SelectedIndex = 0;
        }

        private void chkAria4XP_CheckedChanged(object sender, EventArgs e)
        {
            cboTables.Items.Clear();

            AriaDbCommand command = new AriaDbCommand("Select * from sydfiles order by cfile_nam", new AriaDbConnection("Aria", ""), chkAria4XP.Checked ? Aria.Environment.AriaDatabaseTypes.Aria40SystemFiles : Aria.Environment.AriaDatabaseTypes.Aria27SystemFiles, clientId);
            DataTable table = command.GetDataTable();

            for (int index = 0; index < table.Rows.Count; index++)
            {
                cboTables.Items.Add(table.Rows[index]["cfile_nam"].ToString() + " - " + table.Rows[index]["cFile_ttl"].ToString());
            }

            cboTables.SelectedIndex = 0;
        }
    }
}