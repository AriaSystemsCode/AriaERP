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
    public partial class ImportOptionGridOptions : Form
    {
        public ImportOptionGridOptions()
        {
            InitializeComponent();
        }


        private void ImportTableFields_Load(object sender, EventArgs e)
        {
            cboOptionGrids.Items.Clear();

            AriaDbCommand command = new AriaDbCommand("Select * from sydreprt order by crep_id", new AriaDbConnection("Aria", ""), Aria.Environment.AriaDatabaseTypes.Aria27SystemFiles);
            DataTable table = command.GetDataTable();

            for (int index = 0; index < table.Rows.Count; index++)
            {
                cboOptionGrids.Items.Add(table.Rows[index]["crep_id"].ToString() + " - " + table.Rows[index]["crep_name"].ToString());
            }

            cboOptionGrids.SelectedIndex = 0;
        }
    }
}