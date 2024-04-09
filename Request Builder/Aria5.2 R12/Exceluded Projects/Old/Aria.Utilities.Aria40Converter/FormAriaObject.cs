using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;

namespace Aria.Utilities.Aria40Converter
{
    public partial class FormAriaObject : Form
    {
        private DataTable _ariaObject = new DataTable();

        public FormAriaObject()
        {
            InitializeComponent();

            string objectXml = Path.Combine(Application.StartupPath, @"MergeData\Aria5\AriaObject.XML");
            _ariaObject.ReadXml(objectXml);

            _ariaObject.DefaultView.Sort = "select";
            dataGridView1.DataSource = _ariaObject;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            string objectXml = Path.Combine(Application.StartupPath, @"MergeData\Aria5\AriaObject.XML");
            _ariaObject.WriteXml(objectXml, XmlWriteMode.WriteSchema);
        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {
            _ariaObject.DefaultView.RowFilter = "OrgObjectName like '" + textBox1.Text + "%'";
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Close();
        }
    }
}
