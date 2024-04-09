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
    public partial class FormAriaObjectProperty : Form
    {
        private DataTable _ariaObjectProperty = new DataTable();

        public FormAriaObjectProperty()
        {
            InitializeComponent();

            string objectPropertyXml = Path.Combine(Application.StartupPath, @"MergeData\Aria5\AriaObjectProperty.XML");
            _ariaObjectProperty.ReadXml(objectPropertyXml);

            _ariaObjectProperty.DefaultView.Sort = "select";
            dataGridView1.DataSource = _ariaObjectProperty;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            string objectPropertyXml = Path.Combine(Application.StartupPath, @"MergeData\Aria5\AriaObjectProperty.XML");
            _ariaObjectProperty.WriteXml(objectPropertyXml, XmlWriteMode.WriteSchema);
        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {
            _ariaObjectProperty.DefaultView.RowFilter = "ObjectName = '" + textBox1.Text + "'";
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void textBox2_TextChanged(object sender, EventArgs e)
        {
            _ariaObjectProperty.DefaultView.RowFilter = "ObjectName = '" + textBox1.Text + "' AND FieldName = '" + textBox2.Text + "'";
        }

        private void FormAriaObjectProperty_Load(object sender, EventArgs e)
        {

        }
    }
}
