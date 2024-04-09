using System;
using System.Windows.Forms;
using System.Data;
using Map;

namespace TestMap
{
    public partial class Form1 : Form
    {
        public DataSet dsSource = new DataSet();
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            Map.XmlToSqlClass test = new Map.XmlToSqlClass();
         //   test.XmlToSql(textBox1.Text, textBox2.Text, textBox3.Text, textBox4.Text, textBox5.Text, textBox6.Text, textBox7.Text, "","");
            //test.XmlToSql("", "", @"C:\JascoBug.xml", @".\SQLEXPRESS", "Amazon", "crm", "crm", "", "");
            test.SqlToXml(@"D:\Mapping\Shipment.xml", @"D:\Mapping\t\OrderFulfillmentModified.xsd", @"C:\ship.xml", @".\SQLEXPRESS", "Amazon", "crm", "crm", "", "");
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Map.SqlToXmlClass test = new Map.SqlToXmlClass();
            //test.SqlToXml(@".\SQLEXPRESS", "Amazon", "crm", "crm", "PO_ITEMS_T,PO_ADDRESS_T",@"c:\rename.xml", @"C:\test.xml");
            test.SqlToXml(@".\SQLEXPRESS", "Amazon", "crm", "crm", "test", @"", @"C:\test.xml", "");
        }
    }
}
