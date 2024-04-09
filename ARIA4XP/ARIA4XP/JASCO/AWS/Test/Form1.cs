using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using System.IO;

namespace Test
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }
        XmlDataDocument doc;
        XmlDocument document;
        private void button1_Click(object sender, EventArgs e)
        {
            AWS.Main aws = new AWS.Main();
            aws.Init("", @"D:\AWS\Amazon.xml", "");
        }

        private void Form1_Load(object sender, EventArgs e)
        {

        }
    }
}
//DataSet ds = new DataSet();
//ds.ReadXmlSchema(@"C:\t\OrderReport.xsd");
//ds.ReadXml(@"C:\t\in_1.xml");
//ds.WriteXml(@"C:\t\in_11.xml", XmlWriteMode.WriteSchema);