using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace CallingWebService1
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            Guid _guid = new Guid("75B51829-66D7-4FBD-AFC0-01521D17D0DF");
            ServiceReference3.AccountWebServiceSoapClient x = new ServiceReference3.AccountWebServiceSoapClient();
            string m = x.AccountStatus(_guid);
            textBox1.Text = m;
        }
    }
}
