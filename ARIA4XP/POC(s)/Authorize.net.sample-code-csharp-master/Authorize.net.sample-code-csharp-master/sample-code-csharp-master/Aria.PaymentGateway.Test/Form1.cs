using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Aria.PaymentGateway.Test
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            AriaPaymentGateway a = new AriaPaymentGateway();
            ConnectionInfo con = new ConnectionInfo();
            con.Environment = ConnectionInfo.RunEnvironment.SandBox;
            con.Items["ApiLoginID"] = "8r6G5dwXUjd";
            con.Items["ApiTransactionKey"] = "63X7NwEdhR8649WX";

            CustomerProfile customer = new CustomerProfile();
            customer.Id = "Customer A121";
            customer.Email = "CustomerA121@com,p1.com";
            var res = a.AddCustomerProfile(con, customer);
        }
    }
}
