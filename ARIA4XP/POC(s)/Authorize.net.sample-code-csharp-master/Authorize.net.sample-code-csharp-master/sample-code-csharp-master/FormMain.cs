using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace net.authorize.sample
{
    public partial class FormMain : Form
    {
        public FormMain()
        {
            InitializeComponent();
        }

        private void buttonChargeCreditCard_Click(object sender, EventArgs e)
        {
            FormChargeCreditCard form = new FormChargeCreditCard();
            form.Show(this);
        }

        private void buttonAuthorize_Click(object sender, EventArgs e)
        {
            FormAuthorizeCreditCard form = new FormAuthorizeCreditCard();
            form.Show(this);
        }

        private void buttonCapture_Click(object sender, EventArgs e)
        {

            FormCapture form = new FormCapture();
            form.Show(this);
        }

        private void buttonVoid_Click(object sender, EventArgs e)
        {
            FormVoid form = new FormVoid();
            form.Show(this);
        }

        private void button1_Click(object sender, EventArgs e)
        {
            FormCreateCustomerProfile form = new FormCreateCustomerProfile();
            form.Show(this);
        }

        private void buttonCharegeCustomerProfile_Click(object sender, EventArgs e)
        {
            FormChargeCustomerProfile form = new FormChargeCustomerProfile();
            form.Show(this);
        }
    }
}
