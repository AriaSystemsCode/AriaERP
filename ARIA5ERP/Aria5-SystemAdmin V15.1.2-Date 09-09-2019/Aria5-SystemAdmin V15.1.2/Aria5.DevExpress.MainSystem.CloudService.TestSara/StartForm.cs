using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Aria5.DevExpress.MainSystem.CloudService.TestSara
{
    public partial class StartForm : Form
    {
        public StartForm()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            if (comboBox1.Text == "Get Account Status")
            {
                System.Threading.Thread t = new System.Threading.Thread(new System.Threading.ThreadStart(ThreadProcGetAccountStatus));
                t.Start();
            }

            if (comboBox1.Text == "Register New Account")
            {
                System.Threading.Thread t = new System.Threading.Thread(new System.Threading.ThreadStart(ThreadProcRegisterNewAccount));
                t.Start();
            }

            if (comboBox1.Text == "Generate Activation Key By Manager")
            {
                System.Threading.Thread t = new System.Threading.Thread(new System.Threading.ThreadStart(ThreadProcGenerateActivationKeyByManager));
                t.Start();
            }
            
            this.Close();
        }

        public static void ThreadProcGetAccountStatus()
        {
            Application.Run(new GetAccountStatus());
        }

        public static void ThreadProcRegisterNewAccount()
        {
            Application.Run(new RegisterNewAccount());
        }

        public static void ThreadProcGenerateActivationKeyByManager()
        {
            Application.Run(new FormGenerateActivationKeyByManager());
        }
      
        private void comboBox1_SelectedIndexChanged(object sender, EventArgs e)
        {

        }

        private void StartForm_Load(object sender, EventArgs e)
        {
            comboBox1.SelectedIndex = 0;
        }
    }
}
