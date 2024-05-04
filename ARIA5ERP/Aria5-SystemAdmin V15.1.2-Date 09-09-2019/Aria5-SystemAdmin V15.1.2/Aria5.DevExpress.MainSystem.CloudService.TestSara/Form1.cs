using Aria5SystemAdmin.Module.Managers;
using DevExpress.Xpo;
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
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=azuresqladmin;Password=aria_123";
             Session session = XpoDefault.Session;
            string deviceSign = "176.92|162.47|18.71|34.108";
            string applicationID = "Aria5-Windows8Xaml-1TouchAwayFrontend";
            Guid accountOid = new Guid("d002568f-f55a-46ba-b290-dac589c7e2b6");

            ConfigurationItemManager.GetInformation(session, accountOid, applicationID, deviceSign);
        }
    }
}
