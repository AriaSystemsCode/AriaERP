using Aria5SystemAdmin.Module.BusinessObjects;
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
    public partial class AccountRegistrationForm : Form
    {
        public AccountRegistrationForm()
        {
            InitializeComponent();
            StartConnection();
        }

        private void buttonGetDemoCategories_Click(object sender, EventArgs e)
        {
            AccountManager accountManager = new AccountManager();

            List<Account> accountList = accountManager.GetDemoGuideCategories();
        }

        private void buttonGetDemoGuide_Click(object sender, EventArgs e)
        {
            AccountManager accountManager = new AccountManager();
            string categoryId = "cat1";

            List<Account> accountList = accountManager.GetDemoGuides(categoryId);
        }

        public void StartConnection()
        {

            XpoDefault.ConnectionString = @"Data Source=TR-SARA\SQLEXPRESS;Initial Catalog=SolutionTest1;Persist Security Info=True;User ID=sa;Password=aria";

        }
    }
}
