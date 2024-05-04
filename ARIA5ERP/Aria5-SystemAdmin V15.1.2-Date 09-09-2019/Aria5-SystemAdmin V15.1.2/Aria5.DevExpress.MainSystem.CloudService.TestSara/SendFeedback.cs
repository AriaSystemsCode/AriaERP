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
    public partial class SendFeedback : Form
    {
        public SendFeedback()
        {
            InitializeComponent();
        }

        private void Send_Click(object sender, EventArgs e)
        {
            XpoDefault.ConnectionString = @"Data Source=TR_SARAM\SQLEXPRESS;Initial Catalog=Azure_Aria5SystemAdmin_1;User ID=sa;Password=aria_123";
            Session session = XpoDefault.Session;
            string from="sara.m@ariasystems.biz";
            string to = "sara.m@ariasystems.biz";
            string password = "";
            string body = "Hello,<br/> User Email: ADMIN@ARIASYSTEMS.BIZ,<br/>  Rate:Very good,<br/> Rate:Definitely would recommend, <br/> <br/> Thanks .";
            AriaUserManager.SendFeedback(from, password, to, body);
        }
    }
}
