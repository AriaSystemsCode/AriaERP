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
    public partial class GetAccountStatus : Form
    {
        public GetAccountStatus()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            //AccountServiceReference1.AccountWebServiceSoapClient _accountwebservice = new AccountServiceReference1.AccountWebServiceSoapClient();
            //AccountServiceReference.AccountWebServiceSoapClient _accountwebservice = new AccountServiceReference.AccountWebServiceSoapClient();
            // AccountRegistration _accountwebservice = new AccountRegistration();
            //Guid x = new Guid(textBox1.Text);



            #region start connection

            XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=SystemAdminTestingUser1;Password=aria_123";

            Account temp = new Account(XpoDefault.Session);

            string schemaName = "DBO";

            foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
            {
                Type[] types = null;

                try
                {
                    types = assembly.GetTypes();
                }
                catch (Exception)
                {
                }

                if (types != null) foreach (var type in types)
                    {
                        if (type == typeof(XPObjectType) || type.IsSubclassOf(typeof(XPBaseObject)))
                        {
                            var typeInfo = XpoDefault.Session.DataLayer.Dictionary.GetClassInfo(type);

                            if (typeInfo.Table != null)
                            {
                                if (!typeInfo.Table.Name.StartsWith(schemaName + "."))
                                {
                                    typeInfo.Table.Name = schemaName + "." + typeInfo.Table.Name;
                                }

                                foreach (var fk in typeInfo.Table.ForeignKeys)
                                {
                                    if (!fk.PrimaryKeyTable.StartsWith(schemaName + "."))
                                    {
                                        fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable;
                                    }
                                }
                            }
                        }
                    }
            }

            #endregion

         //   ServiceReference1.AccountWebServiceSoapClient accountManager = new ServiceReference1.AccountWebServiceSoapClient();


            //AccountManager accountManager = new AccountManager();
            Guid accountOid = Guid.Parse(textBox1.Text.ToString().TrimEnd());
            //MessageBox.Show(accountManager.GetAccountStatus(accountOid));

        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {

        }

        private void GetAccountStatus_FormClosing(object sender, FormClosingEventArgs e)
        {
            System.Threading.Thread t = new System.Threading.Thread(new System.Threading.ThreadStart(ThreadProc));
            t.Start();
        }

        public static void ThreadProc()
        {

            Application.Run(new StartForm());
        }



    }
}
