using Aria5SystemAdmin.Module.BusinessObjects;
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

namespace Aria5.DevExpress.MainSystem.Windows.TestMina
{
    public partial class FormAddNewAccount : Form
    {
        public FormAddNewAccount()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            //AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient a = new AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient();
            //SetConnection();
            //Account acc = new Account(XpoDefault.Session);
            //acc.Name = textBox1.Text;
            //acc.Id = textBox3.Text;
            //acc.Save();
            //textBox2.Text = acc.Oid.ToString();

        }

        public void SetConnection()
        {
            XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=SystemAdminTestingUser1;Password=aria_123";

            string schemaName = "Test4";

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
        }
    }
}
