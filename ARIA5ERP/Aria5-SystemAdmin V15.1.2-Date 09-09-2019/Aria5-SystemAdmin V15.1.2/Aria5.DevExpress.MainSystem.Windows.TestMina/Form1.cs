using Aria5.DevExpress.MainSystem.Module.BusinessObjects;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.ExpressApp.Security;
using DevExpress.ExpressApp.Security.Strategy;
using DevExpress.ExpressApp.Xpo;
using DevExpress.Xpo;
using DevExpress.Xpo.Metadata;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace Aria5.DevExpress.MainSystem.Windows.TestMina
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        public static void SwitchSchema()
        {
            XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=azuresqladmin;Password=aria_123";
            string schemaName = "Test3";

            Account temp1 = new Account(XpoDefault.Session);
            IdentifierStructure temp2 = new IdentifierStructure(XpoDefault.Session);
            SecuritySystemUser temp3 = new SecuritySystemUser(XpoDefault.Session);
            SecuritySystemRole temp4 = new SecuritySystemRole(XpoDefault.Session);

            XPDictionary xpDictionary = XpoDefault.Session.DataLayer.Dictionary;

            var x = xpDictionary.GetDataStoreSchema(typeof(IdentifierStructure).Assembly, typeof(Account).Assembly, typeof(SecuritySystemUser).Assembly);

            foreach (var type in x)
            {
                if (!type.Name.StartsWith(schemaName + "."))
                {
                    type.Name = schemaName + "." + type.Name;
                }

                foreach (var fk in type.ForeignKeys)
                {
                    if (!fk.PrimaryKeyTable.StartsWith(schemaName + "."))
                    {
                        fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable;
                    }
                }
            }

            
            
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

                            //XpoTypesInfoHelper.GetTypesInfo().RefreshInfo(type);

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


  

            XPCollection<AriaSecuritySystemRole> ariaSecuritySystemRoles = new XPCollection<AriaSecuritySystemRole>(XpoDefault.Session);
            ariaSecuritySystemRoles.Load();

            XPCollection<AriaSecuritySystemUser> ariaSecuritySystemUsers = new XPCollection<AriaSecuritySystemUser>(XpoDefault.Session);
            ariaSecuritySystemUsers.Load();

            ariaSecuritySystemUsers.First().Roles.Add(ariaSecuritySystemRoles.First());
            ariaSecuritySystemUsers.First().Save();

        }

        private void button1_Click(object sender, EventArgs e)
        {
            SwitchSchema();
        }
    }
}
