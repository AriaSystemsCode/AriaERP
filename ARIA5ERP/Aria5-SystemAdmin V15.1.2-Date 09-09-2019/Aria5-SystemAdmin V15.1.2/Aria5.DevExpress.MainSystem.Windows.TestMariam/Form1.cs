using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Aria5.DevExpress.MainSystem.Module.Entities;
using DevExpress.Xpo;
namespace Aria5.DevExpress.MainSystem.Windows.TestMariam
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {

            EntityIdentifierStructure X = new EntityIdentifierStructure();

            XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=SystemAdminTestingUser1;Password=aria_123";
            string schemaName = "SystemAdminTestingEnv";
            foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
            {
                foreach (var type in assembly.GetTypes())
                {
                    if (type == typeof(XPObjectType) || type.IsSubclassOf(typeof(XPBaseObject)))
                    {
                        var typeInfo = XpoDefault.Session.DataLayer.Dictionary.GetClassInfo(type);

                        if (typeInfo.Table != null)
                        {
                            typeInfo.Table.Name = schemaName + "." + typeInfo.Table.Name;

                            foreach (var fk in typeInfo.Table.ForeignKeys)
                            {
                                fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable;
                            }
                        }
                    }

                }
            }

        }
        private void button2_Click(object sender, EventArgs e)
        {
           

            string structureID = textBox1.Text;
            Dictionary<int, object> dic = new Dictionary<int, object>();
            string next = EntityIdentifierStructure.GetNextEntityid(structureID, dic);
           MessageBox.Show(next);
        }

        private void button1_Click(object sender, EventArgs e)
        {
           List<Guid> xx = EntityIdentifierStructure.GetAllIdentifierStructures();
           foreach (Guid item in xx)
           {
               MessageBox.Show(item.ToString());
          }
        }
    }
}
