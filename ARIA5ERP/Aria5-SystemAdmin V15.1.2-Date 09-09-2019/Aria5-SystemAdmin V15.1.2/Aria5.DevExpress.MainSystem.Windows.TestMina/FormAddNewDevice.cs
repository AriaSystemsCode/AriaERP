using Aria5.DevExpress.MainSystem.Module.BusinessObjects;
using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module.Entities;
using DevExpress.Data.Filtering;
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
    public partial class FormAddNewDevice : Form
    {
        private AccountDevice  DeviceInfo;
        private Guid guid3 = new Guid();

        public FormAddNewDevice()
        {
            InitializeComponent();
            //DeviceInfo = new AccountDevice(XpoDefault.Session);

            //textBox2.Text = "3B1BDA5B-E921-433D-A8E9-FB94E000FCEF";
            //textBox15.Text = "d11";

            //textBox3.Text = "09D93727-62BD-4ED7-9CAB-802EF4003DE3";
            //textBox4.Text = "13";
            //textBox5.Text = "d13";
            //textBox6.Text = "m10";
            //textBox7.Text = "d13";
            //textBox8.Text = "d13";
            //textBox9.Text = "d13";
            //textBox10.Text = "d13";
            //textBox11.Text = "d13";
            //textBox12.Text = "d13";
            textBox13.Text = Guid.NewGuid().ToString();

        }

        private void button1_Click(object sender, EventArgs e)
        {
            //XpoDefault.ConnectionString = "Integrated Security=SSPI;Pooling=false;Data Source=TR_MINA\\SQLEXPRESS;Initial Catalog=DeviceManager3";


            //SetConnection();
            //DeviceManager d = new DeviceManager();
            //AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient d = new AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient();
            //if (textBox2.Text != string.Empty && textBox2.Text != null)
            //{

            //    //d.DeviceBusinessObject(textBox1.Text);
            //    Guid guid = new Guid();
            //    guid = Guid.Parse(textBox2.Text);

            //    textBox1.Text = "";
            //    //DeviceInfo = d.GetDeviceBusinessObject(textBox1.Text);
            //    // if ((d.GetDeviceAccountId(textBox1.Text)) != null)
            //    if (d.GetAccountDevices(guid) != null)
            //    {
            //        AccountDeviceServiceReference.AccountDeviceMarshalling[] list = (d.GetAccountDevices(guid));
            //        foreach (AccountDeviceServiceReference.AccountDeviceMarshalling item in list)
            //        {
            //            textBox1.Text = "   " + textBox1.Text + "   " + item.DeviceSignature;
            //        }
            //    }
            //    else
            //    {
            //        textBox2.Text = "NULL";
            //    }
            //    //else textBox2.Text = "NULL";
            //}
            //else
            //{
            //    textBox2.Text = "Please Enter Account Oid";
            //}
        }


        public void AdjustSchema(string schemaName)
        {
            # region Change Schema region

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

            #endregion  Change Schema region

        }



        private void button2_Click(object sender, EventArgs e)
        {
           // XpoDefault.ConnectionString = "Integrated Security=SSPI;Pooling=false;Data Source=TR_MINA\\SQLEXPRESS;Initial Catalog=DeviceManager3";
            StartConnection();
            DeviceInfo = new AccountDevice(XpoDefault.Session);

            //XpoDefault.ConnectionString = "Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;uid=SystemAdminTestingUser1;pwd=aria_123;Pooling=False";
            //AdjustSchema("Test4");
            //Session session = XpoDefault.Session;

            
            Guid guid1 = new Guid();
            Guid guid2 = new Guid();
            guid1 = Guid.NewGuid();
            guid2 = Guid.NewGuid();
            guid3 = Guid.Parse("BC249B6C-F834-4280-B5A0-0710936FE142");

            //Aria5SystemAdmin.Module.BusinessObjects.Account account = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '321D49E6-D8DD-41AA-B017-BD0FB4939AAE'"));

            
            XPCollection devices = new XPCollection(typeof(AccountDevice));

            //CriteriaOperator criteriaAccount = new BinaryOperator("Account.Oid", guid2, BinaryOperatorType.Equal);
            //devices.Criteria = criteriaAccount;
            //devices.Load();

             XPCollection accounts = new XPCollection(XpoDefault.Session, typeof(Account));

             CriteriaOperator criteriaAccount = new BinaryOperator("Oid", Guid.Parse("09D93727-62BD-4ED7-9CAB-802EF4003DE3"), BinaryOperatorType.Equal);
             
             accounts.Criteria = criteriaAccount;
             accounts.Load();
             if (accounts.Count != 0)
             {
                 DeviceInfo.Account = (accounts[0] as Account);
             }
            

            DeviceInfo.ID = textBox4.Text;
            DeviceInfo.ScreenSize = textBox5.Text;
            DeviceInfo.AccountName = textBox6.Text;
            DeviceInfo.Name = textBox7.Text;
            DeviceInfo.AccountId = textBox8.Text;
            DeviceInfo.ASHWID = textBox9.Text;
            DeviceInfo.MACAddress = textBox10.Text;
            DeviceInfo.OperatingSystem = textBox11.Text;
            DeviceInfo.DeviceSignature = textBox12.Text;
            DeviceInfo.Location = Guid.Parse(textBox13.Text);




            //AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient d = new AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient();
            AccountDeviceManager d = new AccountDeviceManager();
            Guid newOid = d.AddNewDevice(DeviceInfo);
            textBox14.Text = newOid.ToString();
        }

        private void Button_Add_New_Device_Click(object sender, EventArgs e)
        {
            FormAddNewAccount formAddNewAccount = new FormAddNewAccount();



            formAddNewAccount.Show();
        }

        public void SetConnection()
        {
            XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=SystemAdminTestingUser1;Password=aria_123";

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

        public void StartConnection()
        {
            XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=SystemAdminTestingUser1;Password=aria_123";

            IdentifierStructure tem2 = new IdentifierStructure(XpoDefault.Session);
            Account temp = new Account(XpoDefault.Session);

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

        private void ButtonGetDeviceAccountID_Click(object sender, EventArgs e)
        {

            //AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient d = new AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient();
            //if (textBox15.Text != string.Empty && textBox15.Text != null)
            //{
            //    textBox1.Text = d.GetDeviceAccountId(textBox15.Text);
            //}
            //else
            //{
            //    textBox15.Text = "Please Enter Device Signature.";
            //}
        }

        private void ButtonGetDeviceOid_Click(object sender, EventArgs e)
        {
            //AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient d = new AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient();
            //if (textBox15.Text != string.Empty && textBox15.Text != null)
            //{
            //    textBox1.Text = d.GetDeviceOid(textBox15.Text).ToString();
            //}
            //else
            //{
            //    textBox2.Text = "Please Enter Device Signature.";
            //}
        }

        private void ButtonGetAccountDeviceObject_Click(object sender, EventArgs e)
        {
            //AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient d = new AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient();
            //if (textBox15.Text != string.Empty && textBox15.Text != null)
            //{
            //    textBox1.Text = d.GetDeviceBusinessObject(textBox15.Text).DeviceSignature;
            //}
            //else
            //{
            //    textBox15.Text = "Please Enter Device Signature.";
            //}
        }

        private void ButtonIsAuthanticateDevice_Click(object sender, EventArgs e)
        {
            //AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient d = new AccountDeviceServiceReference.AccountDeviceWebServiceSoapClient();
            //try
            //{

            //    if (textBox2.Text != string.Empty && textBox2.Text != null)
            //    {
            //        Guid guid1 = new Guid();
            //        guid1 = Guid.Parse("441718E0-DF0E-4C37-BD5C-7A7B472C9F3E");
            //        textBox1.Text = d.AuthanticateDevice(Guid.Parse(textBox2.Text), textBox15.Text).ToString();
            //    }
            //    else
            //    {
            //        textBox2.Text = "Please Enter Device Signature.";
            //    }
            //}
            //catch (Exception ex)
            //{
            //    textBox1.Text = ex.Message;
            //}

        }


    }
}
