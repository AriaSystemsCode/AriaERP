//using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
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
    public partial class RegisterNewAccount : Form
    {
        public RegisterNewAccount()
        {
            InitializeComponent();
        }

        private void Savebutton_Click(object sender, EventArgs e)
        {

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

           // ServiceReference1.AccountWebServiceSoapClient _accountwebservice = new ServiceReference1.AccountWebServiceSoapClient();
          //  AccountManager _accountwebservice = new AccountManager();

            #region commented code
            //  AccountServiceReference.AccountWebServiceSoapClient _accountwebservice = new AccountServiceReference.AccountWebServiceSoapClient();
            //ServiceReference3.AccountRegistrationWebServiceSoapClient _accountwebservice = new ServiceReference3.AccountRegistrationWebServiceSoapClient();

            //AccountServiceReference.PersonMarshalling Person = new AccountServiceReference.PersonMarshalling();
            //AccountServiceReference.AccountMarshalling Account = new AccountServiceReference.AccountMarshalling();
            //AccountServiceReference.ContactAddressMarshalling Address = new AccountServiceReference.ContactAddressMarshalling();
            //AccountServiceReference.ContactPhoneMarshalling Phone = new AccountServiceReference.ContactPhoneMarshalling();
            //AccountServiceReference.RegisterNewAountResultMarshalling AccountResult = new AccountServiceReference.RegisterNewAountResultMarshalling();


            //MarshallingPerson Person = new MarshallingPerson();
            //AccountSerializable Account = new AccountSerializable();
            //MarshallingContactAddress Address = new MarshallingContactAddress();
            //MarshallingContactPhone Phone = new MarshallingContactPhone();
            //NewAccountResult AccountResult = new NewAccountResult();
            #endregion

            //Person Person = new Person(XpoDefault.Session);
            //Account Account = new Account(XpoDefault.Session);
            //Address Address = new Address(XpoDefault.Session);
            //ContactPhone Phone = new ContactPhone(XpoDefault.Session);
            //RegisterNewAcountResultMarshalling AccountResult = new RegisterNewAcountResultMarshalling();


        //    ServiceReference1.PersonMarshalling Person = new ServiceReference1.PersonMarshalling();
        //    ServiceReference1.AccountMarshalling Account = new ServiceReference1.AccountMarshalling();
        //    ServiceReference1.ContactAddressMarshalling Address = new ServiceReference1.ContactAddressMarshalling();
        //    ServiceReference1.ContactPhoneMarshalling Phone = new ServiceReference1.ContactPhoneMarshalling();
        //    ServiceReference1.RegisterNewAountResultMarshalling AccountResult = new ServiceReference1.RegisterNewAountResultMarshalling();


        //    Person.FirstName = FNtextBox1.Text;
        //    Person.LastName = LNtextBox1.Text;
        //    Person.MiddleName = MNtextBox1.Text;
        //    Person.NickName = NNtextBox1.Text;
        //    Person.EMailAddress = MailtextBox1.Text;
        //    Person.WebPageAddress = WebtextBox1.Text;
        //    Person.BirthDate = dateTimePicker1.Value;
        //    Person.SSN = SSNtextBox1.Text;
        //    Person.DepartmentName = DepartmenttextBox1.Text;
        //    Person.PositionTitle = PositiontextBox1.Text;
        //    Person.TitleOfCourtesy = textBox1.Text;
        //        //((TitleOfCourtesy)Enum.Parse(typeof(TitleOfCourtesy), textBox1.Text)); 
        //    Account.Name = AccountNametextBox1.Text;
        //    Phone.PhoneNumber = PhoneNotextBox1.Text;
        //    Phone.PhoneTypeDescription = PhoneDesctextBox1.Text;
        //    Address.Country = CountrytextBox1.Text;
        //    Address.CountryCode = CountryCodetextBox1.Text;
        //    Address.City = CitytextBox1.Text;
        //    Address.PostalCode = PositiontextBox1.Text;
        //    Address.State = StatetextBox1.Text;
        //    Address.AddressLine1 = AdresstextBox1.Text;


        //AccountResult =   _accountwebservice.RegisterNewAccount(Account, Person, Address, Phone);


        //    //   _accountwebservice.RegisterNewPhone();
        //    // AccountRegistration _accountwebservice = new AccountRegistration();
        //    //  _accountwebservice.RegisterNewAccount(Account, Person, Address, Phone);

        //    //AccountResult.AccountGuid = Account.Oid;
        //    //AccountResult.PersonGuid = Person.Oid;
        //MessageBox.Show(" Account '" + AccountResult.AccountGuid.ToString() + "'");
        //MessageBox.Show(" Account '" + AccountResult.PersonGuid.ToString() + "'");
  
        }

        private void RegisterNewAccount_Load(object sender, EventArgs e)
        {

        }

        private void RegisterNewAccount_FormClosing(object sender, FormClosingEventArgs e)
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
