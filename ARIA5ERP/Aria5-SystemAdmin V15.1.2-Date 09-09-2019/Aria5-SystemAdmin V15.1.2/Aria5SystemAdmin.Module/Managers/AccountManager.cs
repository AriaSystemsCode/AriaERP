using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria5.DevExpress.MainSystem.Module.Managers;
using DevExpress.ExpressApp.Security.Strategy;
using Aria5SystemAdmin.Module.DataTypes;
using System.Net;
using Microsoft.WindowsAzure.Storage;
using System.IO;
using Microsoft.WindowsAzure.Storage.Auth;
using Microsoft.WindowsAzure.Storage.Blob;


namespace Aria5SystemAdmin.Module.Managers
{
    public class AccountManager
    {

      
        public string GetAccountStatus(Guid AccountParam)
        {
                       
            Session session = XpoDefault.Session;

            Account _Account = session.FindObject<Account>(CriteriaOperator.Parse("[Oid] = '" + AccountParam + "'"));

            if (_Account == null)
            {
                throw new Exception("|Invalid account Oid|");
            }
            else
            {
                return _Account.Status.ToString();
            }
        }


        public Account GetAccountObject(Guid AccountParam)
        {

            Session session = XpoDefault.Session;

            Account _Account = session.FindObject<Account>(CriteriaOperator.Parse("[Oid] = '" + AccountParam + "'"));

            Account _Account1 = session.FindObject<Account>(CriteriaOperator.Parse("Name = 'Best Buy'"));
            if (_Account == null)
            {
                throw new Exception("|Invalid account Oid|");
            }
            else
            {
                return _Account;
            }
        }

        public NewAccount GetNewAccountObject(Guid account)
        {
            NewAccount newAccount = new NewAccount();
            Session session = XpoDefault.Session;

            Account accountBO = session.FindObject<Account>(CriteriaOperator.Parse("[Oid] = '" + account + "'"));
            Person personBO = session.FindObject<Person>(CriteriaOperator.Parse("[ParentContact] = '" + account + "'"));
            ContactAddress contactAddressBO = session.FindObject<ContactAddress>(CriteriaOperator.Parse("[Contact] = '" + account + "'"));
            ContactPhone contactPhoneBO = session.FindObject<ContactPhone>(CriteriaOperator.Parse("[Contact] = '" + account + "'"));

            if (accountBO == null || personBO == null || contactAddressBO == null || contactPhoneBO == null)
            {
                throw new Exception("|Invalid account Oid|");
            }
            else
            {
                //account
                newAccount.AccountName = accountBO.Name;
                newAccount.TradeName = accountBO.TradeName;
                newAccount.Guide = accountBO.Guide;
                newAccount.InternalContactNo = accountBO.InternalContactNo;
                newAccount.AccountDescription = accountBO.Description;
                if (accountBO.Status != null)
                    newAccount.Status = accountBO.Status;

                  //Person
                newAccount.PersonOid              =   personBO.Oid;
                newAccount.PersonName             =   personBO.Name;         
                newAccount.FirstName              =   personBO.FirstName;       
                newAccount.LastName               =   personBO.LastName;       
                newAccount.MiddleName             =   personBO.MiddleName;    
                newAccount.NickName               =   personBO.NickName;       
                newAccount.BirthDate              =   personBO.BirthDate;   
                newAccount.EMailAddress           =   personBO.EMailAddress; 
                newAccount.WebPageAddress         =   personBO.WebPageAddress;
                newAccount.PersonDescription      =   personBO.Description;

                if(personBO.Department  != null)
                {
                Department _department = session.FindObject<Department>(CriteriaOperator.Parse("[Oid] == '" + personBO.Department.Oid + "'"));
                newAccount.Department = _department;
                }
                if (personBO.Position != null)
                {
                    Position _position = session.FindObject<Position>(CriteriaOperator.Parse("[Oid] == '" + personBO.Position.Oid + "'"));
                    newAccount.Position = _position;
                }
                newAccount.DepartmentName         =   personBO.DepartmentName;  
                newAccount.PositionTitle          =   personBO.PositionTitle;   
                newAccount.SSN                    =   personBO.SSN;             
                newAccount.SpouseName             =   personBO.SpouseName;      
                newAccount.Salutation             =   personBO.Salutation;
                newAccount.TitleOfCourtesy        =   personBO.TitleOfCourtesy.ToString();

                //phone
                if (contactPhoneBO.PhoneType != null)
                {
                    PhoneType _PhoneType = session.FindObject<PhoneType>(CriteriaOperator.Parse("[Oid] == '" + contactPhoneBO.PhoneType.Oid + "'"));
                    newAccount.PhoneType = _PhoneType;
                }
                newAccount.PhoneTypeDescription = contactPhoneBO.PhoneTypeDescription;
                newAccount.PhoneNumber = contactPhoneBO.PhoneNumber;
                newAccount.Extension = contactPhoneBO.Extension;

                //address
                newAccount.AddressTypeDescription    = contactAddressBO.AddressTypeDescription;
                newAccount.AddressLine1              = contactAddressBO.AddressLine1;
                newAccount.AddressLine2              = contactAddressBO.AddressLine2;
                newAccount.AddressLine3              = contactAddressBO.AddressLine3;
                newAccount.City                      = contactAddressBO.City;
                newAccount.State                     = contactAddressBO.State;
                newAccount.PostalCode                = contactAddressBO.PostalCode;
                newAccount.Country                   = contactAddressBO.Country;
                newAccount.CountryCode               = contactAddressBO.CountryCode;
                newAccount.RegionName                = contactAddressBO.RegionName;
            }

            return newAccount;
        }

        //Mina.B 2015-08-05 Check if email exist before [Begin]
        public bool IsEmailExist(string email)
        {
            Session session = XpoDefault.Session;
            Person person = session.FindObject<Person>(CriteriaOperator.Parse("EMailAddress == '" + email + "'"));
            if (person == null)
            {
                return false;
            }
            else
            {
                return true;
            }
        }
        //Mina.B 2015-08-05 Check if email exist before [End]

        public Guid RegisterNewAccount(Account account, Person person, ContactAddress contactAddress, ContactPhone contactPhone)
        {

            Session session = XpoDefault.Session;

            ValidateRequiredFields(account, person, contactAddress, contactPhone);
         
            List<string> _valuesLengthList = new List<string>();

            Person ValidEmail = session.FindObject<Person>(CriteriaOperator.Parse("EMailAddress == '" + person.EMailAddress + "'"));

            if (ValidEmail == null)
            {
                //if (_requiredFieldList.Count == 0 && _valuesLengthList.Count == 0)
                
                {
                    string IdentifierStructureValue = IdentifierStructureStub();

                    if (IdentifierStructureValue != string.Empty)
                    {
                        #region save account data.
                        
                        account.Id = IdentifierStructureValue;
                        account.Save();

                        #endregion save account data.

                        #region save person data.

                        person.Id = IdentifierStructureValue;
                        person.ParentContact = account.Oid;
                        person.Save();

                        // Add by Mahmoud to add person to account persions collection
                        account.Persons.Add(person);
                        

                        #endregion save person data.

                        #region save phone data.

                        // Add another phone to contact
                        contactPhone.Contact = person;
                        contactPhone.Save();

                        contactPhone.Contact = account;
                        contactPhone.Save();

                        #endregion save phone data.

                        #region save address data.

                        contactAddress.Contact = person;
                        contactAddress.Save();

                        contactAddress.Contact = account;
                        contactAddress.Save();

                        EntityAddress eaddress = new EntityAddress(session);
                        eaddress.AddressLine1 = contactAddress.AddressLine1;
                        eaddress.AddressLine2 = contactAddress.AddressLine2;
                        eaddress.AddressLine3 = contactAddress.AddressLine3;
                        eaddress.AddressType = contactAddress.Addresstype;
                        eaddress.Country = contactAddress.Country;
                        eaddress.City = contactAddress.City;
                        eaddress.Name = contactAddress.Name;
                        if(contactAddress.Region != null) eaddress.Region = contactAddress.Region.Description;
                        eaddress.PostalCode = contactAddress.PostalCode;
                        eaddress.Save();

                        account.EntityAddresses.Add(eaddress);
                        account.Save();

                        #endregion save address data.

                        #region save user
                        AriaSecuritySystemUser ariaSecuritySystemUser = session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("UserName = '" + person.EMailAddress + "' "));
                        if (ariaSecuritySystemUser != null)
                        {
                            throw new Exception("|User Name already exists|");
                        }
                        AriaSecuritySystemUser newUser = new AriaSecuritySystemUser(session);
                        newUser.Account = account;
                        newUser.UserName = person.EMailAddress;
                        AriaUserManager.AddUserManager(newUser);

                        //HIA [Begin] step 1 
                        //AriaUserManager.RestPassword(person.EMailAddress);
                        //HIA [End]

                        #endregion
                    
                        #region save 1TouchAway Support user
                        //SARA.N,1 [Tracking# + Aria5-DevExpress-Account]_Programming, 2-08-2015 [START]
                        AriaSecuritySystemUser OneTouchAwaySupportUser = new AriaSecuritySystemUser(session);
                        OneTouchAwaySupportUser.Account = account;
                        OneTouchAwaySupportUser.UserName = account.AccountCode.Trim().ToUpper() + "_1TouchAway";
                        OneTouchAwaySupportUser.SetPassword("1TouchAway");
                        AriaUserManager.AddUserManager(OneTouchAwaySupportUser);

                        //SARA.N,1 [Tracking# + Aria5-DevExpress-Account]_Programming, 2-08-2015 [END]
                        #endregion

                        #region assign role to user

                        XPCollection<AriaSecuritySystemRole> ariaSecuritySystemRoles = new XPCollection<AriaSecuritySystemRole>(session);
                        ariaSecuritySystemRoles.Load();
                        AriaRoleManager ariaRoleManager = new AriaRoleManager();
                        foreach (AriaSecuritySystemRole item in ariaSecuritySystemRoles)
                        {

                            if (item.IsAdministrative == true)
                            {
                                ariaRoleManager.AssignRoleToUser(newUser.Oid, item.Oid);

                                break;
                            }


                        }
                        #endregion
                    }
                }
            }

            return account.Oid;
        }

        #region stub
        public void ValidateRequiredFields(Account account, Person person, ContactAddress contactAddress, ContactPhone contactPhone)
        {
            List<string> RequiredList = new List<string>();
            string exception = "";
            string exceptionMsg = "";
            if (person.EMailAddress == string.Empty)
            {
                //throw new Exception
                RequiredList.Add("Email");
            }

            if (person.FirstName == string.Empty)
            {
                RequiredList.Add("First Name");
            }

            if (person.LastName == string.Empty)
            {
                RequiredList.Add("Last Name");
            }

            if (account.Name == string.Empty)
            {
                RequiredList.Add("Account Name");
            }

            if (contactPhone.PhoneNumber == string.Empty)
            {
                RequiredList.Add("Phone Number");
            }

            if (contactAddress.AddressLine1 == string.Empty)
            {
                RequiredList.Add("Address Line 1");
            }

            if (contactAddress.City == string.Empty)
            {
                RequiredList.Add("City");
            }


            //if (contactAddress.State == string.Empty)
            //{
                //RequiredList.Add("State");
            //}


            //if (contactAddress.PostalCode == string.Empty)
            //{
                //RequiredList.Add("Postal Code");
            //}


            //if (contactAddress.Contact == null)
            //{
            //    RequiredList.Add("Contact");
            //}

            if (RequiredList.Count != 0)
            {
                foreach (string item in RequiredList)
                {
                    exceptionMsg = exceptionMsg + ", " + item;

                }
                if (String.IsNullOrEmpty(exceptionMsg) == false)
                {
                    exceptionMsg = "|Please add required fields :" + exceptionMsg.Substring(1)+"|";
                    throw new Exception(exceptionMsg);

                }


            }

          
        }
    
        public List<string> ValuesLength(string TableName, string FieldName)
        {

            Session session = new Session();
            Person _person = new Person(session);
            Business _business = new Business(session);
            ContactPhone _ContactPhone = new ContactPhone(session);

            List<string> ValiedList = new List<string>();

            switch (TableName)
            {
               
                case "person":
                    {
                        switch (FieldName)
                        {
                            case "FirstName":
                                if (_person.FirstName.Length > 100)
                                {
                                    ValiedList.Add("FirstName length is too long");

                                }
                                break;
                        }
                    }
                    break;

                case "Business":
                    {
                        switch (FieldName)
                        {
                            case "TradeName":
                                if (_business.TradeName.Length > 100)
                                {
                                    ValiedList.Add("TradeName length is too long");

                                }
                                break;
                        }
                    }
                    break;

                case "contactPhone":
                    {
                        switch (FieldName)
                        {
                            case "PhoneNumber":
                                if (_ContactPhone.PhoneNumber.Length > 30)
                                {
                                    ValiedList.Add("Phone Number length is too long");

                                }
                                break;
                        }
                    }
                    break;
            }



            return ValiedList;
        }

        public string IdentifierStructureStub()
        {

            List<string> guidList = new List<string>();
            string returnValue = "";
            string nextId = "";
            Dictionary<int, object> seg = new Dictionary<int, object>();

            guidList = IdentifierStructureManager.GetAllIdentifierStructures();
            nextId = guidList.FirstOrDefault().ToString();
         
            returnValue = IdentifierStructureManager.GetNextId(nextId, seg);
        
            return returnValue;


        }
        #endregion

        //HIA [Begin] 
        public static bool  UnRegisterAccountPerson(Account account, Person person)
        {
             bool returnvalue = false;

             Session session = XpoDefault.Session;

             ContactPhone contactPhone = session.FindObject<ContactPhone>(CriteriaOperator.Parse("Contact='" + person.Oid + "'"));
             contactPhone.Delete();

             Address address = session.FindObject<Address>(CriteriaOperator.Parse("Contact='" + person.Oid + "'"));
             address.Delete();

             AriaSecuritySystemUser ariaSecuritySystemUser = session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("Account='" + account.Oid + "' and UserName='"+person.EMailAddress+"'"));
             ariaSecuritySystemUser.Delete();

             person.Delete();
             returnvalue = true;

             return returnvalue;
        
        }

        public static bool UnRegisterRole(Guid account)
        {
             bool returnvalue = false;

             Session session = XpoDefault.Session;
             string EMailAddress = "sara.m@ariasystems.biz";

             AriaSecuritySystemUser ariaSecuritySystemUser1 = new AriaSecuritySystemUser(session);
           //  ariaSecuritySystemUser1.Account = session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("Oid='" + account +"'")); 
             //ariaSecuritySystemUser1.Oid = Guid.NewGuid();
             ariaSecuritySystemUser1.UserName = "a";
             ariaSecuritySystemUser1.Save();

             XPCollection<SecuritySystemUser> ariaSecuritySystemUsers = new XPCollection<SecuritySystemUser>(session);
             ariaSecuritySystemUsers.Load();
            
             AriaSecuritySystemUser ariaSecuritySystemUser = session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("Account='" + account + "' and UserName='" + EMailAddress + "'"));

            
            
             ariaSecuritySystemUser.Delete();

             returnvalue = true;

             return returnvalue;
        
        }
        public static  bool UnRegisterAccount(Account account)
        {
            bool returnvalue = false;
            Session session = XpoDefault.Session;

            List<string> _valuesLengthList = new List<string>();
            Person x = new Person(session);

            XPCollection<Person> persons = new XPCollection<Person>(session, CriteriaOperator.Parse("ParentContact='" + account.Oid + "'"));
            persons.Load();
            foreach(Person person in persons)
            {
                UnRegisterAccountPerson(account, person);

            }

          //  Aria5SystemAdmin.Module.Entities.AccountDeviceManager.UnRegisterAccountDevices(account);

            account.Delete();

            returnvalue = true;

            return returnvalue;
        }


        //HIA [End] 


        //Sara.M 01/26/2015 [Tracking# +Aria5-HTML5-AccountRegistration]_Programming  [Start]


        public List<CategoriesDataType> GetDemoGuideCategories()
        {

        
            Session session = XpoDefault.Session;


            XPCollection entityClassificationCollection = new XPCollection(session, typeof(EntityClassification), CriteriaOperator.Parse("[Name] = 'DEMO'"));
            entityClassificationCollection.Load();
            string EntityClassificationString = "";
            foreach (EntityClassification x in entityClassificationCollection)
            { EntityClassificationString = EntityClassificationString + "'" + x.Oid.ToString() + "',"; }

            EntityClassificationString = EntityClassificationString.Substring(0, EntityClassificationString.Length - 1);

            XPCollection accountCollection = new XPCollection(session, typeof(Account), CriteriaOperator.Parse("[EntityClassification] in (" + EntityClassificationString + ") "));
            accountCollection.Load();


            //XPCollection accountCollection = new XPCollection(session, typeof(Account), CriteriaOperator.Parse("[ClassificationId] = 'DEMO'"));
            //accountCollection.Load();

            List<CategoriesDataType> categoriesNameList = new List<CategoriesDataType>();
            List<Account> accountList = new List<Account>();

            //// return  Category ID and Category Name (Distinct) for all demo accounts with ClassificationID = 'DEMO'


            if (accountCollection.Count != 0)
            {
                foreach (Account account in accountCollection)
                {

                    EntityCategory category = session.FindObject<EntityCategory>(CriteriaOperator.Parse("[CategoryId] = '" + account.CategoryId + "'"));
                    CategoriesDataType guideCategories = new CategoriesDataType();

                    guideCategories.CategoryId = category.CategoryId;
                    guideCategories.CategoryName = category.Name;

                    if (!categoriesNameList.Any(x => x.CategoryId == guideCategories.CategoryId))
                        categoriesNameList.Add(guideCategories);

                }
            }

            return categoriesNameList;

        }


        public List<Account> GetAllDemoAccounts()
        {

            Session session = XpoDefault.Session;

            //Account demoAccount = new Account(session);

            List<Account> accountList = new List<Account>();
            // If the Category is not sent, demo accounts for all  //categories should be returned.
            XPCollection entityClassificationCollection = new XPCollection(session, typeof(EntityClassification), CriteriaOperator.Parse("[Name] = 'DEMO'"));
            entityClassificationCollection.Load();
            string EntityClassificationString = "";
            foreach(EntityClassification x in entityClassificationCollection )
            { EntityClassificationString = EntityClassificationString +"'"+ x.Oid.ToString() + "',"; }

            EntityClassificationString = EntityClassificationString.Substring(0, EntityClassificationString.Length - 1);

            XPCollection accountCollection = new XPCollection(session, typeof(Account), CriteriaOperator.Parse("[EntityClassification] in (" + EntityClassificationString + ")"));
            accountCollection.Load();

            

            
            //Sara.N,26/08/2015, CheckDemoGuid On FTB [Start]
            if (accountCollection.Count != 0)
            {
                XPCollection FilteredCollection = accountCollection;
                //Set onnection with Azure Website.
                SetConnection();
                // Loop On List Of DemoAccounts List and Filter it
                for (int i = 0; i < FilteredCollection.Count; i++)
                {
                    if (!CheckDemoguidonAzure(((Account)FilteredCollection[i]).Name.ToString()))
                    {
                        FilteredCollection.Remove(FilteredCollection[i]);
                    }
                }
                //FilteredCollectio
                if (FilteredCollection != null)
                {
                    accountCollection = FilteredCollection;
                    foreach (Account account in accountCollection)
                    {
                        accountList.Add(account);
                    }
                }
            }

            return accountList;
        }
        //Sara.N,26/08/2015, CheckDemoGuid On FTB [Start]
        private static CloudStorageAccount storageAccount;
        public static void SetConnection()
        {
            string accountName = "portalvhdsc5pxbhgplf1gf";// System.Configuration.ConfigurationManager.AppSettings["accountName"];
            string accountKey = "cZGvbB0sEQJR4RF19qM4AdPz9opv5k25E9GXUFL7G1OGaZDW592dEKcP/+btSdVsd1F3tYs9bDXwW/kGPM94Bg==";//System.Configuration.ConfigurationManager.AppSettings["accountKey"];
            StorageCredentials creds = new StorageCredentials(accountName, accountKey);
            storageAccount = new CloudStorageAccount(creds, true);
        }

        private bool CheckDemoguidonAzure(string guideName)
        {

            CloudBlobClient blobClient = storageAccount.CreateCloudBlobClient();
            // Retrieve a reference to a container. 
            CloudBlobContainer container = blobClient.GetContainerReference("demos");
            CloudBlobDirectory dir = container.GetDirectoryReference(guideName);
            CloudBlockBlob blobDatabase = dir.GetBlockBlobReference("1touchawaydb.db");
            if (blobDatabase.Exists() & blobDatabase.Properties.Length > 0)
            {
                return true;
            }
            else
            {
                return false;
            }


        }
        //Sara.N,26/08/2015, CheckDemoGuid On AzureWebsite [End]

        public List<Account> GetDemoAccountsForCategory(string CategoryID)
        {
            Session session = XpoDefault.Session;

            Account demoAccount = new Account(session);
            List<Account> accountList = new List<Account>();

            if (CategoryID != null)
            {

                //returns the Category Id, Category Name, Account ID and  Account Name for all demo accounts (Accounts with  //ClassificationID = 'DEMO') and belong to specific category.


                
                
                XPCollection entityClassificationCollection = new XPCollection(session, typeof(EntityClassification), CriteriaOperator.Parse("[Name] = 'DEMO'"));
                entityClassificationCollection.Load();
                string EntityClassificationString = "";
                foreach (EntityClassification x in entityClassificationCollection)
                { EntityClassificationString = EntityClassificationString + "'" + x.Oid.ToString() + "',"; }

                EntityClassificationString = EntityClassificationString.Substring(0, EntityClassificationString.Length - 1);

                XPCollection accountCollection = new XPCollection(session, typeof(Account), CriteriaOperator.Parse("[EntityClassification] in (" + EntityClassificationString + ") AND [CategoryId] = '" + CategoryID + "'"));
                accountCollection.Load();

                //XPCollection accountCollection = new XPCollection(session, typeof(Account), CriteriaOperator.Parse("[ClassificationId] = 'DEMO' AND [CategoryId] = '" + CategoryID + "'"));
                //accountCollection.Load();
                foreach (Account account in accountCollection)
                {
                    accountList.Add(account);
                }
            }



            //return demo list of Accounts;

            return accountList;
        }

        public List<Account> GetDemoAccountsForStatus(string StatusID)
        {
            Session session = XpoDefault.Session;

            Account demoAccount = new Account(session);
            List<Account> accountList = new List<Account>();

            if (StatusID != null)
            {

                //returns the Category Id, Category Name, Account ID and  Account Name for all demo accounts (Accounts with  //ClassificationID = 'DEMO') and belong to specific category.

                XPCollection entityClassificationCollection = new XPCollection(session, typeof(EntityClassification), CriteriaOperator.Parse("[Name] = 'DEMO'"));
                entityClassificationCollection.Load();
                string EntityClassificationString = "";
                foreach (EntityClassification x in entityClassificationCollection)
                { EntityClassificationString = EntityClassificationString + "'" + x.Oid.ToString() + "',"; }

                EntityClassificationString = EntityClassificationString.Substring(0, EntityClassificationString.Length - 1);

                XPCollection accountCollection = new XPCollection(session, typeof(Account), CriteriaOperator.Parse("[EntityClassification] in (" + EntityClassificationString + ") AND [StatusId] = '" + StatusID + "'"));
                accountCollection.Load();


                //XPCollection accountCollection = new XPCollection(session, typeof(Account), CriteriaOperator.Parse("[ClassificationId] = 'DEMO' AND [StatusId] = '" + StatusID + "'"));
                //accountCollection.Load();
                foreach (Account account in accountCollection)
                {
                    accountList.Add(account);
                }
            }



            //return demo list of Accounts;

            return accountList;
        }


        //Sara.M 01/26/2015 [Tracking# +Aria5-HTML5-AccountRegistration]_Programming [End]

        //sara.M [Tracking# +Aria5-Windows8Xaml-Login]_Programming [Start]
        public CategoriesDataType GetCategoryByAccount(Guid Account)
        {

            Session session = XpoDefault.Session;
            Account _Account = session.FindObject<Account>(CriteriaOperator.Parse("[Oid] = '" + Account + "'"));
            EntityCategory category = session.FindObject<EntityCategory>(CriteriaOperator.Parse("[CategoryId] = '" + _Account.CategoryId + "'"));
            CategoriesDataType guideCategories = new CategoriesDataType();

            guideCategories.CategoryId = category.CategoryId;
            guideCategories.CategoryName = category.Name;
            guideCategories.CategoryOid = category.Oid;

            return guideCategories;

        }

        //sara.M [Tracking# +Aria5-Windows8Xaml-Login]_Programming [End]
    }
}
