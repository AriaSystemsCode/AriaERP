using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Services;
using DevExpress.Data.Filtering;
using Aria5SystemAdmin.Module.Entities;
using Aria5SystemAdmin.Module.Managers;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
using Aria5SystemAdmin.Module.DataTypes;
using System.Web.Configuration;



namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.WebServices
{
    /// <summary>
    /// Summary description for AccountWebService
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    // [System.Web.Script.Services.ScriptService]
    public class AccountWebService : System.Web.Services.WebService
    {

        [WebMethod(EnableSession = true)]
        public string GetAccountStatus(Guid account)
        {
            Helpers.SchemaHelper.SwitchSchema();

            AccountManager _entitiesAccountRegistration = new AccountManager();
            return _entitiesAccountRegistration.GetAccountStatus(account);
        }


        [WebMethod(EnableSession = true)]
        public AccountMarshalling GetAccountObject(Guid account)
        {
            Helpers.SchemaHelper.SwitchSchema();
            AccountManager entitiesAccountRegistration = new AccountManager();
            AccountMarshalling accountMarshalling = new AccountMarshalling();
            Session session = XpoDefault.Session;
            Account accountBO = entitiesAccountRegistration.GetAccountObject(account);

            accountMarshalling.Name = accountBO.Name;
            if(accountBO.Status != null) accountMarshalling.StatusSer = accountBO.Status.ToString();
            accountMarshalling.TradeName = accountBO.TradeName;
            accountMarshalling.InternalContactNo = accountBO.InternalContactNo;
            accountMarshalling.Guide = accountBO.Guide;

            //sara 29/1/2015
            accountMarshalling.Id = accountBO.Id;

            if (accountBO.EntityCategory != null) accountMarshalling.CategoryOid = accountBO.EntityCategory.Oid;
            
            return accountMarshalling;
        }



        [WebMethod(EnableSession = true)]
        public NewAccountMarshalling GetNewAccountObject(Guid account)
        {
            Helpers.SchemaHelper.SwitchSchema();
            AccountManager entitiesAccountRegistration = new AccountManager();
            NewAccountMarshalling newAccountMarshalling = new NewAccountMarshalling();
            Session session = XpoDefault.Session;

            NewAccount newAccount = entitiesAccountRegistration.GetNewAccountObject(account);
            newAccountMarshalling.AccountName = newAccount.AccountName;
            newAccountMarshalling.StatusSer = newAccount.Status.ToString();
            newAccountMarshalling.TradeName = newAccount.TradeName;
            newAccountMarshalling.InternalContactNo = newAccount.InternalContactNo;
            newAccountMarshalling.Guide = newAccount.Guide;
            newAccountMarshalling.AccountDescription = newAccount.AccountDescription;


            newAccountMarshalling.PersonOid = newAccount.PersonOid;
            newAccountMarshalling.PersonName = newAccount.PersonName;
            newAccountMarshalling.FirstName = newAccount.FirstName;
            newAccountMarshalling.LastName = newAccount.LastName;
            newAccountMarshalling.MiddleName = newAccount.MiddleName;
            newAccountMarshalling.NickName = newAccount.NickName;
            newAccountMarshalling.BirthDate = newAccount.BirthDate;
            newAccountMarshalling.EMailAddress = newAccount.EMailAddress;
            newAccountMarshalling.WebPageAddress = newAccount.WebPageAddress;
            newAccountMarshalling.PersonDescription = newAccount.PersonDescription;


            if (newAccount.Department != null)
            {
                Department _department = session.FindObject<Department>(CriteriaOperator.Parse("[Oid] == '" + newAccount.Department + "'"));
                newAccountMarshalling.Department = _department.Oid;
            }
            if (newAccount.Position != null)
            {
                Position _position = session.FindObject<Position>(CriteriaOperator.Parse("[Oid] == '" + newAccount.Position + "'"));
                newAccountMarshalling.Position = _position.Oid;
            }
            newAccountMarshalling.DepartmentName = newAccount.DepartmentName;
            newAccountMarshalling.PositionTitle = newAccount.PositionTitle;
            newAccountMarshalling.SSN = newAccount.SSN;
            newAccountMarshalling.SpouseName = newAccount.SpouseName;
            newAccountMarshalling.Salutation = newAccount.Salutation;
            newAccountMarshalling.TitleOfCourtesy = newAccount.TitleOfCourtesy.ToString();

            //phone
            if (newAccount.PhoneType != null)
            {
                PhoneType _PhoneType = session.FindObject<PhoneType>(CriteriaOperator.Parse("[Oid] == '" + newAccount.PhoneType + "'"));
                newAccountMarshalling.PhoneType = _PhoneType.Oid;
            }
            newAccountMarshalling.PhoneTypeDescription = newAccount.PhoneTypeDescription;
            newAccountMarshalling.PhoneNumber = newAccount.PhoneNumber;
            newAccountMarshalling.Extension = newAccount.Extension;

            //address
            newAccountMarshalling.AddressTypeDescription = newAccount.AddressTypeDescription;
            newAccountMarshalling.AddressLine1 = newAccount.AddressLine1;
            newAccountMarshalling.AddressLine2 = newAccount.AddressLine2;
            newAccountMarshalling.AddressLine3 = newAccount.AddressLine3;
            newAccountMarshalling.City = newAccount.City;
            newAccountMarshalling.State = newAccount.State;
            newAccountMarshalling.PostalCode = newAccount.PostalCode;
            newAccountMarshalling.Country = newAccount.Country;
            newAccountMarshalling.CountryCode = newAccount.CountryCode;
            newAccountMarshalling.RegionName = newAccount.RegionName;

            return newAccountMarshalling;
        }

        [WebMethod(EnableSession = true)]
        public RegisterNewAcountResultMarshalling RegisterNewAccount(AccountMarshalling account, PersonMarshalling person, ContactAddressMarshalling contactAddress, ContactPhoneMarshalling contactPhone)
        {
            Helpers.SchemaHelper.SwitchSchema();

            AccountManager _entitiesAccountRegistration = new AccountManager();
            Session session = XpoDefault.Session;

            #region account
            Account accountBO = new Account(session);
            accountBO.Name = account.Name;
            accountBO.TradeName = account.Name;
            accountBO.Description = account.Name;
            accountBO.InternalContactNo = account.InternalContactNo;
            accountBO.Guide = account.Guide;
            accountBO.WebPageAddress = person.WebPageAddress;

            EntityStatus _entityStatus = session.FindObject<EntityStatus>(CriteriaOperator.Parse("[StatusID] == 'NEW'"));
            if (_entityStatus == null)
            {
                _entityStatus = new EntityStatus(session);
                _entityStatus.StatusID = "NEW";
                _entityStatus.Description = "New";
                _entityStatus.Save();

            }
            accountBO.Status = _entityStatus;
            accountBO.StatusId = _entityStatus.StatusID;

            #endregion

            #region person
            Person personBO = new Person(session);

            if (person != null)
            {
                if (person.Department != null)
                {
                    Department _department = session.FindObject<Department>(CriteriaOperator.Parse("[Oid] == '" + person.Department + "'"));
                    personBO.Department = _department;
                }

                if (person.Department != null)
                {
                    Position _position = session.FindObject<Position>(CriteriaOperator.Parse("[Oid] == '" + person.Position + "'"));
                    personBO.Position = _position;
                }

            }

            personBO.Status = _entityStatus;
            personBO.StatusId = _entityStatus.StatusID;
            personBO.Name = person.FirstName + " " + person.LastName;
            personBO.Description = person.FirstName + " " + person.LastName;
            personBO.FirstName = person.FirstName;
            personBO.LastName = person.LastName;
            personBO.MiddleName = person.MiddleName;
            personBO.NickName = person.NickName;
            personBO.BirthDate = person.BirthDate;
            personBO.EMailAddress = person.EMailAddress;
            personBO.WebPageAddress = person.WebPageAddress;

            personBO.DepartmentName = person.DepartmentName;
            personBO.PositionTitle = person.PositionTitle;
            personBO.SSN = person.SSN;
            personBO.SpouseName = person.SpouseName;
            personBO.Salutation = person.Salutation;
            personBO.TitleOfCourtesy = ((TitleOfCourtesy)Enum.Parse(typeof(TitleOfCourtesy), person.TitleOfCourtesy));

            #endregion

            #region phone

            ContactPhone contactPhoneBO = new ContactPhone(session);
            XPCollection phoneTypeCollection = new XPCollection(XpoDefault.Session, typeof(PhoneType), CriteriaOperator.Parse("[Description] == 'Business'"));
            phoneTypeCollection.Load();
            if (phoneTypeCollection.Count > 0)
            {
                contactPhoneBO.PhoneType = phoneTypeCollection[0] as PhoneType;
            }

            else
            {
                PhoneType phoneType = new PhoneType(session);
                phoneType.Description = "Business";
                phoneType.Id = "BUSINESS";

                phoneType.Save();

                contactPhoneBO.PhoneType = phoneType;

            }

            contactPhoneBO.PhoneTypeDescription = "Business";
            contactPhoneBO.PhoneNumber = contactPhone.PhoneNumber;
            contactPhoneBO.Extension = contactPhone.Extension;
            #endregion

            #region address

            ContactAddress addressBO = new ContactAddress(session);
            XPCollection AddressTypeCollection = new XPCollection(XpoDefault.Session, typeof(ContactAddress), CriteriaOperator.Parse("[Description] == 'Business'"));
            AddressTypeCollection.Load();
            if (AddressTypeCollection.Count > 0)
            {
                addressBO.Addresstype = AddressTypeCollection[0] as AddressType;
            }

            else
            {
                AddressType addresstype = new AddressType(session);
                addresstype.Description = "Business";
                addresstype.Id = "BUSINESS";
                addresstype.Save();

                addressBO.Addresstype = addresstype;

            }
            addressBO.AddressTypeDescription = "Business";
            addressBO.AddressLine1 = contactAddress.AddressLine1;
            addressBO.AddressLine2 = contactAddress.AddressLine2;
            addressBO.AddressLine3 = contactAddress.AddressLine3;
            addressBO.City = contactAddress.City;
            addressBO.State = contactAddress.State;
            addressBO.PostalCode = contactAddress.PostalCode;
        
            addressBO.Country = contactAddress.Country;
            addressBO.CountryCode = contactAddress.CountryCode;
            addressBO.RegionName = contactAddress.RegionName;
            #endregion

            _entitiesAccountRegistration.RegisterNewAccount(accountBO, personBO, addressBO, contactPhoneBO);

            RegisterNewAcountResultMarshalling result = new RegisterNewAcountResultMarshalling();
            result.AccountGuid = accountBO.Oid;
            result.PersonGuid = personBO.Oid;

            return result;
        }

        //Sara.M 01/26/2015 [Tracking# +Aria5-HTML5-AccountRegistration]_Programming  [Start]
        [WebMethod(EnableSession = true)]
        public List<CategoriesDataType> GetDemoGuideCategories()
        {

            Helpers.SchemaHelper.SwitchSchema();
            Session session = XpoDefault.Session;

            AccountManager entitiesAccountRegistration = new AccountManager();
         
            List<CategoriesDataType> returnGuideCategoriesList = entitiesAccountRegistration.GetDemoGuideCategories();

            return returnGuideCategoriesList;
        }

        [WebMethod(EnableSession = true)]
        public List<AccountMarshalling> GetAllDemoAccounts()
        {
            Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;

            AccountManager entitiesAccountRegistration = new AccountManager();
            List<AccountMarshalling> accountMarshallingList = new List<AccountMarshalling>();



            List<Account> accountList = entitiesAccountRegistration.GetAllDemoAccounts();

            foreach (var account in accountList)
            {
                AccountMarshalling accountMarshalling = new AccountMarshalling();

                accountMarshalling.Name = account.Name;
                accountMarshalling.Id = account.Id;
                if(account.Status != null) accountMarshalling.StatusSer = account.Status.ToString();
                accountMarshalling.TradeName = account.TradeName;
                accountMarshalling.InternalContactNo = account.InternalContactNo;
                accountMarshalling.Guide = account.Guide;
                accountMarshalling.AccountOid = account.Oid;
                accountMarshalling.CategoryId = account.CategoryId;
                //sara.N , Check Issue in Web service 24-01-2016 [Start]

                // accountMarshalling.CategoryName = session.FindObject<EntityCategory>(CriteriaOperator.Parse("[CategoryId] = '" + account.CategoryId + "'")).Name;
                 if (account.CategoryId!=null)    accountMarshalling.CategoryName = session.FindObject<EntityCategory>(CriteriaOperator.Parse("[CategoryId] = '" + account.CategoryId + "'")).Name;

                //sara.N , Check Issue in Web service 24-01-2016 [Start]

                accountMarshallingList.Add(accountMarshalling);

            }




            return accountMarshallingList;
        }

        [WebMethod(EnableSession = true)]
        public List<AccountMarshalling> GetDemoAccountsForCategory(string CategoryID)
        {
            Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;

            AccountManager entitiesAccountRegistration = new AccountManager();
            List<AccountMarshalling> accountMarshallingList = new List<AccountMarshalling>();

            List<Account> accountList = entitiesAccountRegistration.GetDemoAccountsForCategory(CategoryID);

            foreach (var account in accountList)
            {
                AccountMarshalling accountMarshalling = new AccountMarshalling();

                accountMarshalling.Name = account.Name;
                accountMarshalling.Id = account.Id;
                accountMarshalling.StatusSer = account.Status.ToString();
                accountMarshalling.TradeName = account.TradeName;
                accountMarshalling.InternalContactNo = account.InternalContactNo;
                accountMarshalling.Guide = account.Guide;
                accountMarshalling.AccountOid = account.Oid;
                accountMarshalling.CategoryId = account.CategoryId;
                accountMarshalling.CategoryName = session.FindObject<EntityCategory>(CriteriaOperator.Parse("[CategoryId] = '" + account.CategoryId + "'")).Name;

                accountMarshallingList.Add(accountMarshalling);
            }




            return accountMarshallingList;
        }


        [WebMethod(EnableSession = true)]
        public List<AccountMarshalling> GetDemoAccountsForStatus(string StatusID)
        {
            Helpers.SchemaHelper.SwitchSchema();
            Session session = XpoDefault.Session;

            AccountManager entitiesAccountRegistration = new AccountManager();
            List<AccountMarshalling> accountMarshallingList = new List<AccountMarshalling>();

            List<Account> accountList = entitiesAccountRegistration.GetDemoAccountsForStatus(StatusID);

            foreach (var account in accountList)
            {
                AccountMarshalling accountMarshalling = new AccountMarshalling();

                accountMarshalling.Name = account.Name;
                accountMarshalling.Id = account.Id;
                accountMarshalling.StatusSer = account.Status.ToString();
                accountMarshalling.TradeName = account.TradeName;
                accountMarshalling.InternalContactNo = account.InternalContactNo;
                accountMarshalling.Guide = account.Guide;
                accountMarshalling.AccountOid = account.Oid;
                accountMarshalling.CategoryId = account.CategoryId;
                accountMarshalling.CategoryName = session.FindObject<EntityCategory>(CriteriaOperator.Parse("[CategoryId] = '" + account.CategoryId + "'")).Name;

                accountMarshallingList.Add(accountMarshalling);
            }




            return accountMarshallingList;
        }

        //Sara.M 01/26/2015 [Tracking# +Aria5-HTML5-AccountRegistration]_Programming  [End]

        //sara.M [Tracking# +Aria5-Windows8Xaml-Login]_Programming [Start]
        [WebMethod(EnableSession = true)]
        public CategoriesDataType GetCategoryByAccount(Guid account)
        {

            Helpers.SchemaHelper.SwitchSchema();
            Session session = XpoDefault.Session;

            AccountManager entitiesAccountRegistration = new AccountManager();
            CategoriesDataType returnCategory = entitiesAccountRegistration.GetCategoryByAccount(account);

            return returnCategory;
        }

        //sara.M [Tracking# +Aria5-Windows8Xaml-Login]_Programming [End]


        [WebMethod(EnableSession = true)]
        public bool CheckEmail(string email)
        {
            Helpers.SchemaHelper.SwitchSchema();
            AccountManager accountManager = new AccountManager();
            return accountManager.IsEmailExist(email);
        }
    }
}
