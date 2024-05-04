using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Services;
using DevExpress.Xpo;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module.Managers;
using DevExpress.Data.Filtering;
using System.Web.Services.Protocols;
using System.Web.Configuration;
using System.Configuration;


namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.WebServices
{
    /// <summary>
    /// Summary description for AriaUserWebService
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    // [System.Web.Script.Services.ScriptService]
    public class AriaUserWebService : System.Web.Services.WebService
    {
        [WebMethod(EnableSession = true)]
        public bool AddNewUser(AriaUserMarshalling User)
        {
           Helpers.SchemaHelper.SwitchSchema();

            bool Addingstatus = false;

            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
                AriaSecuritySystemUser newUser = new AriaSecuritySystemUser(session);

                Account account = session.FindObject<Account>(CriteriaOperator.Parse("Oid = '" + User.Account + "' "));

                if (account == null)
                {
                    throw new Exception("Account does not exists");
                }

                AriaSecuritySystemUser ariaSecuritySystemUser = session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("UserName = '" + User.UserName + "' "));
                if (ariaSecuritySystemUser != null)
                {
                   throw new Exception("User Name already exists");
                }

                newUser.Account = account;
                newUser.UserName = User.UserName;
                newUser.SetPassword(User.Password);
                
                AriaUserManager.AddUserManager(newUser);
            }

            return Addingstatus;
        }

        [WebMethod(EnableSession = true)]
        public Guid UserAuthentication(string userName, string Password)
        {
           Helpers.SchemaHelper.SwitchSchema();

            Guid Addingstatus = Guid.Empty;

            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
                //try
                //{
                    return AriaUserManager.UserAuthentication(userName, Password);
                //}
                //catch (Exception ex)
                //{

                //    throw new SoapException(ex.Message,
                //     SoapException.ServerFaultCode, "UserAuthentication", ex);
                //}
            }

            return Addingstatus;
        }

        [WebMethod(EnableSession = true)]
        public string GenerateTemporaryPassword(string userName)
        {

           Helpers.SchemaHelper.SwitchSchema();

            string Addingstatus = "";

            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
                return AriaUserManager.GenerateTemporaryPassword(userName);
            }

            return Addingstatus;
        }

        [WebMethod(EnableSession = true)]
        public string GenerateTemporaryPasswordAdmin(string userName)
        {
           Helpers.SchemaHelper.SwitchSchema();

            string Addingstatus = "";

            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
                return AriaUserManager.GenerateTemporaryPasswordAdmin(userName);
            }

            return Addingstatus;
        }

        [WebMethod(EnableSession = true)]
        public void SendPassword(string userName, string newTmpPassword, bool Reset = true)
        {
           Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
                AriaUserManager.SendPassword(userName, newTmpPassword, Reset);
            }
        }

        //Sara.M send feedback [Start]
        [WebMethod(EnableSession = true)]
        public void SendFeedback(string emailFrom, string emailFromPassword, string emailTo, string body)
        {
            Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
                AriaUserManager.SendFeedback(emailFrom, emailFromPassword, emailTo, body);
            }
        }

        //Sara.M send feedback [End]

        [WebMethod(EnableSession = true)]
        public void RestPassword(string userName)
        {
           Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
                AriaUserManager.RestPassword(userName);
            }

        }


        [WebMethod(EnableSession = true)]
        public bool ChangePassword(string userName, string oldPassword, string password)
        {
           Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
               return AriaUserManager.ChangePassword(userName, oldPassword, password);
            }
            return false;

        }

        // Sara.N 06/01/2015 Add WebMethod returns All Active Users [Start]
        [WebMethod(EnableSession = true)]
        public AriaUserMarshalling[] GetAccountUsers(Guid account)
        {
            //1- Get Current Account Session
            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
                //1- Call ariaUser Manger toget all users of the Current Account.
                AriaSecuritySystemUser[] result = AriaUserManager.GetAccountUsers(account);

                // 2- Create user marshalling array from all returned users
                AriaUserMarshalling[] AriaMarshillingArray = new AriaUserMarshalling[result.Length];
               
                for (int i=0;i< result.Length;i++)
                {
                    AriaUserMarshalling ariaMarshalling = new AriaUserMarshalling();
                    ariaMarshalling.EmailAddress = result[i].UserName;

                    Person person = session.FindObject<Person>(CriteriaOperator.Parse("EMailAddress == '" + ariaMarshalling.EmailAddress + "'"));

                    if (person != null)
                    {
                        ariaMarshalling.UserName = person.Name;
                    }
                    else
                    {
                        ariaMarshalling.UserName = ariaMarshalling.EmailAddress;
                    }

                    AriaMarshillingArray[i] = ariaMarshalling;

                }
                return AriaMarshillingArray;
            }
            else
            {
                return new AriaUserMarshalling[0];
            }

     
        }
        // Sara.N 06/01/2015 Add WebMethod returns All Active Users [Start]

    }
}
