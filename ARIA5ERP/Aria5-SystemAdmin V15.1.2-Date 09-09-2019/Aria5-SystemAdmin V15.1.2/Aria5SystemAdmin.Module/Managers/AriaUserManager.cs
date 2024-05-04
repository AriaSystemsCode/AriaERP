using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module.Managers
{
    public class AriaUserManager
    {
        public static void AddUserManager(AriaSecuritySystemUser newUser)
        {
            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
                //List<string> requiredFields = Aria5.DevExpress.SystemManager.Module.Aria5TablesDictionary.GetRequiredTableFields("AriaSecuritySystemUser");
                newUser.Save();
            }
            else
            {        
              throw new Exception("|Cannot connect to the database|");
            }
        }

        public static Guid UserAuthentication(string userName, string password)
        {
            Session session = XpoDefault.Session;

            if (session.IsConnected)
            {
                AriaSecuritySystemUser validUsername = session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("UserName = '" + userName + "' "));

                if (validUsername == null)
                {
                    throw new Exception("|Username Doesn't Exist|");
                }
                else
                {
                    if (validUsername.ComparePassword(password) == true)
                    {
                        // Sara.N , 13-07-2015  Login Not Fail in Trail period after unsubscribe the program [Start]
                     
                        ConfigurationItem ConfigurationItem = session.FindObject<ConfigurationItem>(CriteriaOperator.Parse("AccountId= '" + validUsername.Account.Id+ "'"));
                     
                      // EntityStatus status = session.GetObjectByKey<EntityStatus>(ConfigurationItem.Status);
                        if (ConfigurationItem.StatusId != null)
                        {
                            switch (ConfigurationItem.VersionType)
                            {
                                case Version_Types.Demo:
                                    if (DateTime.Now <= ConfigurationItem.EndDate)
                                    {
                                        return validUsername.Account.Oid;
                                    }
                                    else
                                    {
                                        throw new Exception("|Demo Period Expired|");
                                    }
                                  
                                case Version_Types.Trial:
                                    if (ConfigurationItem.StatusId.Trim().ToUpper() == "CANCEL")
                                    {
                                        if (DateTime.Now <= ConfigurationItem.EndDate)
                                        {
                                            return validUsername.Account.Oid;
                                        }
                                        else
                                        {
                                            throw new Exception("|Trail Period Expired|");
                                        }
                                    }
                                    else
                                    {
                                        return validUsername.Account.Oid;
                                    }
                                     
                                case Version_Types.Paying:
                                    if (ConfigurationItem.StatusId.Trim().ToUpper() == "CANCEL")
                                    {
                                        if (DateTime.Now <= ConfigurationItem.EndDate)
                                        {
                                            return validUsername.Account.Oid;
                                        }
                                        else
                                        {
                                           
                                            throw new Exception("|Your account is cancelled|");
                                        }
                                   }
                                    else
                                    {
                                        return validUsername.Account.Oid;
                                    }
                             default:
                                  throw new Exception("|Invalid StatusId|");
                                   
                            }
                           
                        }
                        else
                        {
                            throw new Exception("|Invalid StatusId|");
                        }
                        // Sara.N,13-07-2015 Login Not Fail in Trail period after unsubscribe the program [End]
                    }
                    else
                    {
                        throw new Exception("|Invalid Password|");
                    }
                }
            }
            else
            {
                throw new Exception("|Cannot connect to the database|");
            }
        }

        public static string GenerateTemporaryPassword(string userName)
        {
            string tempPassword = "";
            //StartConnection();
            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
                AriaSecuritySystemUser ValidUsername = session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("UserName = '" + userName + "' "));

                if (ValidUsername == null)
                {
                    throw new Exception("|Username Doesn't Exist|");
                }
                else
                {
                    tempPassword = System.Web.Security.Membership.GeneratePassword(10, 1);
                    ValidUsername.SetPassword(tempPassword.Trim());
                    ValidUsername.Save();
                }
            }
            return tempPassword;
        }

        public static string GenerateTemporaryPasswordAdmin(string userName)
        {
            string tempPassword = "";
            // StartConnection();
            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
                AriaSecuritySystemUser ValidUsername = session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("UserName = '" + userName + "' "));
                if (ValidUsername == null)
                {
                    throw new Exception("|Username Doesn't Exist|");
                }
                else
                {
                    tempPassword = System.Web.Security.Membership.GeneratePassword(10, 1);
                    ValidUsername.SetPassword(tempPassword.Trim());
                    ValidUsername.Save();
                }
            }
            return tempPassword;
        }
        
        public static void SendPassword(string userName, string newTmpPassword, bool Reset=true)
        {

            string emailString, emailTitleString, emailSubjectString = "";

            if (Reset == true)
            {
                emailSubjectString = "Password Reset Request";
                emailTitleString = "1TouchAway.Com";
                emailString = "Dear Customer @FirstName@,<br/>  AriaSystems received a request to reset the password for your Resto account.<br/> Here is your New Password:" + newTmpPassword + " <br/> If you didn't request to Reset Password.,<br/> Please contact 1TouchAway Inc., Support team, to ask for help. <br/>Thanks @<br/>1TouchAway team<br/>support@1TouchAway.biz<br/>";

            }
            else 
                {
                    emailSubjectString = "Get started with your Resto demo";
                    emailTitleString = "1TouchAway.Com";
                  
                //Sara.N,10/08/2015 Change the Email's content - Aria 5 - 1TouchAway - Iteration # 2015-06 [Start] 

                    //emailString = "Dear Customer @FirstName@ ,<br/><br/>Thank you for registering with Resto.<br/><br/>Here is your New password for the Application as follows:<br/><br/>Password: " + newTmpPassword + "<br/><br/>Resto has thousands of new updates every month, many direct from top brands.<br/><br/>We know new Trend hunting can be time consuming, so to help you out, we've set up an automated, tailored shopping updates by email alert for you based on the information been provided. You can create new alerts, change frequency and review existing ones when you are logged in, also you can click here to mange alerts.<br/>https://www.1touchaway.biz<br/><br/>Good Luck.<br/>1TouchAway team<br/>support@1TouchAway.biz<br/>";
                    emailString = "Dear Customer @FirstName@ ,<br/><br/>Thank you for registering with Resto.<br/><br/>Here is your New password for the Application as follows:<br/><br/>Password: " + newTmpPassword + "<br/><br/>You can now start your demo successfully.<br/><br/>We've set up automated, tailored shopping updates by email alerts for you, based on the information you have provided.<br/>Enjoy your Demo.<br/><br/>1TouchAway team.<br/>sales@1TouchAway.biz<br/>";

                    //Sara.N,10/08/2015 Change the Email's content - Aria 5 - 1TouchAway - Iteration # 2015-06 [END]               
                }


            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {
            
                AriaSecuritySystemUser ValidUsername = session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("UserName = '" + userName + "' "));
                Person person = session.FindObject<Person>(CriteriaOperator.Parse(" [ParentContact] = '" + ValidUsername.Account.Oid + "' "));                
                if (ValidUsername == null)
                {
                    throw new Exception("|Username Doesn't Exist|");
                }
                else
                {
                    Email email = new Email();
                    email.ToEmail = ValidUsername.UserName;
                    //email.EmailTitle = "Password Reset Request";
                    //email.EmailBody = "Dear Customer,<br/>  AriaSystems received a request to reset the password for your 1TouchAway account.<br/> Here is your New Password:" + newTmpPassword + " <br/> If you didn't request to Reset Password.,<br/> Please contact Aria System Inc., Support team, to ask for help. <br/>Thanks @<br/>Aria Systems Inc.<br/>Automatic service.";
                    email.EmailSubject = emailSubjectString;
                    email.EmailTitle = emailTitleString;
                   // email.EmailBody = emailString;
                   email.EmailBody = emailString.Replace("@FirstName@", person.FirstName);
                    email.SendEmail();

                    //person.a

                   

                    Email emailToAria = new Email();
                    emailToAria.ToEmail = "marketing@1touchaway.biz,sales@1touchaway.biz";
                    emailToAria.EmailSubject = "New Client Registeration";
                    emailToAria.EmailBody = "Dear Marketing Team,<br/>  1TouchAway received a request to register for a new client. <br/> Account Name : " + (ValidUsername.Account.Name == null ? "N/A" : ValidUsername.Account.Name) + "<br/> Account Code : " + (ValidUsername.Account.AccountCode == null ? "N/A" : ValidUsername.Account.AccountCode) + "<br/> Email Address : " + (ValidUsername.UserName == null ? "N/A" : ValidUsername.UserName) + "<br/> Website URL : " + (ValidUsername.Account.WebPageAddress == null ? "N/A" : ValidUsername.Account.WebPageAddress) + "<br/> Tel.: " + (ValidUsername.Account.ContactPhones == null && ValidUsername.Account.ContactPhones.Count > 0 && ValidUsername.Account.ContactPhones[0].PhoneNumber != null ? "N/A" : ValidUsername.Account.ContactPhones[0].PhoneNumber) + " <br/> For more information , please access Aria account management system <br/>, Support team, to ask for help. <br/>Thanks @<br/>Aria Systems Inc.<br/>Automatic service.";
                    emailToAria.SendEmail();
                
                }
            }
        }
        //Sara.M send feedback [Start]
        public static void SendFeedback(string EmailFrom, string EmailFromPassword, string EmailTo, string Body)
        {

            string emailString, emailTitleString, emailSubjectString = "";


            emailSubjectString = "Resto Feedbacks";
            emailTitleString = "Customer Feedbacks";
            emailString = Body;


            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {


                {
                    Email email = new Email();
                    email.ToEmail = EmailTo;
                    email.FromEmail = EmailFrom;
                    email.EmailPassword = EmailFromPassword;

                    email.EmailSubject = emailSubjectString;
                    email.EmailTitle = emailTitleString;

                    email.EmailBody = emailString;
                    email.SendEmailFeedback();
                }
            }
        }
        //Sara.M send feedback [End]

        //Sara.N, 13-\07\2015[Tracking# + Aria5-DevExpress-Configuration Item]_Programming - Aria 5 - System Admin - Iteration # 2015-07  [Start]
        public static void SendEmail(string EmailFrom, string EmailFromPassword, string EmailTo, string Body,string EmailTitle,string Subject)
        {

            string emailString, emailTitleString, emailSubjectString = "";


            emailSubjectString = Subject;
            emailTitleString = EmailTitle;
            emailString = Body;


            Session session = XpoDefault.Session;
            if (session.IsConnected == true)
            {


                {
                    Email email = new Email();
                    email.ToEmail = EmailTo;
                    email.FromEmail = EmailFrom;
                    email.EmailPassword = EmailFromPassword;

                    email.EmailSubject = emailSubjectString;
                    email.EmailTitle = emailTitleString;

                    email.EmailBody = emailString;
                    email.SendEmailFeedback();
                }
            }
        }
        //Sara.N, 13-\07\2015[Tracking# + Aria5-DevExpress-Configuration Item]_Programming - Aria 5 - System Admin - Iteration # 2015-07 [End]

        
        
        public static void RestPassword(string userName)
        {
            string newTmpPassword = GenerateTemporaryPassword(userName);
            SendPassword(userName, newTmpPassword);

        }

        public static bool ChangePassword(string userName, string oldPassword, string password)
        {
            bool bResult = false;

             Guid accountOid = UserAuthentication(userName, oldPassword);
             Session session = XpoDefault.Session;
                 
             if (session.IsConnected == true)
             {
                 AriaSecuritySystemUser ValidUsername = session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("UserName = '" + userName + "' "));
                 
                if (ValidUsername == null)
                {
                    throw new Exception("|Username Invalid|");
                }
                else
                {
                    ValidUsername.SetPassword(password.Trim());
                    ValidUsername.Save();
                    bResult = true;
                }
                
             }

             return bResult;
        }
        // Sara.N 06/01/2015 Add WebMethod Return Add webmethod returns All Active Users [Start]
        public static AriaSecuritySystemUser[] GetAccountUsers(Guid account)
        {
            if (account != Guid.Empty && account != null)
            {
                Session session = XpoDefault.Session;
                if (session.IsConnected == true)
                {
                    
                    //XPCollection<AriaSecuritySystemUser> users = new XPCollection<AriaSecuritySystemUser>(session);

                    XPCollection<AriaSecuritySystemUser> users = new XPCollection<AriaSecuritySystemUser>(session, (CriteriaOperator.Parse("[Account] = '" + account + "' and [IsActive]='True' ")));

                   // users.Filter = "Account = '" + account + "' and IsActive='True'";
                    users.Load();

                    return users.ToArray();
                }

                throw new Exception("|Invalid Acount |");
            }
            else
            {
                throw new Exception("|Invalid Acount |");
            }
          
        }
        // Sara.N 06/01/2015 Add WebMethod Return Add webmethod returns All Active Users [End]

    }
}
