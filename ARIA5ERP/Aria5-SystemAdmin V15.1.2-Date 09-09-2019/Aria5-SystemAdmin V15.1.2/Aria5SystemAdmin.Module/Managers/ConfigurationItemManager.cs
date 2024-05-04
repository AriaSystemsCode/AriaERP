using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria5SystemAdmin.Module.BusinessObjects;


namespace Aria5SystemAdmin.Module.Managers
{
    public class ConfigurationItemManager
    {



        public static string UserEmailBody = "Dear Customer,<br/><br/> You are now unsubscribed, we are sorry to see you go!<br/><br/> Please be notified that you still have access to your account until the date of expiry. Once the trial date expires, you will no longer have an account with 1Touchaway<br/><br/>";

        public static string GetEmail(Session session, Guid personGuid)
        {
            string personEmail = "";
            if (session.IsConnected == false) session.Connect();

            if (session.IsConnected == true)
            {
                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Person> persons = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Person>(session, (CriteriaOperator.Parse("[Oid] = '" + personGuid + "'")));

                foreach (Aria5SystemAdmin.Module.BusinessObjects.Person item in persons)
                {
                    personEmail = item.EMailAddress;
                    break;
                }
            }
            return personEmail;
        }

        public static void CheckEpiration(Session session)
        {

            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                # region documentation
                // check 7 days before exipration.
                // look, the activiation key entity not found till now?
                // update activiation key with status send before email
                // check 1 days after exipration.
                # endregion documentation

                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session);
                foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                {
                    if (item.VersionType == Aria5SystemAdmin.Module.BusinessObjects.Version_Types.Demo)
                    {
                        Aria5SystemAdmin.Module.BusinessObjects.ActivationKey ActivationKey = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.ActivationKey>(CriteriaOperator.Parse("ID='" + item.ActivationKeyID + "'"));

                        if (ActivationKey != null && ActivationKey.SendEmailBeforeExpiration == false)
                        {// check 7 days before exipration.
                            if (item.EndDate.Date.Subtract(DateTime.Today).Days <= 7 && item.EndDate.Date.Subtract(DateTime.Today).Days > 0)
                            {
                                Email email = new Email();
                                // look for correct email settings/formate?
                                email.ToEmail = GetEmail(session, item.Contact.Oid);
                                email.EmailBody = "Dear Customer,<br/> Please be notified that, your evaluation period, will be expired soon,<br/>  Please contact Aria System Inc., Support team, to ask for new period. <br/> Thanks <br/> Aria Systems Inc.<br/> Automatic service.";
                                email.EmailTitle = "Expiration Alaram";
                                email.SendEmail();

                                // look, the activiation key entity not found till now?
                                // update activiation key with status send before email.    
                                ActivationKey.SendEmailBeforeExpiration = true;
                                ActivationKey.Save();
                            }
                        }

                        // check 1 days after exipration.
                        if (ActivationKey != null && ActivationKey.SendEmailAfterExpiration == false)
                        {
                            if (item.EndDate.Date.Subtract(DateTime.Today).Days < 0)
                            {
                                Email email = new Email();
                                // look for correct email settings/formate?
                                email.ToEmail = GetEmail(session, item.Contact.Oid);
                                email.EmailBody = "Dear Customer,<br/>  Please be notified that, your evaluation period, has been expired,<br/>  Please contact Aria System Inc., Support team, to ask for new period. <br/> Thanks <br/> Aria Systems Inc.<br/> Automatic service.";
                                email.EmailTitle = "Expiration Alaram";
                                email.SendEmail();
                                // look, the activiation key entity not found till now?
                                // update activiation key with status send before email.    
                                ActivationKey.SendEmailAfterExpiration = true;
                                ActivationKey.Save();
                            }

                        }
                    }
                }
            }
            //if (session.IsConnected == true) { session.Disconnect(); }

        }

        // Web Service role
        public static String GetActiviationFile(Session session, Guid Account, Guid Device, Guid Application)
        {
            string ActiviationFile = "";

            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                # region documentation
                //- If Athunticate then, Searches the configuration item table for the received parameters and get the related Activation Key OID
                //Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(CriteriaOperator.Parse("[Account.Oid] = '" + Account + "' and [Device.Oid]='" + Device + "' and Application.Oid='" + Application + "'"));
                //- If Athunticate then, Calls the Activation entity, pass the activation key OID and get the activation key file
                # endregion documentation

                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[Account.Oid] = '" + Account + "' and [Device.Oid]='" + Device + "' and Application.Oid='" + Application + "'")));
                foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                {
                    // This entity not finished yet.
                    //look
                    //ActiviationFile = ActivationEntity(item.Oid);
                    break;
                }
                return ActiviationFile;
            }
            else
            { return null; }
        }

        public static bool CheckActiviationFile(Session session, Guid Account, Guid Device, Guid Application, string ActivationKeyID)
        {
            bool ActivationKeyMatched = false;

            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                # region documentation
                // - If Athunticate then, Searches the configuration item table for the received Account, Device, Application and get the related Activation Key ID.
                //Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(CriteriaOperator.Parse("[Account.Oid] = '" + Account + "' and [Device.Oid]='" + Device + "' and Application.Oid='" + Application + "'"));
                //- If Athunticate then, Compare this ID with the activation key ID for the configuration item for this account\device\application.
                //- If Athunticate then, Return true of the two IDs are matched and false if they are different
                # endregion documentation

                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[Account.Oid] = '" + Account + "' and [Device.Oid]='" + Device + "' and Application.Oid='" + Application + "'")));
                foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                {
                    ActivationKeyMatched = (item.ActivationKeyID == ActivationKeyID);
                    break;
                }
                return ActivationKeyMatched;
            }
            else
            { return false; }

        }

        public static bool GenerateActiviationFile(Session session, Guid ConfigurationItem, Guid Application)
        {
            string ActivationKey = "";

            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                # region documentation
                // - If Athunticate then, Searches the configuration item table for the received Account, Device, Application and get the related Activation Key ID.
                //Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(CriteriaOperator.Parse("[Account.Oid] = '" + Account + "' and [Device.Oid]='" + Device + "' and Application.Oid='" + Application + "'"));
                //- If Athunticate then, Calls the Activation entity to generate new activation key and receive the OID for the generated activation key.
                //- If Athunticate then, Updates the configuration item table with the activation key OID.
                //- If Athunticate then, Return true if update sucessfully, else false.
                //- If Athunticate then, Return true of the two IDs are matched and false if they are different
                # endregion documentation

                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[Oid] = '" + ConfigurationItem + "' and Application.Oid='" + Application + "'")));
                foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                {
                    // look
                    //ActivationKey = ActivationKey.generate(Application);
                    //item.ActivationKey = ActivationKey;
                  
                    item.ActivationKeyID = ActivationKey;
                    item.Save();
                    return true;
                }
            }
            return false;
        }

        public static bool IsAccountApplicationExist(Session session, Guid AccountOid, String ApplicationID)
        {
            // ConfigurationItem information_about_running_application_on_specific_device = new ConfigurationItem(XpoDefault.Session);
            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                # region documentation
                //- If Athunticate then, Calls the Device Manager entity to return the device ID corresponding to the sent device Signature.
                //- If Athunticate then, Searches the ConfigurationItem table for the account, device and application. 
                //Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(CriteriaOperator.Parse("[AccountId] = '" + AccountID.ToString() + "' and [ApplicationId]='" + ApplicationID + "' and DeviceSignature='" + DeviceSignature + "'"));
                # endregion documentation

                Aria5SystemAdmin.Module.BusinessObjects.Account account = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("Oid='" + AccountOid + "'"));
                if (account != null)
                {
                    XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[AccountId] = '" + account.Id + "' and [ApplicationId]='" + ApplicationID + "'")));
                    if (configurationItems != null && configurationItems.Count() > 0)
                    { return true; }
                }
                else
                {
                    return false;
                }
                return false;

            }
            else { return false; }

        }

        public static ConfigurationItem GetInformation(Session session, Guid AccountOid, String ApplicationID, String DeviceSignature)
        {
            // ConfigurationItem information_about_running_application_on_specific_device = new ConfigurationItem(XpoDefault.Session);
            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                # region documentation
                //- If Athunticate then, Calls the Device Manager entity to return the device ID corresponding to the sent device Signature.
                //- If Athunticate then, Searches the ConfigurationItem table for the account, device and application. 
                //Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(CriteriaOperator.Parse("[AccountId] = '" + AccountID.ToString() + "' and [ApplicationId]='" + ApplicationID + "' and DeviceSignature='" + DeviceSignature + "'"));
                # endregion documentation
                //Mina.B 2015-09-14 Get device by device signature and account [Begin]
                //XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AccountDevice> accountDevice = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AccountDevice>(session, (CriteriaOperator.Parse("DeviceSignature='" + DeviceSignature + "'")));
                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AccountDevice> accountDevice = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AccountDevice>(session, (CriteriaOperator.Parse("DeviceSignature='" + DeviceSignature + "' and [Account]='" + AccountOid + "'")));
                //Mina.B 2015-09-14 Get device by device signature and account [End]
                if (accountDevice.Count > 0)
                {
                    Aria5SystemAdmin.Module.BusinessObjects.Account account = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("Oid='" + AccountOid + "'"));
                    XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[AccountId] = '" + account.Id + "' and [ApplicationId]='" + ApplicationID + "' and Device='" + accountDevice[0].Oid + "'")));
                    foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                    {
                        //information_about_running_application_on_specific_device.VersionType = item.VersionType;
                        //information_about_running_application_on_specific_device.StartDate = item.StartDate;
                        //information_about_running_application_on_specific_device.EndDate = item.EndDate;
                        //information_about_running_application_on_specific_device.ActivationKeyID = item.ActivationKeyID;

                        return item;

                    }
                }
                return null;
            }
            else { return null; }

        }

        //sara device [Start]

        public static ConfigurationItem GetInformation(Session session, Guid AccountOid, String ApplicationID)
        {
            // ConfigurationItem information_about_running_application_on_specific_device = new ConfigurationItem(XpoDefault.Session);
            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {

                Aria5SystemAdmin.Module.BusinessObjects.Account account = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("Oid='" + AccountOid + "'"));
                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[AccountId] = '" + account.Id + "' and [ApplicationId]='" + ApplicationID + "'")));
                foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                {
                    //information_about_running_application_on_specific_device.VersionType = item.VersionType;
                    //information_about_running_application_on_specific_device.StartDate = item.StartDate;
                    //information_about_running_application_on_specific_device.EndDate = item.EndDate;
                    //information_about_running_application_on_specific_device.ActivationKeyID = item.ActivationKeyID;

                    return item;

                }

                return null;
            }
            else { return null; }

        }
        //sara device [End]

        public static ConfigurationItem GetInformation(Session session, String AccountID, String ApplicationID, String DeviceSignature)
        {
            ConfigurationItem information_about_running_application_on_specific_device = new ConfigurationItem(XpoDefault.Session);
            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                # region documentation
                //- If Athunticate then, Calls the Device Manager entity to return the device ID corresponding to the sent device Signature.
                //- If Athunticate then, Searches the ConfigurationItem table for the account, device and application. 
                //Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(CriteriaOperator.Parse("[AccountId] = '" + AccountID.ToString() + "' and [ApplicationId]='" + ApplicationID + "' and DeviceSignature='" + DeviceSignature + "'"));
                # endregion documentation
                //Mina.B 2015-09-14 Get device by device signature and account [Begin]
                //XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AccountDevice> accountDevice = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AccountDevice>(session, (CriteriaOperator.Parse("DeviceSignature='" + DeviceSignature + "'")));
                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AccountDevice> accountDevice = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AccountDevice>(session, (CriteriaOperator.Parse("DeviceSignature='" + DeviceSignature + "' and [AccountId]='" + AccountID + "'")));
                //Mina.B 2015-09-14 Get device by device signature and account [End]
                if (accountDevice.Count > 0)
                {
                    XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[AccountId] = '" + AccountID.ToString() + "' and [ApplicationId]='" + ApplicationID + "' and Device='" + accountDevice[0].Oid + "'")));
                    foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                    {
                        information_about_running_application_on_specific_device.VersionType = item.VersionType;
                        information_about_running_application_on_specific_device.StartDate = item.StartDate;
                        information_about_running_application_on_specific_device.EndDate = item.EndDate;
                        information_about_running_application_on_specific_device.ActivationKeyID = item.ActivationKeyID;

                        return information_about_running_application_on_specific_device;

                    }
                }
                return null;
            }
            else { return null; }

        }

        public static bool AddNewConfigurationItem(Session session, ConfigurationItem configurationItem, Guid AccountOid, Guid ContactOid, Guid DeviceOid, Guid ApplicationOid)
        {
            if (session.IsConnected != true) { session.Connect(); }
            #region Adding new account
            if (session.IsConnected == true)
            {

                # region documentation
                //- If Athunticate then, Calls the Identifier Code Structure entity to use the current identifier 

                //structure for the Configuration Item entity and generate ID for the newly created configuration item.
                //                //- If Athunticate then, Search the Contact table for the sent account OID, get the related account ID and 

                //account name and update the Configuration Item table with the account id and name.
                //                //- If Athunticate then, Search the Contact table for the sent account contact OID, get the related 

                //contact name and update the Configuration Item table with the contact name.
                //                //- If Athunticate then, Search the AccountDevice table for the sent device OID, get the related device 

                //signature and device name and update the Configuration Item table with the device
                //                //   signature and device name.
                //                //- If Athunticate then, Search the Application table for the sent application OID, get the related 

                //application ID and application name and update the Configuration Item table with
                //   the application ID and application name.

                # endregion documentation

                Dictionary<int, object> seg = new Dictionary<int, object>();
                configurationItem.Id = Aria5.DevExpress.MainSystem.Module.Managers.IdentifierStructureManager.GetNextId(Aria5.DevExpress.MainSystem.Module.Managers.IdentifierStructureManager.GetAllIdentifierStructures()[0], seg);

                //MinaBaligh 2015-04-07 ConfigurationItem - Add division for account [Start]
                Division division = new Division(session);
                //MinaBaligh 2015-04-07 ConfigurationItem - Add division for account [End]

                Aria5SystemAdmin.Module.BusinessObjects.Account account = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + AccountOid + "'"));
                if (account != null)
                {

                    configurationItem.Account = account;
                    configurationItem.AccountId = account.Id;
                    configurationItem.AccountName = account.Name;


                    //MinaBaligh 2015-04-07 ConfigurationItem - Add division for account [Start]
                    division.Guide = account.Guide;
                    division.Business = account;
                    if (configurationItem.VersionType == Version_Types.Demo)
                    {
                        division.Name = account.AccountCode + "_Demo";
                        configurationItem.Account.DBSchema = configurationItem.Account.AccountCode + "_Demo";
                    }
                    else if (configurationItem.VersionType == Version_Types.Trial)
                    {
                        division.Name = account.AccountCode + "_Trial";
                    }
                    division.SchemaName =division.DBSchema= division.Name;
                    
                    division.Industry = account.Industry;
                    division.Status = account.Status;
                    division.StatusId = account.StatusId;
                    division.CategoryId = account.CategoryId;
                    division.ClassificationId = account.ClassificationId;
                    division.EMailAddress = account.EMailAddress;
                    division.EntityCategory = account.EntityCategory;
                    division.EntityClassification = account.EntityClassification;
                    division.EntityType = account.EntityType;
                    division.EnteredBy = account.EnteredBy;
                    division.EnteredDate = account.EnteredDate;
                    division.ExtraData = account.ExtraData;
                    division.InternalContactNo = account.InternalContactNo;
                    division.IMAGE = account.IMAGE;
                    division.Id = account.Id;
                    division.Guide = account.Guide;
                    division.LanguageCode = account.LanguageCode;
                    division.Language = account.Language;
                    division.Notes = account.Notes;
                    division.Priority = account.Priority;
                    division.TypeId = account.TypeId;
                    division.WebPageAddress = account.WebPageAddress;
                    division.ParentContact = account.ParentContact;
                    division.Currency = account.Currency;
                    division.CurrencyCode = account.CurrencyCode;
                    division.CurrentBalance = account.CurrentBalance;
                    division.CreditLimit = division.CreditLimit;
                    division.Description = account.Description;


                    foreach (EntityRelationship relation in account.News)
                    {
                        EntityRelationship newRelation = new EntityRelationship(session);
                        newRelation.EntityType = relation.EntityType;
                        newRelation.RelatedEntity = relation.RelatedEntity;
                        newRelation.RelatedEntityType = relation.RelatedEntityType;
                        newRelation.Entity = division;
                        newRelation.Save();
                    }



                    foreach (EntityRelationship relation in account.Events)
                    {
                        EntityRelationship newRelation = new EntityRelationship(session);
                        newRelation.EntityType = relation.EntityType;
                        newRelation.RelatedEntity = relation.RelatedEntity;
                        newRelation.RelatedEntityType = relation.RelatedEntityType;
                        newRelation.Entity = division;
                        newRelation.Save();
                    }

                    foreach (EntityRelationship relation in account.Information)
                    {
                        EntityRelationship newRelation = new EntityRelationship(session);
                        newRelation.EntityType = relation.EntityType;
                        newRelation.RelatedEntity = relation.RelatedEntity;
                        newRelation.RelatedEntityType = relation.RelatedEntityType;
                        newRelation.Entity = division;
                        newRelation.Save();
                    }

                    division.Save();
                    //MinaBaligh 2015-04-07 ConfigurationItem - Add division for account [End]
                }

                Aria5SystemAdmin.Module.BusinessObjects.Person person = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Person>(CriteriaOperator.Parse("[Oid] = '" + ContactOid + "'"));
                if (person != null)
                {
                    //  configurationItem.StatusId =EntityStatus.t 
                    configurationItem.Contact = person;
                    configurationItem.ContactName = person.Name;




                    foreach (EntityRelationship relation in person.News)
                    {
                        EntityRelationship newRelation = new EntityRelationship(session);
                        newRelation.EntityType = relation.EntityType;
                        newRelation.RelatedEntity = relation.RelatedEntity;
                        newRelation.RelatedEntityType = relation.RelatedEntityType;
                        newRelation.Entity = division;
                        newRelation.Save();
                    }

                    foreach (EntityRelationship relation in person.ADS)
                    {
                        EntityRelationship newRelation = new EntityRelationship(session);
                        newRelation.EntityType = relation.EntityType;
                        newRelation.RelatedEntity = relation.RelatedEntity;
                        newRelation.RelatedEntityType = relation.RelatedEntityType;
                        newRelation.Entity = division;
                        newRelation.Save();
                    }
                    //division. = person.EntityType;

                }

                Aria5SystemAdmin.Module.BusinessObjects.AccountDevice device = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.AccountDevice>(CriteriaOperator.Parse("[Oid] = '" + DeviceOid + "'"));
                if (device != null)
                {
                    configurationItem.Device = device;
                    configurationItem.DeviceName = device.Name;
                    configurationItem.DeviceSignature = device.DeviceSignature;
                }

                Aria5SystemAdmin.Module.BusinessObjects.Application_T application = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Application_T>(CriteriaOperator.Parse("[Oid] = '" + ApplicationOid + "'"));
                if (application != null)
                {
                    configurationItem.Application = application;
                    configurationItem.ApplicationName = application.Name;
                    configurationItem.ApplicationId = application.Id;
                }

                // look to be removed.
                //configurationItem.Contract = Guid.NewGuid();

                //HIA,1 Add initial value to status 2015-08-02 [Begin]
                configurationItem.Status = session.FindObject<EntityStatus>(CriteriaOperator.Parse("StatusID='ACTIVE'"));
                //HIA,1 Add initial value to status 2015-08-02 [End]
                configurationItem.Save();
                configurationItem.Account.Save();

                //Mina.B 2015-08-16 Create new schema and copy tables and data [Begin]
                if (application != null && application.Oid != Guid.Empty && division != null && division.Oid != Guid.Empty)
                {
                    DataManger dataManager = new DataManger();
                  dataManager.CreateSchema(division.SchemaName);
                    // Sara.N,1 Mechanism to Update Generated Account with RedLobister Demo Schema [Start]

                  //  dataManager.CopySchema(division.Oid, application.Oid, division.SchemaName, configurationItem.DemoAccount);
               dataManager.CopySchema(configurationItem.DemoAccount, application.Oid, division.SchemaName);
                    // Sara.N,1 Mechanism to Update Generated Account with RedLobister Demo Schema [End]

                }
                //Mina.B 2015-08-16 Create new schema and copy tables and data [End]

                return true;
            }
            else
            {
                return false;
            }
            #endregion Adding new account

        }
        // Sara.N 06/02/2015 Add WebMethod Return Add webmethod to change statues of ConfigurationItem to cancel[Start]
        public static bool CancelConfigurationItem(Guid ConfigurationItemID, Session session, String UnsubscribeComment)
        {

            if (session.IsConnected != true)
            { session.Connect(); }
            if (session.IsConnected == true)
            {
                ConfigurationItem ConfigurationItem = session.FindObject<ConfigurationItem>(CriteriaOperator.Parse("Oid='" + ConfigurationItemID + "'"));
                AriaSecuritySystemUser user = session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("Account='" + ConfigurationItem.Account.Oid + "'"));
                if (ConfigurationItem != null)
                {

                    ConfigurationItem.Status = session.FindObject<EntityStatus>(CriteriaOperator.Parse("StatusID='CANCEL'"));
                    //Mina.B Add expire date to notes field [Begin]
                    ConfigurationItem.Notes = "Account select to unsubscribe at date " + ConfigurationItem.ExpireDate;
                    //Mina.B Add expire date to notes field [End]
                    ConfigurationItem.Save();

                    AriaUserManager.SendEmail("sales@1touchaway.biz", "sales@2015", user.UserName.ToString(), UserEmailBody, "UnScubscribing Confirmation Mail", "UnScubscribing Confirmation Mail");
                    AriaUserManager.SendEmail("sales@1touchaway.biz", "sales@2015", "sales@1touchaway.biz", "User" + user.UserName.ToString() + "Unsubscribe Reason" + UnsubscribeComment, "UnScubscribed User", "Unsubscribed User: " + user.UserName.ToString());
                    return true;


                }
                else
                {
                    return false;
                }
            }
            else
            {
                return false;
            }
        }

        // Sara.N 06/02/2015 Add WebMethod Return Add webmethod to change statues of ConfigurationItem to cancel[End]
    }
}
