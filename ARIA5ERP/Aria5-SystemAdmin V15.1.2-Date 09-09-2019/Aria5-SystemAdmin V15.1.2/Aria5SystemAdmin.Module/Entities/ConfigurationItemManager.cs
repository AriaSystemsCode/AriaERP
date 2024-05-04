using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria5SystemAdmin.Module.BusinessObjects;


namespace Aria5SystemAdmin.Module.Entities
{
   public class ConfigurationItemManager
    {
        public static string ConnectionString = "Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;uid=azuresqladmin;pwd=aria_123;Pooling=False";

       // Worker role
        public static void AdjustSchema(string schemaName)
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

        public static string GetEmail(Guid personGuid, Session session)
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

        public static void CheckEpiration()
        {

            XpoDefault.ConnectionString = ConnectionString;
            AdjustSchema("SystemAdminTestingEnv");
            Session session = XpoDefault.Session;


            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session);
                foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                {
                    if (item.VersionType == Aria5SystemAdmin.Module.BusinessObjects.Version_Types.Demo)
                    {
                        // check 7 days before exipration.
                        if (item.EndDate.Date.Subtract(DateTime.Today).Days <= 7 && item.EndDate.Date.Subtract(DateTime.Today).Days > 0)
                        {
                            Email email = new Email();
                            // look for correct email settings/formate?
                            email.ToEmail = GetEmail(item.Contact.Oid, session);
                            email.EmailBody = "Dear Customer,\n Please be notified that, your evaluation period, will be expired soon,\n Please contact Aria System Inc., Support team, to ask for new period. \nThanks @\nAria Systems Inc.\nAutomatic service.";
                            email.EmailTitle = "Expiration Alaram";
                            email.SendEmail();

                            // look, the activiation key entity not found till now?
                            // update activiation key with status send before email.    


                        }

                        // check 1 days after exipration.
                        if (item.EndDate.Date.Subtract(DateTime.Today).Days < 0)
                        {
                            Email email = new Email();
                            // look for correct email settings/formate?
                            email.ToEmail = GetEmail(item.Contact.Oid, session);
                            email.EmailBody = "Dear Customer,\n Please be notified that, your evaluation period, has been expired,\n Please contact Aria System Inc., Support team, to ask for new period. \nThanks @\nAria Systems Inc.\nAutomatic service.";
                            email.EmailTitle = "Expiration Alaram";
                            email.SendEmail();
                            // look, the activiation key entity not found till now?
                            // update activiation key with status send before email.    
                        }

                    }
                }
            }

        }

        // Web Service role
        public static String GetActiviationFile(Guid Account, Guid Device, Guid Application)
        {

            string ActiviationFile = "";
            XpoDefault.ConnectionString = ConnectionString;
            AdjustSchema("SystemAdminTestingEnv");
            Session session = XpoDefault.Session;

            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                //- If Athunticate then, Searches the configuration item table for the received parameters and get the related Activation Key OID
                //Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(CriteriaOperator.Parse("[Account.Oid] = '" + Account + "' and [Device.Oid]='" + Device + "' and Application.Oid='" + Application + "'"));
                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[Account.Oid] = '" + Account + "' and [Device.Oid]='" + Device + "' and Application.Oid='" + Application + "'")));
                foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                {

                    //- If Athunticate then, Calls the Activation entity, pass the activation key OID and get the activation key file
                    // This entity not finished yet.
                    //look
                    //ActiviationFile = ActivationEntity(item.Oid);
                    break;
                }
                return ActiviationFile;
            }
            else { return null; }


        }

        public static bool CheckActiviationFile(Guid Account, Guid Device, Guid Application, string ActivationKeyID)
        {

            bool ActivationKeyMatched   = false;
            XpoDefault.ConnectionString = ConnectionString;
            AdjustSchema("SystemAdminTestingEnv");
            Session session = XpoDefault.Session;

            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                // - If Athunticate then, Searches the configuration item table for the received Account, Device, Application and get the related Activation Key ID.
                //Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(CriteriaOperator.Parse("[Account.Oid] = '" + Account + "' and [Device.Oid]='" + Device + "' and Application.Oid='" + Application + "'"));
                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[Account.Oid] = '" + Account + "' and [Device.Oid]='" + Device + "' and Application.Oid='" + Application + "'")));
                foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                {

                    //- If Athunticate then, Compare this ID with the activation key ID for the configuration item for this account\device\application.
                    ActivationKeyMatched = (item.ActivationKeyID == ActivationKeyID);
                    break;
                }
                //- If Athunticate then, Return true of the two IDs are matched and false if they are different

                return ActivationKeyMatched;
            }
            else { return false; }

        }

        public static bool GenerateActiviationFile(Guid ConfigurationItem, Guid Application)
        {

            string ActivationKey = "";
            XpoDefault.ConnectionString = ConnectionString;
            AdjustSchema("SystemAdminTestingEnv");
            Session session = XpoDefault.Session;

            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                // - If Athunticate then, Searches the configuration item table for the received Account, Device, Application and get the related Activation Key ID.
                //Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(CriteriaOperator.Parse("[Account.Oid] = '" + Account + "' and [Device.Oid]='" + Device + "' and Application.Oid='" + Application + "'"));
                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[Oid] = '" + ConfigurationItem + "' and Application.Oid='" + Application + "'")));
                foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                {
                    //- If Athunticate then, Calls the Activation entity to generate new activation key and receive the OID for the generated activation key.
                    // look
                    //ActivationKey = ActivationKey.generate(Application);

                    //- If Athunticate then, Updates the configuration item table with the activation key OID.
                    //- If Athunticate then, Return true if update sucessfully, else false.
                    //look
                    //item.ActivationKey = ActivationKey;
                    item.ActivationKeyID = ActivationKey;
                    item.Save();
                    return true;

                }
                //- If Athunticate then, Return true of the two IDs are matched and false if they are different
            }

            return false;
            

        }

        public static ConfigurationItem GetInformation(String AccountID, String ApplicationID, String DeviceSignature)
        {
            
            XpoDefault.ConnectionString = ConnectionString;
            AdjustSchema("SystemAdminTestingEnv");
            Session session = XpoDefault.Session;
            ConfigurationItem information_about_running_application_on_specific_device = new ConfigurationItem(XpoDefault.Session);


            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                //- If Athunticate then, Calls the Device Manager entity to return the device ID corresponding to the sent device Signature.
                //- If Athunticate then, Searches the ConfigurationItem table for the account, device and application. 
                //Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(CriteriaOperator.Parse("[AccountId] = '" + AccountID.ToString() + "' and [ApplicationId]='" + ApplicationID + "' and DeviceSignature='" + DeviceSignature + "'"));
                XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> configurationItems = new XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>(session, (CriteriaOperator.Parse("[AccountId] = '" + AccountID.ToString() + "' and [ApplicationId]='" + ApplicationID + "' and DeviceSignature='" + DeviceSignature + "'")));
                foreach (Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem item in configurationItems)
                {
                    information_about_running_application_on_specific_device.VersionType = item.VersionType;
                    information_about_running_application_on_specific_device.StartDate = item.StartDate;
                    information_about_running_application_on_specific_device.EndDate = item.EndDate;
                    return information_about_running_application_on_specific_device;

                }
                return null;
            }
            else { return null; }
            //}
            //else
            //{
            //    return null;
            //}
        }

        public static bool AddNewConfigurationItem(ConfigurationItem ConfigurationItemParametersItem, Guid AccountOid, Guid ContactOid, Guid DeviceOid, Guid ApplicationOid)
        {
            XpoDefault.ConnectionString = ConnectionString;
            AdjustSchema("SystemAdminTestingEnv");
            Session session = XpoDefault.Session;
            if (session.IsConnected != true) { session.Connect(); }

            #region Adding new account
            if (session.IsConnected == true)
            {
                Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem configurationItem = new Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem(session);

                //- If Athunticate then, Calls the Identifier Code Structure entity to use the current identifier structure for the Configuration Item entity and generate ID for the newly created configuration item.

                Dictionary<int, object> seg = new Dictionary<int, object>();
                configurationItem.Id = Aria5.DevExpress.MainSystem.Module.Entities.EntityIdentifierStructure.GetNextEntityid(Aria5.DevExpress.MainSystem.Module.Entities.EntityIdentifierStructure.GetAllIdentifierStructures()[0], seg);
                //configurationItem.Id = "123";

                //- If Athunticate then, Search the Contact table for the sent account OID, get the related account ID and account name and update the Configuration Item table with the account id and name.
                Aria5SystemAdmin.Module.BusinessObjects.Account account = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + AccountOid + "'"));
                if (account != null)
                {
                    configurationItem.Account = account;
                    configurationItem.AccountId = account.Id;
                    configurationItem.AccountName = account.Name;
                }
                //- If Athunticate then, Search the Contact table for the sent account contact OID, get the related contact name and update the Configuration Item table with the contact name.
                Aria5SystemAdmin.Module.BusinessObjects.Person person = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Person>(CriteriaOperator.Parse("[Oid] = '" + ContactOid + "'"));
                if (person != null)
                {
                    configurationItem.Contact = person;
                    configurationItem.ContactName = person.Name;
                }

                //- If Athunticate then, Search the AccountDevice table for the sent device OID, get the related device signature and device name and update the Configuration Item table with the device
                //   signature and device name.
                Aria5SystemAdmin.Module.BusinessObjects.AccountDevice device = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.AccountDevice>(CriteriaOperator.Parse("[Oid] = '" + DeviceOid + "'"));
                if (device != null)
                {
                    configurationItem.Device = device;
                    configurationItem.DeviceName = device.Name;
                    configurationItem.DeviceSignature = device.DeviceSignature;
                }
                //Mandatory
                configurationItem.Description = ConfigurationItemParametersItem.Description;
                configurationItem.NumberOfUsers = ConfigurationItemParametersItem.NumberOfUsers;

                //String ConfigurationItemDescription, Guid AccountOID, Guid ContactOID, Guid DeviceOID,
                //Guid ApplicationOID, String InstallationType, String VersionType, Date Start Date, Date End Date


                //- If Athunticate then, Search the Application table for the sent application OID, get the related application ID and application name and update the Configuration Item table with
                //   the application ID and application name.
                Aria5SystemAdmin.Module.BusinessObjects.Application_T application = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Application_T>(CriteriaOperator.Parse("[Oid] = '" + ApplicationOid + "'"));
                if (application != null)
                {
                    configurationItem.Application = application;
                    configurationItem.ApplicationName = application.Name;
                    configurationItem.ApplicationId = application.Id;
                }

                
                configurationItem.InstallationType = ConfigurationItemParametersItem.InstallationType;
                configurationItem.VersionType = ConfigurationItemParametersItem.VersionType;
                configurationItem.StartDate = ConfigurationItemParametersItem.StartDate;
                configurationItem.EndDate = ConfigurationItemParametersItem.EndDate;
                configurationItem.DemoAccount = ConfigurationItemParametersItem.DemoAccount;

                // look to be removed.
                configurationItem.Contract = Guid.NewGuid();

                configurationItem.Save();

                
                return true;
            }
            else
            {
                return false;
            }
            #endregion Adding new account

        }


    }
}
