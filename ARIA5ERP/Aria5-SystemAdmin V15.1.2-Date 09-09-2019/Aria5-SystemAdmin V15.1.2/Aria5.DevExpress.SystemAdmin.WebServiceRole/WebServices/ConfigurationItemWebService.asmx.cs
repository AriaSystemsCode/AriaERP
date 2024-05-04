using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Services;
using Aria5.DevExpress.MainSystem.Module.Managers;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
using Aria5SystemAdmin.Module.BusinessObjects;
using System.Web.Configuration;

// Mahmoud.S 07/19/2016 Timeout  In Filling clients Attachments Table while registeration [Start]
//namespace Aria5.DevExpress.SystemAdmin.WebServiceRole
namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.WebServices
// Mahmoud.S 07/19/2016 Timeout  In Filling clients Attachments Table while registeration [Start]

{
    /// <summary>
    /// Web service for adding and getting information, about any configuration item.
    /// </summary>

    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]

    public class ConfigurationItemWebService : System.Web.Services.WebService
    {
        [WebMethod(EnableSession = true)]
        public ConfigurationItemMarshalling GetInformation(String AccountID, String ApplicationID, String DeviceSignature)
        {
            Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;

            ConfigurationItemMarshalling information_about_running_application_on_specific_device = new ConfigurationItemMarshalling();

            ConfigurationItem configurationItem = Aria5SystemAdmin.Module.Managers.ConfigurationItemManager.GetInformation(session, AccountID, ApplicationID, DeviceSignature);
            if (configurationItem.VersionType != null) information_about_running_application_on_specific_device.VersionType = configurationItem.VersionType.ToString();
            information_about_running_application_on_specific_device.StartDate       = configurationItem.StartDate;
            information_about_running_application_on_specific_device.EndDate         = configurationItem.EndDate;
            information_about_running_application_on_specific_device.ActivationKeyID = configurationItem.ActivationKeyID;
            information_about_running_application_on_specific_device.Oid = configurationItem.Oid;
            information_about_running_application_on_specific_device.DemoAccount = configurationItem.DemoAccount;
            //Mina.B 2015-07-28 [Begin]
            information_about_running_application_on_specific_device.ContactOid = configurationItem.Contact.Oid;
            information_about_running_application_on_specific_device.ContactName = configurationItem.ContactName;
            information_about_running_application_on_specific_device.Status = configurationItem.StatusId;
            //Mina.B 2015-07-29 [End]

            if(configurationItem.InstallationType != null) information_about_running_application_on_specific_device.InstallationType = configurationItem.InstallationType.ToString();
            information_about_running_application_on_specific_device.NumberOfUsers = configurationItem.NumberOfUsers;
            information_about_running_application_on_specific_device.StartDate = configurationItem.StartDate;
            information_about_running_application_on_specific_device.EndDate = configurationItem.EndDate;
            if (configurationItem.DeviceSignature != null) information_about_running_application_on_specific_device.DeviceSignature = configurationItem.DeviceSignature;
            return information_about_running_application_on_specific_device;

        }

        [WebMethod(EnableSession = true)]
        public ConfigurationItemMarshalling GetInformationByOid(Guid AccountOid, String ApplicationID, String DeviceSignature)
        {
           Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;

            ConfigurationItemMarshalling information_about_running_application_on_specific_device = new ConfigurationItemMarshalling();

            ConfigurationItem configurationItem = Aria5SystemAdmin.Module.Managers.ConfigurationItemManager.GetInformation(session, AccountOid, ApplicationID, DeviceSignature);
            if(configurationItem.VersionType != null) information_about_running_application_on_specific_device.VersionType = configurationItem.VersionType.ToString();
            information_about_running_application_on_specific_device.StartDate = configurationItem.StartDate;
            information_about_running_application_on_specific_device.EndDate = configurationItem.EndDate;
            information_about_running_application_on_specific_device.ActivationKeyID = configurationItem.ActivationKeyID;
            information_about_running_application_on_specific_device.Oid = configurationItem.Oid;
            information_about_running_application_on_specific_device.DemoAccount = configurationItem.DemoAccount;
            //Mina.B 2015-07-28 [Begin]
            information_about_running_application_on_specific_device.ContactOid = configurationItem.Contact.Oid;
            information_about_running_application_on_specific_device.ContactName = configurationItem.ContactName;
            information_about_running_application_on_specific_device.Status = configurationItem.StatusId;
            
            //Mina.B 2015-07-29 [End]

            if(configurationItem.InstallationType != null) information_about_running_application_on_specific_device.InstallationType = configurationItem.InstallationType.ToString();
            information_about_running_application_on_specific_device.NumberOfUsers = configurationItem.NumberOfUsers;
            information_about_running_application_on_specific_device.StartDate = configurationItem.StartDate;
            information_about_running_application_on_specific_device.EndDate = configurationItem.EndDate;
            if (configurationItem.DeviceSignature != null) information_about_running_application_on_specific_device.DeviceSignature = configurationItem.DeviceSignature;
            return information_about_running_application_on_specific_device;

        }


        //sara device [Start]
        [WebMethod(EnableSession = true)]
        public ConfigurationItemMarshalling GetInformationByAccountAndAppId(Guid AccountOid, String ApplicationID)
        {
            Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;

            ConfigurationItemMarshalling information_about_running_application_on_specific_device = new ConfigurationItemMarshalling();

            ConfigurationItem configurationItem = Aria5SystemAdmin.Module.Managers.ConfigurationItemManager.GetInformation(session, AccountOid, ApplicationID);
            if(configurationItem.VersionType != null) information_about_running_application_on_specific_device.VersionType = configurationItem.VersionType.ToString();
            information_about_running_application_on_specific_device.StartDate = configurationItem.StartDate;
            information_about_running_application_on_specific_device.EndDate = configurationItem.EndDate;
            information_about_running_application_on_specific_device.ActivationKeyID = configurationItem.ActivationKeyID;
            information_about_running_application_on_specific_device.Oid = configurationItem.Oid;
            information_about_running_application_on_specific_device.DemoAccount = configurationItem.DemoAccount;
            //Mina.B 2015-07-28 [Begin]
            information_about_running_application_on_specific_device.ContactOid = configurationItem.Contact.Oid;
            information_about_running_application_on_specific_device.ContactName = configurationItem.ContactName;
            information_about_running_application_on_specific_device.Status = configurationItem.StatusId;
            //Mina.B 2015-07-29 [End]

            if(configurationItem.InstallationType != null) information_about_running_application_on_specific_device.InstallationType = configurationItem.InstallationType.ToString();
            information_about_running_application_on_specific_device.NumberOfUsers = configurationItem.NumberOfUsers;
            information_about_running_application_on_specific_device.StartDate = configurationItem.StartDate;
            information_about_running_application_on_specific_device.EndDate = configurationItem.EndDate;
            if (configurationItem.DeviceSignature != null) information_about_running_application_on_specific_device.DeviceSignature = configurationItem.DeviceSignature;
            return information_about_running_application_on_specific_device;

        }
        //sara device [End]
        [WebMethod(EnableSession = true)]
        public bool IsAccountApplicationExist(Guid AccountOid, String ApplicationID)
        {
           Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;

            return Aria5SystemAdmin.Module.Managers.ConfigurationItemManager.IsAccountApplicationExist(session, AccountOid, ApplicationID);
           
        }

        

        [WebMethod(EnableSession = true)]
        public String GetActiviationFile(Guid Account, Guid Device, Guid Application)
        {
           Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;

            return Aria5SystemAdmin.Module.Managers.ConfigurationItemManager.GetActiviationFile(session, Account, Device, Application);
        }

        [WebMethod(EnableSession = true)]
        public bool CheckActiviationFile(Guid Account, Guid Device, Guid Application, string ActivationKeyID)
        {
           Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;

            return Aria5SystemAdmin.Module.Managers.ConfigurationItemManager.CheckActiviationFile(session, Account, Device, Application, ActivationKeyID);

        }

        [WebMethod(EnableSession = true)]
        public bool GenerateActiviationFile(Guid ConfigurationItem, Guid Application)
        {
           Helpers.SchemaHelper.SwitchSchema();

            Session session = XpoDefault.Session;

            return Aria5SystemAdmin.Module.Managers.ConfigurationItemManager.GenerateActiviationFile(session, ConfigurationItem, Application);
        }

        [WebMethod(EnableSession = true)]
        public bool AddNewConfigurationItem(ConfigurationItemMarshalling ConfigurationItemParametersItem)
        {
            // Helpers.SchemaHelper.SwitchSchema();
            // Mahmoud.S 07/19/2016 Timeout  In Filling clients Attachments Table while registeration [Start]
            Helpers.SchemaHelper.SwitchSchema();
            // Mahmoud.S 07/19/2016 Timeout  In Filling clients Attachments Table while registeration [End]

            Session session = XpoDefault.Session;


            ConfigurationItem configurationItem = new ConfigurationItem(session);
            configurationItem.Description = ConfigurationItemParametersItem.ConfigurationItemDescription;
            configurationItem.DemoAccount = ConfigurationItemParametersItem.DemoAccount;
            configurationItem.StartDate = ConfigurationItemParametersItem.StartDate;
            configurationItem.EndDate = ConfigurationItemParametersItem.EndDate;
            configurationItem.NumberOfUsers = ConfigurationItemParametersItem.NumberOfUsers;
            //configurationItem.VersionType = ((Aria5SystemAdmin.Module.BusinessObjects.Version_Types)Enum.Parse(typeof(Aria5SystemAdmin.Module.BusinessObjects.Version_Types), ConfigurationItemParametersItem.VersionType));
            //configurationItem.InstallationType = ((Aria5SystemAdmin.Module.BusinessObjects.installation_types)Enum.Parse(typeof(Aria5SystemAdmin.Module.BusinessObjects.installation_types), ConfigurationItemParametersItem.InstallationType));
            configurationItem.VersionType = ((Version_Types)Version_Types.Parse(typeof(Version_Types), ConfigurationItemParametersItem.VersionType, true));
            configurationItem.InstallationType = ((installation_types)installation_types.Parse(typeof(installation_types), ConfigurationItemParametersItem.InstallationType, true));
           // configurationItem.InstallationType = ConfigurationItemParametersItem.InstallationType;
            //configurationItem.VersionType = ConfigurationItemParametersItem.VersionType.ToString();
            return Aria5SystemAdmin.Module.Managers.ConfigurationItemManager.AddNewConfigurationItem(session, configurationItem, ConfigurationItemParametersItem.AccountOid, ConfigurationItemParametersItem.ContactOid, ConfigurationItemParametersItem.DeviceOid, ConfigurationItemParametersItem.ApplicationOid);

        }

        // Sara.N 06/02/2015 Add WebMethod Return Add webmethod to change statues of ConfigurationItem to cancel[Start]
        [WebMethod(EnableSession = true)]
        public bool CancelConfigurationItem(Guid ConfigurationItemID, String UnsubscribeComment)
        {
            // Mahmoud.S 07/19/2016 Timeout  In Filling clients Attachments Table while registeration [Start]
            Helpers.SchemaHelper.SwitchSchema();
            // Mahmoud.S 07/19/2016 Timeout  In Filling clients Attachments Table while registeration [End]

            Session session = XpoDefault.Session;
           return(  Aria5SystemAdmin.Module.Managers.ConfigurationItemManager.CancelConfigurationItem(ConfigurationItemID, session, UnsubscribeComment));

           //  Aria5SystemAdmin.Module.Managers.AriaUserManager.SendFeedback(EmailFrom, EmailFromPassword, EmailTo, Body);
        }
        // Sara.N 06/02/2015 Add WebMethod Return Add webmethod to change statues of ConfigurationItem to cancel[Start]


        // Mahmoud.S 07/19/2016 Timeout  In Filling clients Attachments Table while registeration [Start]
        [WebMethod(EnableSession = true)]
        public int GetDataRecordsCount(Guid accountOid, string tableName)
        {
            Helpers.SchemaHelper.SwitchSchema();
            Session session = XpoDefault.Session;
            return 0;//testing using default value to overcome the error in the next line //Emad 29/3 
            //return Aria5SystemAdmin.Module.Managers.ConfigurationItemManager.GetDataRecordsCount(session, accountOid, tableName);
        }

        [WebMethod(EnableSession = true)]
        public int CopyDemoDataRecords(Guid demoAccountOid, Guid accountOid, string tableName, int from, int to)
        {
            Helpers.SchemaHelper.SwitchSchema();
            Session session = XpoDefault.Session;
            return 0;//testing using default value to overcome the error in the next line //Emad 29/3 

           // return Aria5SystemAdmin.Module.Managers.ConfigurationItemManager.CopyDemoDataRecords(session, demoAccountOid, accountOid, tableName, from, to);
        }
        // Mahmoud.S 07/19/2016 Timeout  In Filling clients Attachments Table while registeration [Start]
    }

}
