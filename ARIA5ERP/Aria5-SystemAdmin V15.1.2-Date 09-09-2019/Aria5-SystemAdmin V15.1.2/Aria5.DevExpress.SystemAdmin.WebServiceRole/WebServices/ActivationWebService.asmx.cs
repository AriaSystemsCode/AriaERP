using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Services;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
using DevExpress.Xpo;
using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module.Managers;
using System.Web.Configuration;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.WebServices
{
    /// <summary>
    /// Summary description for ActivationWebService
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    // [System.Web.Script.Services.ScriptService]
    public class ActivationWebService : System.Web.Services.WebService
    {

       [WebMethod(EnableSession = true)]
        public ActivationMarshalling  GenerateNewActivationKey(Guid configrationItemOid)
        {
           Helpers.SchemaHelper.SwitchSchema();

            ActivationKey activationKey = new ActivationKey();
            ActivationMarshalling activationKeyMarshalling = new ActivationMarshalling();
            ActivationManager activationManager = new ActivationManager();
            activationKey =  activationManager.GenerateNewActivationKey(configrationItemOid);

            activationKeyMarshalling.AccountId = activationKey.AccountId;
            activationKeyMarshalling.AccountName = activationKey.AccountName;
            activationKeyMarshalling.ActivationKeyFile = activationKey.ActivationKeyFile;
            activationKeyMarshalling.Application = activationKey.Application.Oid.ToString();
            activationKeyMarshalling.ApplicationId = activationKey.ApplicationId;
            activationKeyMarshalling.ApplicationName = activationKey.ApplicationName;
            activationKeyMarshalling.ConfigurationItem = activationKey.ConfigurationItem.Oid;
            activationKeyMarshalling.Device = activationKey.Device.Oid.ToString();
            activationKeyMarshalling.DeviceName = activationKey.DeviceName;
            activationKeyMarshalling.DeviceSignature = activationKey.DeviceSignature;
            activationKeyMarshalling.DigitalSignature = activationKey.DigitalSignature;
            activationKeyMarshalling.EndDate = activationKey.EndDate;
            activationKeyMarshalling.GenerateDate = activationKey.GenerateDate;
            activationKeyMarshalling.ID = activationKey.ID;
            activationKeyMarshalling.InstallationType = activationKey.InstallationType.ToString();
            activationKeyMarshalling.NumberOfUsers = activationKey.NumberOfUsers;
            activationKeyMarshalling.StartDate = activationKey.StartDate;
            activationKeyMarshalling.VersionType = activationKey.VersionType.ToString();
            return activationKeyMarshalling;
 
        }

       [WebMethod(EnableSession = true)]
        public bool IsActivationKeyFound(Guid configrationItemOid)
        {
           Helpers.SchemaHelper.SwitchSchema();

            ActivationManager activationManager = new ActivationManager();
           return activationManager.IsActivationKeyFound(configrationItemOid);
        }

       [WebMethod(EnableSession = true)]
        public ActivationMarshalling GetCurrentActivationKeyInfo(Guid configrationItemOid)
        {
           Helpers.SchemaHelper.SwitchSchema();

            ActivationKey activationKey = new ActivationKey();
            ActivationMarshalling activationKeyMarshalling = new ActivationMarshalling();
            ActivationManager activationManager = new ActivationManager();
            activationKey = activationManager.GetCurrentActivationKeyInfo(configrationItemOid);


            activationKeyMarshalling.AccountId = activationKey.AccountId;
            activationKeyMarshalling.AccountName = activationKey.AccountName;
            activationKeyMarshalling.ActivationKeyFile = activationKey.ActivationKeyFile;
            activationKeyMarshalling.Application = activationKey.Application.Oid.ToString();
            activationKeyMarshalling.ApplicationId = activationKey.ApplicationId;
            activationKeyMarshalling.ApplicationName = activationKey.ApplicationName;
            activationKeyMarshalling.ConfigurationItem = activationKey.ConfigurationItem.Oid;
            activationKeyMarshalling.Device = activationKey.Device.Oid.ToString();
            activationKeyMarshalling.DeviceName = activationKey.DeviceName;
            activationKeyMarshalling.DeviceSignature = activationKey.DeviceSignature;
            activationKeyMarshalling.DigitalSignature = activationKey.DigitalSignature;
            activationKeyMarshalling.EndDate = activationKey.EndDate;
            activationKeyMarshalling.GenerateDate = activationKey.GenerateDate;
            activationKeyMarshalling.ID = activationKey.ID;
            activationKeyMarshalling.InstallationType = activationKey.InstallationType.ToString();
            activationKeyMarshalling.NumberOfUsers = activationKey.NumberOfUsers;
            activationKeyMarshalling.StartDate = activationKey.StartDate;
            activationKeyMarshalling.VersionType = activationKey.VersionType.ToString();
            return activationKeyMarshalling;
 
        }


       [WebMethod(EnableSession = true)]
       public ActivationMarshalling GetCurrentActivationKeyInfoByDeviceAndApplication(Guid applicationOId, Guid accountOid, string digitalSignature)
       {
          Helpers.SchemaHelper.SwitchSchema();

           ActivationKey activationKey = new ActivationKey();
           ActivationMarshalling activationKeyMarshalling = new ActivationMarshalling();
           ActivationManager activationManager = new ActivationManager();
           activationKey = activationManager.GetCurrentActivationKeyInfoByApp(applicationOId,accountOid,digitalSignature);


           activationKeyMarshalling.AccountId = activationKey.AccountId;
           activationKeyMarshalling.AccountName = activationKey.AccountName;
           activationKeyMarshalling.ActivationKeyFile = activationKey.ActivationKeyFile;
           activationKeyMarshalling.Application = activationKey.Application.Oid.ToString();
           activationKeyMarshalling.ApplicationId = activationKey.ApplicationId;
           activationKeyMarshalling.ApplicationName = activationKey.ApplicationName;
           activationKeyMarshalling.ConfigurationItem = activationKey.ConfigurationItem.Oid;
           activationKeyMarshalling.Device = activationKey.Device.Oid.ToString();
           activationKeyMarshalling.DeviceName = activationKey.DeviceName;
           activationKeyMarshalling.DeviceSignature = activationKey.DeviceSignature;
           activationKeyMarshalling.DigitalSignature = activationKey.DigitalSignature;
           activationKeyMarshalling.EndDate = activationKey.EndDate;
           activationKeyMarshalling.GenerateDate = activationKey.GenerateDate;
           activationKeyMarshalling.ID = activationKey.ID;
           activationKeyMarshalling.InstallationType = activationKey.InstallationType.ToString();
           activationKeyMarshalling.NumberOfUsers = activationKey.NumberOfUsers;
           activationKeyMarshalling.StartDate = activationKey.StartDate;
           activationKeyMarshalling.VersionType = activationKey.VersionType.ToString();
           return activationKeyMarshalling;

       }

       [WebMethod(EnableSession = true)]
       public string GetCurrentActivationKeyId(string iD)
       {
          Helpers.SchemaHelper.SwitchSchema();
           
           ActivationKey activationKey = new ActivationKey();
           ActivationManager activationManager = new ActivationManager();
           activationKey.ActivationKeyFile = activationManager.GetCurrentActivationKeyId(iD).ActivationKeyFile;

           return activationKey.ActivationKeyFile;

       }
    }
}
