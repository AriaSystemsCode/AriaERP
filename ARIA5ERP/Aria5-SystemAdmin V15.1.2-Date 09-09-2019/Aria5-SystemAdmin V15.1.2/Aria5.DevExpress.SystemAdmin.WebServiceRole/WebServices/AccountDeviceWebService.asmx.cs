using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module.Entities;
using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Configuration;
using System.Web.Services;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.WebServices
{
    /// <summary>
    /// Summary description for AccountDeviceWebService
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    // [System.Web.Script.Services.ScriptService]
    public class AccountDeviceWebService : System.Web.Services.WebService
    {

        [WebMethod(EnableSession = true)]
        public Guid AddNewDevice(AccountDeviceMarshalling deviceInfo)
        {
           Helpers.SchemaHelper.SwitchSchema();
            
            if (deviceInfo != null)
            {
                AccountDeviceManager deviceManager = new AccountDeviceManager();
                AccountDevice accountDevice = new AccountDevice(XpoDefault.Session);

                accountDevice.AccountId = deviceInfo.AccountId;
                accountDevice.AccountName = deviceInfo.AccountName;
                accountDevice.ASHWID = deviceInfo.ASHWID;
                accountDevice.DeviceSignature = deviceInfo.DeviceSignature;
                accountDevice.ID = deviceInfo.ID;
                accountDevice.Location = deviceInfo.Location;
                accountDevice.MACAddress = deviceInfo.MACAddress;
                accountDevice.Name = deviceInfo.Name;
                accountDevice.OperatingSystem = deviceInfo.OperatingSystem;
                accountDevice.ScreenSize = deviceInfo.ScreenSize;

                XPCollection accounts = new XPCollection(XpoDefault.Session, typeof(Account));
                CriteriaOperator criteriaAccount = new BinaryOperator("Oid", deviceInfo.Account, BinaryOperatorType.Equal);
                accounts.Criteria = criteriaAccount;
                accounts.Load();
                if (accounts.Count != 0)
                {
                    accountDevice.Account = (accounts[0] as Account);

                    return deviceManager.AddNewDevice(accountDevice);
                }
                else
                {
                    return Guid.Empty;
                }
            }
            else
            {
                return Guid.Empty;
            }

        }


        [WebMethod(EnableSession = true)]
        public Guid GetDeviceAccountOid(Guid accountOid, string deviceSignature)
        {
           Helpers.SchemaHelper.SwitchSchema(); 
            
            if (deviceSignature != null && deviceSignature != string.Empty)
            {
                AccountDeviceManager deviceManager = new AccountDeviceManager();
                return deviceManager.GetDeviceAccountOid(accountOid, deviceSignature);
            }
            else
            {
                return Guid.Empty;
            }
        }

        [WebMethod(EnableSession = true)]
        public Guid GetDeviceOid(Guid accountOid, string deviceSignature)
        {
          Helpers.SchemaHelper.SwitchSchema();

            if (deviceSignature != null && deviceSignature != string.Empty)
            {
                AccountDeviceManager deviceManager = new AccountDeviceManager();
                return deviceManager.GetDeviceOid(accountOid, deviceSignature);
            }
            else
            {
                return Guid.Empty;
            }

        }

        [WebMethod(EnableSession = true)]
        public List<AccountDeviceMarshalling> GetAccountDevices(Guid accountID)
        {
           Helpers.SchemaHelper.SwitchSchema();

            if (accountID != Guid.Empty && accountID != null)
            {
                AccountDeviceManager deviceManager = new AccountDeviceManager();

                List<AccountDeviceMarshalling> accountDeviceResultList = new List<AccountDeviceMarshalling>();
                List<AccountDevice> list = new List<AccountDevice>();

                list = deviceManager.GetAccountDevices(accountID);

                if (list == null)
                {
                    return null;
                }
                else if (list.Count != 0)
                {
                    for (int i = 0; i < list.Count; i++)
                    {
                        AccountDeviceMarshalling device = new AccountDeviceMarshalling();
                        device.Account = list[i].Account.Oid;
                        device.AccountId = list[i].AccountId;
                        device.AccountName = list[i].AccountName;
                        device.ASHWID = list[i].ASHWID;
                        device.DeviceSignature = list[i].DeviceSignature;
                        device.Location = list[i].Location;
                        device.MACAddress = list[i].MACAddress;
                        device.OperatingSystem = list[i].OperatingSystem;
                        device.ScreenSize = list[i].ScreenSize;
                        device.ID = list[i].ID;
                        device.Name = list[i].Name;

                        accountDeviceResultList.Add(device);
                    }
                    return accountDeviceResultList;
                }
                else
                {
                    return null;
                }
            }
            else
            {
                return null;
            }

        }

        [WebMethod(EnableSession = true)]
        public string GetDeviceAccountId(string deviceSignature)
        {
            Helpers.SchemaHelper.SwitchSchema();

            if (deviceSignature != null && deviceSignature != string.Empty)
            {
                AccountDeviceManager deviceManager = new AccountDeviceManager();
                return deviceManager.GetDeviceAccountId(deviceSignature);
            }
            else
            {
                return null;
            }

        }

        [WebMethod(EnableSession = true)]
        public AccountDeviceMarshalling GetDeviceBusinessObject(string deviceSignature)
        {
          Helpers.SchemaHelper.SwitchSchema();

            if (deviceSignature != null && deviceSignature != string.Empty)
            {
                AccountDeviceManager deviceManager = new AccountDeviceManager();
                AccountDevice device = new AccountDevice(new Session());
                device = deviceManager.GetAccountDeviceObject(deviceSignature);
                AccountDeviceMarshalling accountDeviceResult = new AccountDeviceMarshalling();

                accountDeviceResult.Account = device.Account.Oid;
                accountDeviceResult.AccountId = device.AccountId;
                accountDeviceResult.AccountName = device.AccountName;
                accountDeviceResult.ASHWID = device.ASHWID;
                accountDeviceResult.DeviceSignature = device.DeviceSignature;
                accountDeviceResult.Location = device.Location;
                accountDeviceResult.MACAddress = device.MACAddress;
                accountDeviceResult.OperatingSystem = device.OperatingSystem;
                accountDeviceResult.ScreenSize = device.ScreenSize;
                accountDeviceResult.ID = device.ID;
                accountDeviceResult.Name = device.Name;

                return accountDeviceResult;
            }
            else
            {
                return null;
            }

        }

        [WebMethod(EnableSession = true)]
        public bool AuthanticateDevice(Guid accountOid, string deviceSignature)
        {
           Helpers.SchemaHelper.SwitchSchema();

            if (deviceSignature != null && deviceSignature != string.Empty && accountOid != Guid.Empty && accountOid != null)
            {
                AccountDeviceManager deviceManager = new AccountDeviceManager();
                return deviceManager.IsAuthanticateDevice(accountOid, deviceSignature);
            }
            else
            {
                return false;
            }

        }
    }
}
