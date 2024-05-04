using Aria5.DevExpress.MainSystem.Module.Managers;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;



namespace Aria5SystemAdmin.Module.Entities
{
    public class AccountDeviceManager
    {
        public Guid AddNewDevice(AccountDevice deviceInfo)
        {
            // Get Account
            XPCollection accounts = new XPCollection(typeof(Account));

            CriteriaOperator criteriaAccount = new BinaryOperator("Oid", deviceInfo.Account.Oid, BinaryOperatorType.Equal);
            accounts.Criteria = criteriaAccount;
            accounts.Load();

            if (accounts.Count != 0 && deviceInfo != null)
            {
                // Account exist
                XPCollection devices = new XPCollection(XpoDefault.Session, typeof(AccountDevice));

                CriteriaOperator criteria = new BinaryOperator("DeviceSignature", deviceInfo.DeviceSignature, BinaryOperatorType.Equal);
                devices.Criteria = criteria;
                devices.Load();

                if (devices.Count == 0)
                {
                    // Device is not exist
                    AccountDevice newDevice = new AccountDevice(XpoDefault.Session);
                    string IdentifierStructureValue = IdentifierStructureStub();

                    newDevice.AccountId = deviceInfo.Account.Id;
                    newDevice.Account = deviceInfo.Account;
                    newDevice.AccountName = deviceInfo.Account.Name;

                    newDevice.ASHWID = deviceInfo.ASHWID;
                    newDevice.ID = IdentifierStructureValue;
                    newDevice.Location = deviceInfo.Location;
                    newDevice.MACAddress = deviceInfo.MACAddress;
                    newDevice.Name = deviceInfo.Name;
                    newDevice.OperatingSystem = deviceInfo.OperatingSystem;
                    newDevice.ScreenSize = deviceInfo.ScreenSize;

                    if (deviceInfo.DeviceSignature != null && deviceInfo.DeviceSignature != string.Empty)
                    {
                        newDevice.DeviceSignature = deviceInfo.DeviceSignature;
                    }
                    else if (deviceInfo.ASHWID != null && deviceInfo.ASHWID != string.Empty)
                    {
                        newDevice.DeviceSignature = deviceInfo.ASHWID;
                    }
                    else
                    {
                        newDevice.DeviceSignature = deviceInfo.MACAddress;
                    }

                    newDevice.Save();

                    return newDevice.Oid;
                }
                else if (((AccountDevice)devices[0]).Account.Oid != deviceInfo.Account.Oid)
                {
                    //Device exist in other account
                    //throw new Exception("Can't add device");
                    AccountDevice newDevice = new AccountDevice(XpoDefault.Session);
                    string IdentifierStructureValue = IdentifierStructureStub();

                    newDevice.AccountId = deviceInfo.Account.Id;
                    newDevice.Account = deviceInfo.Account;
                    newDevice.AccountName = deviceInfo.Account.Name;

                    newDevice.ASHWID = deviceInfo.ASHWID;
                    newDevice.ID = IdentifierStructureValue;
                    newDevice.Location = deviceInfo.Location;
                    newDevice.MACAddress = deviceInfo.MACAddress;
                    newDevice.Name = deviceInfo.Name;
                    newDevice.OperatingSystem = deviceInfo.OperatingSystem;
                    newDevice.ScreenSize = deviceInfo.ScreenSize;

                    if (deviceInfo.DeviceSignature != null && deviceInfo.DeviceSignature != string.Empty)
                    {
                        newDevice.DeviceSignature = deviceInfo.DeviceSignature;
                    }
                    else if (deviceInfo.ASHWID != null && deviceInfo.ASHWID != string.Empty)
                    {
                        newDevice.DeviceSignature = deviceInfo.ASHWID;
                    }
                    else
                    {
                        newDevice.DeviceSignature = deviceInfo.MACAddress;
                    }

                    newDevice.Save();

                    return newDevice.Oid;
                }
                else
                {
                    //Device exist
                    ((AccountDevice)devices[0]).Save();
                    return (devices[0] as AccountDevice).Oid;
                }
            }
            else
            {
                //Account not exist
                throw new Exception("Can't add device");
            }
        }

        private string IdentifierStructureStub()
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

        public Guid GetDeviceAccountOid(Guid accountOid, string deviceSignature)
        {
            XPCollection<AccountDevice> devices = new XPCollection<AccountDevice>((CriteriaOperator.Parse("[Account] = '" + accountOid + "' and [DeviceSignature]='" + deviceSignature + "'")));

            //XPCollection devices = new XPCollection(typeof(AccountDevice));

            //CriteriaOperator criteria = new BinaryOperator("DeviceSignature", deviceSignature, BinaryOperatorType.Equal);
            //devices.Criteria = criteria;
            devices.Load();

            if (devices.Count != 0)
            {
                return (devices[0] as AccountDevice).Account.Oid;
            }
            else
            {
                throw new Exception("Device is not found");
            }
        }


        public Guid GetDeviceOid(Guid accountOid, string deviceSignature)
        {
            XPCollection<AccountDevice> devices = new XPCollection<AccountDevice>((CriteriaOperator.Parse("[Account] = '" + accountOid + "' and [DeviceSignature]='" + deviceSignature + "'")));

            //XPCollection devices = new XPCollection(typeof(AccountDevice));

            //CriteriaOperator criteria = new BinaryOperator("DeviceSignature", deviceSignature, BinaryOperatorType.Equal);
            //devices.Criteria = criteria;
            devices.Load();

            if (devices.Count != 0)
            {
                return (devices[0] as AccountDevice).Oid;
            }
            else
            {
                throw new Exception("Device is not found");
            }

        }


        public List<AccountDevice> GetAccountDevices(Guid account)
        {


            XPCollection devices = new XPCollection(XpoDefault.Session, typeof(AccountDevice));
            List<AccountDevice> deviceList = new List<AccountDevice>();

            //CriteriaOperator criteria = new BinaryOperator("Account.Oid", account, BinaryOperatorType.Equal);
            CriteriaOperator criteria = CriteriaOperator.Parse("[Account] = '" + account + "'");
            devices.Criteria = criteria;
            devices.Load();


            if (devices.Count != 0)
            {
                foreach (AccountDevice device in devices)
                {
                    deviceList.Add(device);
                }
                return deviceList;
            }
            else
            {
                throw new Exception("Devices are not found");
            }
        }

        public string GetDeviceAccountId(string deviceSignature)
        {
            XPCollection devices = new XPCollection(typeof(AccountDevice));

            CriteriaOperator criteria = new BinaryOperator("DeviceSignature", deviceSignature, BinaryOperatorType.Equal);
            devices.Criteria = criteria;
            devices.Load();

            if (devices.Count != 0)
            {
                return (devices[0] as AccountDevice).AccountId;
            }
            else
            {
                throw new Exception("Device is not found");
            }
        }

        public AccountDevice GetAccountDeviceObject(string deviceSignature)
        {
            XPCollection devices = new XPCollection(typeof(AccountDevice));

            CriteriaOperator criteria = new BinaryOperator("DeviceSignature", deviceSignature, BinaryOperatorType.Equal);
            devices.Criteria = criteria;
            devices.Load();

            if (devices.Count != 0)
            {
                return (devices[0] as AccountDevice);
            }
            else
            {
                throw new Exception("Device is not found");
            }
        }


        public bool IsAuthanticateDevice(Guid accountOid, string deviceSignature)
        {
            if (deviceSignature != null && deviceSignature != string.Empty && accountOid != Guid.Empty && accountOid != null)
            {
                // Get device
                XPCollection devices = new XPCollection(XpoDefault.Session, typeof(AccountDevice));

                //CriteriaOperator criteriaAccount = new BinaryOperator("DeviceSignature", deviceSignature, BinaryOperatorType.Equal);
                CriteriaOperator criteriaAccount = CriteriaOperator.Parse("[Account.Oid] = '" + accountOid + "' and [DeviceSignature]='" + deviceSignature + "'");

                devices.Criteria = criteriaAccount;
                devices.Load();

                if (devices.Count != 0)
                {
                    return true;
                    //if(((AccountDevice)devices[0]).Account.Oid == accountOid)
                    //{
                    //    return true;
                    //}
                    //else //if(((AccountDevice)devices[0]).Account.Oid == accountOid)
                    //{
                    //    return false;
                    //}
                }
                else
                {
                    throw new Exception("Device is not found");
                }
            }
            else
            {
                throw new Exception("Some parameters are not valid");
            }
            //// Account exist
            //XPCollection devices = new XPCollection(typeof(AccountDevice));

            //CriteriaOperator criteria = new BinaryOperator("DeviceSignature", deviceSignature, BinaryOperatorType.Equal);
            //devices.Criteria = criteria;
            //devices.Load();

            //if (devices.Count != 0)
            //{
            //    return true;
            //}
            //else
            //{
            //    return false;
            //}
            //    }
            //    else
            //    {
            //        return false;
            //    }
            //}
            //else
            //{
            //    return false;
            //}
        }
    }
}
