using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module.Managers;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Aria5.DevExpress.MainSystem.CloudService.TestSara
{
    public partial class FormGenerateActivationKeyByManager : Form
    {
        public FormGenerateActivationKeyByManager()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            GenerateNewActivationKey(Guid.Parse(textBox1.Text));
        }

        public ActivationMarshalling GenerateNewActivationKey(Guid configrationItemOid)
        {
            StartConnection();
            ActivationKey activationKey = new ActivationKey();
            ActivationMarshalling activationKeyMarshalling = new ActivationMarshalling();
            ActivationManager activationManager = new ActivationManager();
            activationKey = activationManager.GenerateNewActivationKey(configrationItemOid);

            activationKeyMarshalling.AccountId = activationKey.AccountId;
            activationKeyMarshalling.AccountName = activationKey.AccountName;
            activationKeyMarshalling.ActivationKeyFile = activationKey.ActivationKeyFile;
            activationKeyMarshalling.Application = activationKey.Application.Oid.ToString();
            activationKeyMarshalling.ApplicationId = activationKey.ApplicationId;
            activationKeyMarshalling.ApplicationName = activationKey.ApplicationName;
           // activationKeyMarshalling.ConfigurationItem = activationKey.ConfigurationItem;
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

        public bool IsActivationKeyFound(Guid configrationItemOid)
        {
            StartConnection();
            ActivationManager activationManager = new ActivationManager();
            return activationManager.IsActivationKeyFound(configrationItemOid);
        }

        public ActivationMarshalling GetCurrentActivationKeyInfo(Guid configrationItemOid)
        {
            StartConnection();
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
           // activationKeyMarshalling.ConfigurationItem = activationKey.ConfigurationItem;
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

        public ActivationMarshalling GetCurrentActivationKeyInfoByDeviceAndApplication(Guid applicationOId, Guid accountOid, string digitalSignature)
        {
            StartConnection();
            ActivationKey activationKey = new ActivationKey();
            ActivationMarshalling activationKeyMarshalling = new ActivationMarshalling();
            ActivationManager activationManager = new ActivationManager();
            //activationKey = activationManager.GetCurrentActivationKeyInfo(applicationOId, accountOid, digitalSignature);


            activationKeyMarshalling.AccountId = activationKey.AccountId;
            activationKeyMarshalling.AccountName = activationKey.AccountName;
            activationKeyMarshalling.ActivationKeyFile = activationKey.ActivationKeyFile;
            activationKeyMarshalling.Application = activationKey.Application.Oid.ToString();
            activationKeyMarshalling.ApplicationId = activationKey.ApplicationId;
            activationKeyMarshalling.ApplicationName = activationKey.ApplicationName;
           // activationKeyMarshalling.ConfigurationItem = activationKey.ConfigurationItem;
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

        public string GetCurrentActivationKeyId(string iD)
        {
            StartConnection();

            ActivationManager activationManager = new ActivationManager();
            return null; // activationManager.GetCurrentActivationKeyId(iD);
        }

        public void StartConnection()
        {
            XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=SystemAdminTestingUser1;Password=aria_123";

            Account temp = new Account(XpoDefault.Session);

            string schemaName = "Test4";

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

        }
    }



    [Serializable]
    public class ActivationMarshalling
    {
        private System.String _digitalSignature;
        private System.String _activationKeyFile;
        private System.DateTime _endDate;
        private System.DateTime _startDate;
        private System.Int32 _numberOfUsers;
        private System.String _versionType;
        private System.String _installationType;
        private System.String _applicationName;
        private System.String _applicationId;
        private System.String _application;
        private System.String _deviceName;
        private System.String _deviceSignature;
        private System.String _device;
        private System.String _accountName;
        private System.String _accountId;
        private System.DateTime _generateDate;
        private System.Guid _configurationItem;
        private System.String _iD;


        public System.String ID
        {
            get
            {
                return _iD;
            }
            set
            {
                _iD = value;
            }
        }

        public System.Guid ConfigurationItem
        {
            get
            {
                return _configurationItem;
            }
            set
            {
                _configurationItem = value;
            }
        }

        public System.DateTime GenerateDate
        {
            get
            {
                return _generateDate;
            }
            set
            {
                _generateDate = value;
            }
        }


        public System.String AccountId
        {
            get
            {
                return _accountId;
            }
            set
            {
                _accountId = value;
            }
        }

        public System.String AccountName
        {
            get
            {
                return _accountName;
            }
            set
            {
                _accountName = value;
            }
        }
        public System.String Device
        {
            get
            {
                return _device;
            }
            set
            {
                _device = value;
            }
        }
        public System.String DeviceSignature
        {
            get
            {
                return _deviceSignature;
            }
            set
            {
                _deviceSignature = value;
            }
        }
        public System.String DeviceName
        {
            get
            {
                return _deviceName;
            }
            set
            {
                _deviceName = value;
            }
        }

        public System.String Application
        {
            get
            {
                return _application;
            }
            set
            {
                _application = value;
            }
        }

        public System.String ApplicationId
        {
            get
            {
                return _applicationId;
            }
            set
            {
                _applicationId = value;
            }
        }

        public System.String ApplicationName
        {
            get
            {
                return _applicationName;
            }
            set
            {
                _applicationName = value;
            }
        }

        public System.String InstallationType
        {
            get
            {
                return _installationType;
            }
            set
            {
                _installationType = value;
            }
        }

        public System.String VersionType
        {
            get
            {
                return _versionType;
            }
            set
            {
                _versionType = value;
            }
        }
        public System.Int32 NumberOfUsers
        {
            get
            {
                return _numberOfUsers;
            }
            set
            {
                _numberOfUsers = value;
            }
        }

        public System.DateTime StartDate
        {
            get
            {
                return _startDate;
            }
            set
            {
                _startDate = value;
            }
        }

        public System.DateTime EndDate
        {
            get
            {
                return _endDate;
            }
            set
            {
                _endDate = value;
            }
        }

        public System.String ActivationKeyFile
        {
            get
            {
                return _activationKeyFile;
            }
            set
            {
                _activationKeyFile = value;
            }
        }
        public System.String DigitalSignature
        {
            get
            {
                return _digitalSignature;
            }
            set
            {
                _digitalSignature = value;
            }
        }
    }

}
