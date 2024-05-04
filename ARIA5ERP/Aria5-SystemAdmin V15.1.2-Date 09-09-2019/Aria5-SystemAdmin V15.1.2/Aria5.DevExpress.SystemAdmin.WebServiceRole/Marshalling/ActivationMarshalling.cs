using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{
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
