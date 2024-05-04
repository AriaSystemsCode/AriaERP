using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module.DataTypes
{
    [Serializable]
    public class ActivationDataType
    {

        private System.String _digitalSignature;
        private System.DateTime _expireDate;
        private System.Int32 _numberOfUsers;
        private System.String _versionType;
        private System.String _installationType;
        private System.String _application;
        private System.String _deviceName;
        private System.String _deviceSignature;
        private System.String _accountName;
        private System.String accountCode;
        private System.String activationKeyID;


        public System.String ActivationKeyID
        {
            get
            {
                return activationKeyID;
            }
            set
            {
                activationKeyID = value;
            }
        }

        public System.String AccountCode
        {
            get
            {
                return accountCode;
            }
            set
            {
                accountCode = value;
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


        public System.DateTime ExpireDate
        {
            get
            {
                return _expireDate;
            }
            set
            {
                _expireDate = value;
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
