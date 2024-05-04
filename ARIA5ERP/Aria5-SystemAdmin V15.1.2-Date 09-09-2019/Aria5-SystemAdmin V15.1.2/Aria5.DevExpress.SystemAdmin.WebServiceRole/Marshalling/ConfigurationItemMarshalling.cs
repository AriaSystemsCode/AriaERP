using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{
    [Serializable]
    public class ConfigurationItemMarshalling
    {

        private string _deviceSignature;
        public string DeviceSignature
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
        private string configurationItemDescription;
        public string ConfigurationItemDescription
        {
            get
            {
                return configurationItemDescription;
            }
            set
            {
                configurationItemDescription = value;
            }
        }

        private Guid oid;

        public Guid Oid
        {
            get { return oid; }
            set { oid = value; }
        }

        private Guid accountOid;
        public Guid AccountOid
        {
            get
            {
                return accountOid;
            }
            set
            {
                accountOid = value;
            }
        }

        private Guid contactOid;
        public Guid ContactOid
        {
            get
            {
                return contactOid;
            }
            set
            {
                contactOid = value;
            }
        }

        private Guid deviceOid;
        public Guid DeviceOid
        {
            get
            {
                return deviceOid;
            }
            set
            {
                deviceOid = value;
            }
        }

        private Guid applicationOid;
        public Guid ApplicationOid
        {
            get
            {
                return applicationOid;
            }
            set
            {
                applicationOid = value;
            }
        }

        private string installationType;
        public string InstallationType
        {
            get
            {
                return installationType;
            }
            set
            {
                installationType = value;
            }
        }

        private string versionType;
        public string VersionType
        {
            get
            {
                return versionType;
            }
            set
            {
                versionType = value;
            }
        }

        private DateTime startDate;
        public DateTime StartDate
        {
            get
            {
                return startDate;
            }
            set
            {
                startDate = value;
            }
        }

        private DateTime endDate;
        public DateTime EndDate
        {
            get
            {
                return endDate;
            }
            set
            {
                endDate = value;
            }
        }

        private Int16 numberOfUsers;
        public Int16 NumberOfUsers
        {
            get { return numberOfUsers; }
            set { numberOfUsers = value; }
        }

        private Guid demoAccount;
        public Guid DemoAccount
        {
            get { return demoAccount; }
            set { demoAccount = value; }
        }

        private System.String _activationKeyID;
        public System.String ActivationKeyID
        {
            get
            {
                return _activationKeyID;
            }
            set
            {
                _activationKeyID = value;
            }
        }

        private System.Guid _activationKey;
        public System.Guid ActivationKey
        {
            get
            {
                return _activationKey;
            }
            set
            {
                _activationKey = value;
            }
        }

       //Mina.B 2015-07-28 [Begin]
        private string _status;
        public string Status
        {
            get
            {
                return _status;
            }
            set
            {
                _status = value;
            }
        }
        private string _contactName;
        public string ContactName
        {
            get
            {
                return _contactName;
            }
            set
            {
                _contactName = value;
            }
        }
        //Mina.B 2015-07-28 [End]


    }

}