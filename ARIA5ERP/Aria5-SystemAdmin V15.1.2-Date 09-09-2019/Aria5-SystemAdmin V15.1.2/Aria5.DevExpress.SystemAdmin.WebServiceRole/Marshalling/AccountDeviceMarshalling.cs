using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{
    [Serializable]
    public class AccountDeviceMarshalling
    {


        private string iD;

        public string ID
        {
            get
            {
                return iD;
            }
            set
            {
                iD = value;
            }
        }

        private Guid account;
        //Property of Account
        public Guid Account
        {
            get
            {
                return account;
            }
            set
            {
                account = value;
            }
        }

        //Property of Account ID
        private string accountId;

        public string AccountId
        {
            get
            {
                return accountId;
            }
            set
            {
                accountId = value;
            }
        }
        //Property of Account Name
        private string accountName;
        public string AccountName
        {
            get
            {
                return accountName;
            }
            set
            {
                accountName = value;
            }
        }
        //Property of Device Name
        private string name;
        public string Name
        {
            get
            {
                return name;
            }
            set
            {
                name = value;
            }
        }
        //Property of Media Access Control Address
        private string mACAddress;
        public string MACAddress
        {
            get
            {
                return mACAddress;
            }
            set
            {
                mACAddress = value;
            }
        }
        //Property of App Specific Hardware ID
        private string aSHWID;
        public string ASHWID
        {
            get
            {
                return aSHWID;
            }
            set
            {
                aSHWID = value;
            }
        }
        //Property of Screen Size
        private string screenSize;
        public string ScreenSize
        {
            get
            {
                return screenSize;
            }
            set
            {
                screenSize = value;
            }
        }
        //Property of Device Signature
        private string deviceSignature;
        public string DeviceSignature
        {
            get
            {
                return deviceSignature;
            }
            set
            {
                deviceSignature = value;
            }
        }
        //Property of Operation System
        private string operatingSystem;
        public string OperatingSystem
        {
            get
            {
                return operatingSystem;
            }
            set
            {
                operatingSystem = value;
            }
        }
        //Property of Operation System
        private Guid location;
        public Guid Location
        {
            get
            {
                return location;
            }
            set
            {
                location = value;
            }
        }
    }
}