using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{
   

    [Serializable]
    public class ContactAddressMarshalling
    {
        private System.String _name;
        private System.String _addressLine1;
        private System.String _addressLine2;
        private System.String _addressLine3;
        private System.String _city;
        private System.String _state;
        private System.String _postalCode;
        private System.String _country;
        private System.String countryCode;
        private System.String regionName;

        public System.String Name
        {
            get
            {
                return _name;
            }
            set
            {
                _name = value;
            }
        }
        public System.String AddressLine1
        {
            get
            {
                return _addressLine1;
            }
            set
            {
                _addressLine1 = value;
            }
        }
        public System.String AddressLine2
        {
            get
            {
                return _addressLine2;
            }
            set
            {
                _addressLine2 = value;
            }
        }
        public System.String AddressLine3
        {
            get
            {
                return _addressLine3;
            }
            set
            {
                _addressLine3 = value;
            }
        }
        public System.String City
        {
            get
            {
                return _city;
            }
            set
            {
                _city = value;
            }
        }
        public System.String State
        {
            get
            {
                return _state;
            }
            set
            {
                _state = value;
            }
        }
        public System.String PostalCode
        {
            get
            {
                return _postalCode;
            }
            set
            {
                _postalCode = value;
            }
        }
        public System.String Country
        {
            get
            {
                return _country;
            }
            set
            {
                _country = value;
            }
        }
        public System.String CountryCode
        {
            get
            {
                return countryCode;
            }
            set
            {
                countryCode = value;
            }
        }
        public System.String RegionName
        {
            get
            {
                return regionName;
            }
            set
            {
                regionName = value;
            }
        }
    }

}