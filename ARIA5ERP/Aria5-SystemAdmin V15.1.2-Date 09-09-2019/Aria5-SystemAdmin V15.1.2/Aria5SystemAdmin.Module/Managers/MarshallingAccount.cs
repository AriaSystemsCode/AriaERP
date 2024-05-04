using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Persistent.Validation;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module.Managers
{
    //[Serializable]
    //public class SerializableAccountRegistration
    //{

    //}

    [Serializable]
    public class MarshallingContact
    {
        private System.String _name;
        private System.String _eMailAddress;
        private System.String _webPageAddress;
        private System.String _parentContact;

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
        public System.String EMailAddress
        {
            get
            {
                return _eMailAddress;
            }
            set
            {
                _eMailAddress = value;
            }
        }
        public System.String WebPageAddress
        {
            get
            {
                return _webPageAddress;
            }
            set
            {
                _webPageAddress = value;
            }
        }
        public System.String ParentContact
        {
            get
            {
                return _parentContact;
            }
            set
            {
                _parentContact = value;
            }
        }


    }

    
    //public class MarshallingPerson : MarshallingContact
    [Serializable]
    public class MarshallingPerson 
    {
        private System.String   _firstName;
        private System.String   _middleName;
        private System.Guid     _department;
        private System.String   _lastName;
        private System.String   _nickName;
        private System.DateTime _birthDate;
        private System.String   _spouseName;
        private System.String   _sSN;
        private System.String   _salutation;
        private System.String   _positionTitle;
        private System.Guid     _position;
        private System.String   _departmentName;
        private System.String   _titleOfCourtesy;
        private System.String   _name;
        private System.String   _eMailAddress;
        private System.String   _webPageAddress;
        private System.String   _parentContact;

        public System.String FirstName
        {
            get
            {
                return _firstName;
            }
            set
            {
                _firstName = value;
            }
        }
        public System.String MiddleName
        {
            get
            {
                return _middleName;
            }
            set
            {
                _middleName = value;
            }
        }
        public Guid Department
        {
            get { return _department; }
            set { _department = value; }
        }
        public System.String LastName
        {
            get
            {
                return _lastName;
            }
            set
            {
                _lastName = value;
            }
        }
        public System.String NickName
        {
            get
            {
                return _nickName;
            }
            set
            {
                _nickName = value;
            }
        }
        public System.DateTime BirthDate
        {
            get
            {
                return _birthDate;
            }
            set
            {
                _birthDate = value;
            }
        }
        public System.String SpouseName
        {
            get
            {
                return _spouseName;
            }
            set
            {
                _spouseName = value;
            }
        }
        public System.String SSN
        {
            get
            {
                return _sSN;
            }
            set
            {
                _sSN = value;
            }
        }
        public System.String Salutation
        {
            get
            {
                return _salutation;
            }
            set
            {
                _salutation = value;
            }
        }
        public System.String PositionTitle
        {
            get
            {
                return _positionTitle;
            }
            set
            {
                 _positionTitle = value;
            }
        }
        public Guid Position
        {
            get
            {
                return _position;
            }
            set
            {
                _position = value;
            }
        }
        public System.String DepartmentName
        {
            get
            {
                return _departmentName;
            }
            set
            {
                _departmentName = value;
            }
        }
        public System.String TitleOfCourtesy
        {
            get
            {
                return _titleOfCourtesy;
            }
            set
            {
                _titleOfCourtesy = value;
            }
        }
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
        public System.String EMailAddress
        {
            get
            {
                return _eMailAddress;
            }
            set
            {
                _eMailAddress = value;
            }
        }
        public System.String WebPageAddress
        {
            get
            {
                return _webPageAddress;
            }
            set
            {
                _webPageAddress = value;
            }
        }
        public System.String ParentContact
        {
            get
            {
                return _parentContact;
            }
            set
            {
                _parentContact = value;
            }
        }

    }
    // [Serializable]
    //public enum TitleOfCourtesyenum 
    //{
    //    Dr,
    //    Miss,
    //    Mr,
    //    Mrs,
    //    Ms
    //};

    [Serializable]
    public class MarshallingContactAddress
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

    //public class MarshallingContactPhone : MarshallingContact
    [Serializable]
    public class MarshallingContactPhone
    {
        private System.Guid phonetype;
        private System.String _phoneNumber;
        private System.String _extension;
        private System.String _phoneTypeDescription;


        public System.Guid PhoneType
        {
            get
            {
                return phonetype;
            }
            set
            {
                phonetype = value;
            }
        }
        public System.String PhoneNumber
        {
            get
            {
                return _phoneNumber;
            }
            set
            {
                _phoneNumber = value;
            }
        }
        public System.String Extension
        {
            get
            {
                return _extension;
            }
            set
            {
                _extension = value;
            }
        }
        public System.String PhoneTypeDescription
        {
            get
            {
                return _phoneTypeDescription;
            }
            set
            {
                _phoneTypeDescription = value;
            }
        }
       
    }

    //[Serializable]
    //public class AccountSerializable : BusinessSerializable
    //{ }

    //public class AccountSerializable : MarshallingContact
    [Serializable]
    public class AccountSerializable
    {
        private System.String _tradeName;
        private System.String _internalContactNo;
        private System.Int16 _guide;
        private System.String _name;
        private System.String _status;


        public System.String TradeName
        {
            get
            {
                return _tradeName;
            }
            set
            {
                _tradeName = value;
            }
        }
        public System.String InternalContactNo
        {
            get
            {
                return _internalContactNo;
            }
            set
            {
                _internalContactNo = value;
            }
        }
        public System.Int16 Guide
        {
            get
            {
                return _guide;
            }
            set
            {
                _guide = value;
            }
        }
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
        public System.String StatusSer
        {
            get { return _status; }
            set { _status = value; }
        }


    }


    [Serializable]
    public class RegisterNewAountResultMarshalling
    {
        private System.Guid guidAccount;
        private System.Guid guidPerson;

        public System.Guid GuidPerson
        {
            get { return guidPerson; }
            set { guidPerson = value; }
        }

        public System.Guid GuidAccount
        {
            get { return guidAccount; }
            set { guidAccount = value; }
        }

    }
}
