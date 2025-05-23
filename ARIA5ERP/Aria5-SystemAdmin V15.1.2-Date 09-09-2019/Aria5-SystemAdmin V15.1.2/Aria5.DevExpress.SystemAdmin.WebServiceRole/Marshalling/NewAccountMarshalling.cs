﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{
     [Serializable]
    public class NewAccountMarshalling
    {

        #region account 

        private System.String _tradeName;
        private System.String _internalContactNo;
        private System.Int16 _guide;
        private System.String _accountName;
        private System.String _status;
        private System.String _accountDescription;

        public System.String AccountDescription
        {
            get { return _accountDescription; }
            set { _accountDescription = value; }
        }


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
        public System.String StatusSer
        {
            get { return _status; }
            set { _status = value; }
        }
        #endregion

        #region address
        private System.String _addressContactName;
        private System.String _addressLine1;
        private System.String _addressLine2;
        private System.String _addressLine3;
        private System.String _city;
        private System.String _state;
        private System.String _postalCode;
        private System.String _country;
        private System.String countryCode;
        private System.String regionName;
        private System.String addressTypeDescription;

        public System.String AddressTypeDescription
        {
            get { return addressTypeDescription; }
            set { addressTypeDescription = value; }
        }
        public System.String AddressContactName
        {
            get
            {
                return _addressContactName;
            }
            set
            {
                _addressContactName = value;
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
        #endregion

        #region person
        private System.String _firstName;
        private System.String _middleName;
        private System.Guid _department;
        private System.String _lastName;
        private System.String _nickName;
        private System.DateTime _birthDate;
        private System.String _spouseName;
        private System.String _sSN;
        private System.String _salutation;
        private System.String _positionTitle;
        private System.Guid _position;
        private System.String _departmentName;
        private System.String _titleOfCourtesy;
        private System.String _personName;
        private System.String _eMailAddress;
        private System.String _webPageAddress;
        private System.String _parentContact;
        private System.Guid personOid;
        private System.String _personDescription;

        public System.String PersonDescription
        {
            get { return _personDescription; }
            set { _personDescription = value; }
        }

        public System.Guid PersonOid
        {
            get { return personOid; }
            set { personOid = value; }
        }

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
        public System.String PersonName
        {
            get
            {
                return _personName;
            }
            set
            {
                _personName = value;
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
        #endregion

        #region phone
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

        #endregion

    }
}