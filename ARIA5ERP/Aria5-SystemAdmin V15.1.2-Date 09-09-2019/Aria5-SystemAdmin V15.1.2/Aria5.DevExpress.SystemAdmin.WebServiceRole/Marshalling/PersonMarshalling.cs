using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{

    [Serializable]
    public class PersonMarshalling
    {
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
        private System.String _name;
        private System.String _eMailAddress;
        private System.String _webPageAddress;
        private System.String _parentContact;

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

}