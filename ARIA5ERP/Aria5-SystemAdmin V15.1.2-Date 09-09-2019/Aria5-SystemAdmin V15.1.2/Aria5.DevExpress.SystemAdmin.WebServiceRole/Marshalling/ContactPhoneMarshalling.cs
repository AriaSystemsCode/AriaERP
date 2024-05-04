using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{
  

    [Serializable]
    public class ContactPhoneMarshalling
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

}