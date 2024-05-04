using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{

    [Serializable]
    public class ContactMarshalling
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
}