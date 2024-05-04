using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Persistent.Validation;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{

    [Serializable]
    public class AccountMarshalling
    {
        private System.String _tradeName;
        private System.String _internalContactNo;
        private System.Int16 _guide;
        private System.String _name;
        private System.String _status;


        private System.Guid _categoryOid;


        public System.Guid CategoryOid
        {
            get
            {
                return _categoryOid;
            }
            set
            {
                _categoryOid = value;
            }
        }
        //Sara.M 01/26/2015 [Tracking# +Aria5-HTML5-AccountRegistration]_Programming [Start]
        private System.String _id;

        public System.String Id
        {
            get { return _id; }
            set { _id = value; }
        }

        //Sara.M 01/26/2015 [Tracking# +Aria5-HTML5-AccountRegistration]_Programming [End]
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


        ////Sara.M 01/26/2015 [Tracking# +Aria5-HTML5-AccountRegistration]_Programming  [Start]

        private System.Guid _accountOid;
        public System.Guid AccountOid
        {
            get
            {
                return _accountOid;
            }
            set
            {
                _accountOid = value;
            }
        }

        private System.String _categoryId;
        public System.String CategoryId
        {
            get
            {
                return _categoryId;
            }
            set
            {
                _categoryId = value;
            }
        }

        private System.String _categoryName;
        public System.String CategoryName
        {
            get
            {
                return _categoryName;
            }
            set
            {
                _categoryName = value;
            }
        }

        private System.String _classificationId;
        public System.String ClassificationId
        {
            get
            {
                return _classificationId;
            }
            set
            {
                _classificationId = value;
            }
        }


        //Sara.M 01/26/2015 [Tracking# +Aria5-HTML5-AccountRegistration]_Programming  [End]

    }


}
