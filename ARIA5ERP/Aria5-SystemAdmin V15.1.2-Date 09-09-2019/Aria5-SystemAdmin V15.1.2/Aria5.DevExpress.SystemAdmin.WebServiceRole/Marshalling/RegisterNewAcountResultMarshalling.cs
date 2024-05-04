using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{

    [Serializable]
    public class RegisterNewAcountResultMarshalling
    {
        private System.Guid accountOid;
        private System.Guid personOid;

        public System.Guid PersonGuid
        {
            get { return personOid; }
            set { personOid = value; }
        }

        public System.Guid AccountGuid
        {
            get { return accountOid; }
            set { accountOid = value; }
        }

    }

}