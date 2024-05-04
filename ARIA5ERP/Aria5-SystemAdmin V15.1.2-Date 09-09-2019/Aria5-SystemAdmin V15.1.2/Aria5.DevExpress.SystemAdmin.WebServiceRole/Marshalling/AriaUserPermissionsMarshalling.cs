using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{
    [Serializable]
    public class AriaUserPermissionsMarshalling
    {
        public bool IsAdministrator { set; get; }
        public List<AriaUserTypePermissionsMarshalling> UserTypePermissions { set; get; }
    }
}