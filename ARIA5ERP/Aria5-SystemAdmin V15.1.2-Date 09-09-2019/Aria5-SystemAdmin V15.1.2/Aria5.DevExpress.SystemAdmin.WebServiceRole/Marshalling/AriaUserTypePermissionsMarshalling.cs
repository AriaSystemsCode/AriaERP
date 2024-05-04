using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{
    [Serializable]
    public class AriaUserTypePermissionsMarshalling
    {
        public string TargetType { get; set; }
        public int ObjectType { get; set; }
        public bool AllowRead { get; set; }
        public bool AllowWrite { get; set; }
        public bool AllowCreate { get; set; }
        public bool AllowDelete { get; set; }
        public bool AllowNavigate { get; set; }
        public List<AriaMethodPermissionMarshalling> MethodPermission { get; set; }
        public List<AriaPropertyPermissionMarshalling> PropertyPermission { get; set; }
        public List<AriaUserObjectPermissionsMarshalling> UserObjectPermissions { get; set; }
    }
}