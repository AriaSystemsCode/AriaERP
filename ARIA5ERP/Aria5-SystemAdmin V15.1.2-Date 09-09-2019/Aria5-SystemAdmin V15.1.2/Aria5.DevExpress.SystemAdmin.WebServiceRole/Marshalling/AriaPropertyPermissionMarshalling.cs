using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{
    [Serializable]
    public class AriaPropertyPermissionMarshalling
    {
        public string PropertyName { set; get; }
        public bool AllowRead { set; get; }
        public bool AllowWrite { set; get; }
    }
}