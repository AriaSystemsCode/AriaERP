using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{
    [Serializable]
    public class AriaMethodPermissionMarshalling
    {
        public string MethodName { set; get; }
        public bool AllowExecute { set; get; }
    }
}