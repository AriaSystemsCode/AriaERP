using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module.DataTypes
{
    [Serializable]
    public class AriaMethodPermission
    {
        public string MethodName { set; get; }
        public bool AllowExecute {set;get;}
    }
}
