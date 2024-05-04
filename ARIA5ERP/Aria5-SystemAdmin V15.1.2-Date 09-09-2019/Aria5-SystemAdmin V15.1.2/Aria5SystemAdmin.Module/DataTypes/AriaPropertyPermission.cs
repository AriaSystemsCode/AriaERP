using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module.DataTypes
{
    [Serializable]
    public class AriaPropertyPermission
    {
        public string PropertyName { set; get; }
        public bool AllowRead { set; get; }
        public bool AllowWrite { set; get; }
    }
}
