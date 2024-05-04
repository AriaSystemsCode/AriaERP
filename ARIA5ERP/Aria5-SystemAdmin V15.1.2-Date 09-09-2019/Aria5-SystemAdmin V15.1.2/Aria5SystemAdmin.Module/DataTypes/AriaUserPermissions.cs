using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module.DataTypes
{
    [Serializable]
    public class AriaUserPermissions
    {
        public bool IsAdministrator { set; get; }
        public List<AriaUserTypePermissions> UserTypePermissions { set; get; }
    }
}
