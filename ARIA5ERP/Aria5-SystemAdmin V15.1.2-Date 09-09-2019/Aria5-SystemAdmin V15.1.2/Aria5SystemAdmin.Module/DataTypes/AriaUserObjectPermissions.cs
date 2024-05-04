using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module.DataTypes
{
    [Serializable]
    public class AriaUserObjectPermissions
    {
        public string TargetType { get; set; }
        public int ObjectType { get; set; }
        public bool AllowRead { get; set; }
        public bool AllowWrite { get; set; }
        public bool AllowCreate { get; set; }
        public bool AllowDelete { get; set; }
        public bool AllowNavigate { get; set; }
        public List<AriaMethodPermission> MethodPermission { get; set; }
        public List<AriaPropertyPermission> PropertyPermission { get; set; }
        public string Criteria { get; set; }
    }
}
