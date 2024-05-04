using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module
{
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct)]
    public class DivisionAttribute : Attribute
    {
        public bool ContainDivision { set; get; }
        public DivisionAttribute(bool containDivision)
        {
            ContainDivision = containDivision;
        }
    }
}
