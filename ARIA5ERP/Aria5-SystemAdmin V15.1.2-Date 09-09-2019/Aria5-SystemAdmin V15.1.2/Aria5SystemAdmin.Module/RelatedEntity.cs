using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module
{
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct)]
    public class RelatedEntity : Attribute
    {
        public string EntityName  {set; get;} 
        public RelatedEntity(string entityName)
        {
            EntityName = entityName;
        }
    }
}
