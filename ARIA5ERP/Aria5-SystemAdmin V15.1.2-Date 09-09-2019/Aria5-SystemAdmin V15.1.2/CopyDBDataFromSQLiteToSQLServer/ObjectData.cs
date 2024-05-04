using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CopyDBDataFromSQLiteToSQLServer
{
    public class ObjectData
    {
        public ObjectData()
        {
            Properties = new List<string>();
            Values = new List<object>();
        }
        public List<string> Properties { set; get; }
        public List<object> Values { set; get; }
    }
}
