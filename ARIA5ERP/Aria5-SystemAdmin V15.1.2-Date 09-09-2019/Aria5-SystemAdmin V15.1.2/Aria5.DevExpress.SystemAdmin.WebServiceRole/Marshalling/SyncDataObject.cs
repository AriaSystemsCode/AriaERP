using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{
    public class SyncDataObject
    {
        public Guid AccountOID { get; set; }
        public Guid EntityOID { get; set; }
        public string TableName { get; set; }
        public string  RecordStatus { get; set; }
        public  string  Version { get; set; }
       public List<string> Properties;
 
     public   List<object> Values;

    }
}