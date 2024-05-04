using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module
{
    public class IsClient : Attribute
    {
        public bool ClientTable { get; set; }

        public bool NeedPrefix { get; set; }

        public IsClient()
        {
            ClientTable = true;
            NeedPrefix = false;
        }

        public IsClient(bool clientTable, bool needPrefix)
        {
            ClientTable = clientTable;
            NeedPrefix = needPrefix;       
        }
    }
}
