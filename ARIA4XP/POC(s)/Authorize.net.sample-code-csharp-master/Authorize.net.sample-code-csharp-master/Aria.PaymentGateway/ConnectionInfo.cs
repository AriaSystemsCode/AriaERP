using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aria.PaymentGateway
{
    public class ConnectionInfo
    {
        public enum RunEnvironment
        { 
            SandBox, 
            Production
        }

        public Dictionary<string, string> Items { get; set; }

        public RunEnvironment Environment { get; set; }

        public ConnectionInfo()
        {
            Items = new Dictionary<string, string>();
            Environment = RunEnvironment.SandBox;
        }
    }
}
