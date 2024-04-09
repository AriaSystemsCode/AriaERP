using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Data;
using System.Data.SqlClient;
using Aria.Environment;

namespace Aria.DataAccessLayer
{
    public class AriaConnection
    {
        private static AriaEnviromentVariables Env = null;
        private static string _clientId;
        public string clientId
        {
            get { return _clientId;  }
            set { _clientId = value ;} 
           
        }

        public static string SystemConnectionString
        {
            get
            {
                Env = new AriaEnviromentVariables();
                //T20100512.0026 Hassan 2010 05 23 [Begin]
                Env.ClientID = _clientId;
                Env.ConnectionsRefresh();
                //T20100512.0026 Hassan 2010 05 23 [END]
                return Env.Aria50SystemFilesConnectionString;
            }
        }
    }
}
