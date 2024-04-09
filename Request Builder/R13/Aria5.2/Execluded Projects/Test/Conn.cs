using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Data.SqlClient;
using System.Data;
using Aria.Environment;

namespace Test
{
    public class Conn
    {
        private static string _conn = "";

        public static string Conn1
        {
            get { return _conn; }
            set { _conn = value; }
        }

    }
}
