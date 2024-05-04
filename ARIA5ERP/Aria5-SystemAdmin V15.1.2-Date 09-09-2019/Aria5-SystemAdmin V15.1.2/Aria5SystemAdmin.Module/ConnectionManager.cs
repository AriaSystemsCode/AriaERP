using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aria.WindowsStore.DataAccessLayer.Framework
{
    public static class ConnectionManager
    {
        public static SQLite.SQLiteConnection Connection { get; set; }

        public static void SetConnection(string connection)
        {
            Connection = new SQLite.SQLiteConnection(connection);
        }
    }
}
