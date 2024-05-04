using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CopyDBDataFromSQLiteToSQLServer
{
    public class EqualityComparer :IEqualityComparer<string>
    {
        public bool Equals(string x, string y)
        {
            if (x.ToUpper().Trim() == y.ToUpper().Trim())
            {
                return true;
            }
            else { return false; }
        }
        public int GetHashCode(string codeh)
        {
            return 0;
        }
    }
}
