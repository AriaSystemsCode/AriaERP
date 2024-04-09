using System;
using Aria.DataTypes.RequestHandler;

namespace Aria.Data
{
    /// <summary>
    /// Contain some property help in generate connection string (not responsible of generated any connection string)
    /// </summary>
    [Serializable]
    public class AriaDbConnection : MarshalByRefObject
    {
        private string _customerName = "";
        public string CustomerName
        {
            get { return _customerName; }
            set { _customerName = value; }
        }

        private string _companyName = "";
        public string CompanyName
        {
            get { return _companyName; }
            set { _companyName = value; }
        }

        private AriaRequestContext _context = null;
        public AriaRequestContext Context
        {
            get { return _context; }
            set { _context = value; }
        }
        
        public AriaDbConnection(string customerName, string companyName)
        {
            _customerName = customerName;
            _companyName = companyName;
        }

        public AriaDbConnection()
        {
        }
    }
}
