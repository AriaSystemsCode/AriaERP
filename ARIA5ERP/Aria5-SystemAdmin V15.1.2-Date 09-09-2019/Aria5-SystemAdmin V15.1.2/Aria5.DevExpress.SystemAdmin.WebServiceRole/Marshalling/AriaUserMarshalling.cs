using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling
{
    [Serializable]
    public class AriaUserMarshalling
    {

        private Guid account;
        public Guid Account
        {
            get { return account; }
            set { account = value; }
        }

        private string userName;
        public string UserName
        {
            get { return userName; }
            set { userName = value; }
        }

        private string password;
        public string Password
        {
            get { return password; }
            set { password = value; }
        }
        // Sara.N 06/01/2015 Add WebMethod Return Add webmethod returns All Active Users [Start]
        private string emailAddress;
        public string EmailAddress
        {
            get { return emailAddress; }
            set { emailAddress = value; }
        }
        // Sara.N 06/01/2015 Add WebMethod Return Add webmethod returns All Active Users [End]
    }

}