using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aria.PaymentGateway
{
    public class AriaPaymentGateway
    {
        private IPaymentGateway PaymentGateway { get; set; }

        public AriaPaymentGateway()
        {
            PaymentGateway = new AuthorizeNetAdapter.AuthorizeNetGateway();
        }

        public CustomerProfileResponse AddCustomerProfile(ConnectionInfo connection, CustomerProfile profile)
        {
            return PaymentGateway.AddCustomerProfile(connection, profile);
        }
    }
}
