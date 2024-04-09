using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aria.PaymentGateway
{
    public class CustomerProfile
    {
        public string Id { get; set; }
        public string Email { get; set; }

        public CustomerProfile()
        {
            Id = "";
            Email = "";
        }
    }
}
