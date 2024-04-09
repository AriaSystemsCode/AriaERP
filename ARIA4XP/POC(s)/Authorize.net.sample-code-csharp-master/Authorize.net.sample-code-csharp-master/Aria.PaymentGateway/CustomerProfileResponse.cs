using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aria.PaymentGateway
{
    public class CustomerProfileResponse
    {
        public enum CustomerProfileResponseStatus
        {
            OK,
            Error
        }

        public CustomerProfileResponseStatus Status { get; set; }

        public string[] ResultCode { get; set; }
        public string[] Message { get; set; }
        public string[] Text { get; set; }
        public string[] Description { get; set; }

        public string ProfileId { get; set; }
        public string ProfilePaymentId { get; set; }
        public string ProfileShippingId { get; set; }

        public CustomerProfileResponse()
        {
            Status = CustomerProfileResponseStatus.OK;

            ResultCode = new string[1] { "" };
            Message = new string[1] { "" };
            Text = new string[1] { "" };
            Description = new string[1] { "" };

            ProfileId = "";
            ProfilePaymentId = "";
            ProfileShippingId = "";
        }
    }
}
