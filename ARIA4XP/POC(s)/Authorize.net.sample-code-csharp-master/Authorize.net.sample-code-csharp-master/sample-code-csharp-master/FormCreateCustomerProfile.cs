using AuthorizeNet.Api.Contracts.V1;
using AuthorizeNet.Api.Controllers;
using AuthorizeNet.Api.Controllers.Bases;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace net.authorize.sample
{
    public partial class FormCreateCustomerProfile : Form
    {
        public FormCreateCustomerProfile()
        {
            InitializeComponent();
        }

        private void buttonCreateCustomerProfile_Click(object sender, EventArgs e)
        {
            Console.WriteLine("Create Customer Profile Sample");

            // set whether to use the sandbox environment, or production enviornment
            ApiOperationBase<ANetApiRequest, ANetApiResponse>.RunEnvironment = AuthorizeNet.Environment.SANDBOX;

            const string apiLoginId = "8r6G5dwXUjd";
            const string transactionKey = "63X7NwEdhR8649WX";

            // define the merchant information (authentication / transaction id)
            ApiOperationBase<ANetApiRequest, ANetApiResponse>.MerchantAuthentication = new merchantAuthenticationType()
            {
                name = apiLoginId,
                ItemElementName = ItemChoiceType.transactionKey,
                Item = transactionKey,
            };

            var creditCard = new creditCardType
            {
                cardNumber = textBoxCardNumber.Text,
                expirationDate = textBoxExpirationDate.Text,
                cardCode = textBoxCardCode.Text
            };

            // standard api call to retrieve response
            paymentType cc = new paymentType { Item = creditCard };

            List<customerPaymentProfileType> paymentProfileList = new List<customerPaymentProfileType>();
            customerPaymentProfileType ccPaymentProfile = new customerPaymentProfileType();
            ccPaymentProfile.payment = cc;


            paymentProfileList.Add(ccPaymentProfile);

            List<customerAddressType> addressInfoList = new List<customerAddressType>();


            customerAddressType officeAddress = new customerAddressType();
            officeAddress.address = textBoxAddress.Text;
            officeAddress.city = textBoxCity.Text;
            officeAddress.zip = textBoxZip.Text;

            addressInfoList.Add(officeAddress);


            customerProfileType customerProfile = new customerProfileType();
            customerProfile.merchantCustomerId = textBoxID.Text;
            customerProfile.email = textBoxEmail.Text;
            customerProfile.paymentProfiles = paymentProfileList.ToArray();
            customerProfile.shipToList = addressInfoList.ToArray();

            var request = new createCustomerProfileRequest { profile = customerProfile, validationMode = validationModeEnum.none };

            // instantiate the controller that will call the service
            var controller = new createCustomerProfileController(request);
            controller.Execute();

            // get the response from the service (errors contained if any)
            createCustomerProfileResponse response = controller.GetApiResponse();

            // validate response 
            if (response != null)
            {
                if (response.messages.resultCode == messageTypeEnum.Ok)
                {
                    if (response.messages.message != null)
                    {
                        Console.WriteLine("Success!");

                        textBoxProfileId.Text = response.customerProfileId;
                        textBoxProfilePaymentId.Text = response.customerPaymentProfileIdList[0];
                        textBoxProfileShippingId.Text = response.customerShippingAddressIdList[0];

                        Console.WriteLine("Customer Profile ID: " + response.customerProfileId);
                        Console.WriteLine("Payment Profile ID: " + response.customerPaymentProfileIdList[0]);
                        Console.WriteLine("Shipping Profile ID: " + response.customerShippingAddressIdList[0]);
                    }
                }
                else
                {
                    Console.WriteLine("Customer Profile Creation Failed.");
                    Console.WriteLine("Error Code: " + response.messages.message[0].code);
                    Console.WriteLine("Error message: " + response.messages.message[0].text);
                }
            }
            else
            {
                if (controller.GetErrorResponse().messages.message.Length > 0)
                {
                    Console.WriteLine("Customer Profile Creation Failed.");
                    Console.WriteLine("Error Code: " + response.messages.message[0].code);
                    Console.WriteLine("Error message: " + response.messages.message[0].text);
                }
                else
                {
                    Console.WriteLine("Null Response.");
                }
            }
        }
    }
}
