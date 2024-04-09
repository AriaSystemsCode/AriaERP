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
    public partial class FormChargeCreditCard : Form
    {
        public FormChargeCreditCard()
        {
            InitializeComponent();
        }

        private void buttonPay_Click(object sender, EventArgs e)
        {
            Console.WriteLine("Charge Credit Card Sample");
            
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

            var billingAddress = new customerAddressType
            {
                firstName = textBoxFirstName.Text,
                lastName = textBoxLastName.Text,
                address = textBoxAddress.Text,
                city = textBoxCity.Text,
                zip = textBoxZip.Text
            };

            //standard api call to retrieve response
            var paymentType = new paymentType { Item = creditCard };

            // Add line Items
            var lineItems = new lineItemType[1];
            lineItems[0] = new lineItemType { itemId = textBoxItemID.Text, name = textBoxName.Text, quantity = Convert.ToInt32(textBoxQuantity.Text), unitPrice = Convert.ToDecimal(textBoxUnitPrice.Text) };

            var transactionRequest = new transactionRequestType
            {
                transactionType = transactionTypeEnum.authCaptureTransaction.ToString(),    // charge the card

                amount = Convert.ToInt32(textBoxQuantity.Text) * Convert.ToDecimal(textBoxUnitPrice.Text),
                payment = paymentType,
                billTo = billingAddress,
                lineItems = lineItems
            };

            var request = new createTransactionRequest { transactionRequest = transactionRequest };

            // instantiate the controller that will call the service
            var controller = new createTransactionController(request);
            controller.Execute();

            // get the response from the service (errors contained if any)
            var response = controller.GetApiResponse();

            // validate response
            if (response != null)
            {
                if (response.messages.resultCode == messageTypeEnum.Ok)
                {
                    if (response.transactionResponse.messages != null)
                    {
                        textBoxTransactionId.Text = response.transactionResponse.transId;

                        Console.WriteLine("Successfully created transaction with Transaction ID: " + response.transactionResponse.transId);
                        Console.WriteLine("Response Code: " + response.transactionResponse.responseCode);
                        Console.WriteLine("Message Code: " + response.transactionResponse.messages[0].code);
                        Console.WriteLine("Description: " + response.transactionResponse.messages[0].description);
                        Console.WriteLine("Success, Auth Code : " + response.transactionResponse.authCode);
                    }
                    else
                    {
                        Console.WriteLine("Failed Transaction.");
                        textBoxTransactionId.Text = "Failed Transaction.";
                        if (response.transactionResponse.errors != null)
                        {
                            Console.WriteLine("Error Code: " + response.transactionResponse.errors[0].errorCode);
                            Console.WriteLine("Error message: " + response.transactionResponse.errors[0].errorText);
                        }
                    }
                }
                else
                {
                    Console.WriteLine("Failed Transaction.");
                    textBoxTransactionId.Text = "Failed Transaction.";
                    if (response.transactionResponse != null && response.transactionResponse.errors != null)
                    {
                        Console.WriteLine("Error Code: " + response.transactionResponse.errors[0].errorCode);
                        Console.WriteLine("Error message: " + response.transactionResponse.errors[0].errorText);
                    }
                    else
                    {
                        Console.WriteLine("Error Code: " + response.messages.message[0].code);
                        Console.WriteLine("Error message: " + response.messages.message[0].text);
                    }
                }
            }
            else
            {
                textBoxTransactionId.Text = "Null Response.";
                Console.WriteLine("Null Response.");
            }
        }
    }
}
