using AuthorizeNet.Api.Contracts.V1;
using AuthorizeNet.Api.Controllers;
using AuthorizeNet.Api.Controllers.Bases;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aria.PaymentGateway.AuthorizeNetAdapter
{
    public class AuthorizeNetGateway : IPaymentGateway
    {
        CustomerProfileResponse IPaymentGateway.AddCustomerProfile(ConnectionInfo connection, CustomerProfile profile)
        {
            // set whether to use the sandbox environment, or production enviornment
            if (connection.Environment == ConnectionInfo.RunEnvironment.SandBox)
            {
                ApiOperationBase<ANetApiRequest, ANetApiResponse>.RunEnvironment = AuthorizeNet.Environment.SANDBOX;
            }
            else
            {
                ApiOperationBase<ANetApiRequest, ANetApiResponse>.RunEnvironment = AuthorizeNet.Environment.PRODUCTION;
            }

            // define the merchant information (authentication / transaction id)
            ApiOperationBase<ANetApiRequest, ANetApiResponse>.MerchantAuthentication = new merchantAuthenticationType()
            {
                name = connection.Items["ApiLoginID"].ToString(),
                ItemElementName = ItemChoiceType.transactionKey,
                Item = connection.Items["ApiTransactionKey"].ToString(),
            };



            customerProfileType customerProfile = new customerProfileType();
            customerProfile.merchantCustomerId = profile.Id;
            customerProfile.email = profile.Email;

            var request = new createCustomerProfileRequest { profile = customerProfile, validationMode = validationModeEnum.none };

            // instantiate the controller that will call the service
            var controller = new createCustomerProfileController(request);
            controller.Execute();

            // get the response from the service (errors contained if any)
            createCustomerProfileResponse response = controller.GetApiResponse();

            CustomerProfileResponse result = new CustomerProfileResponse();

            // validate response 
            if (response != null)
            {
                if (response.messages.resultCode == messageTypeEnum.Ok)
                {
                    if (response.messages.message != null)
                    {
                        result.Status = CustomerProfileResponse.CustomerProfileResponseStatus.OK;

                        result.ProfileId = response.customerProfileId;
                    }
                }
                else
                {
                    result.Status = CustomerProfileResponse.CustomerProfileResponseStatus.Error;

                    result.ResultCode = new string[1] { response.messages.message[0].code };
                    result.Message = new string[1] { response.messages.message[0].text };
                    result.Text = new string[1] { AuthorizeNetResponseCodeMessage.ResourceManager.GetString(response.messages.message[0].code, response.messages.message[0].text) };
                    result.Description = new string[1] { AuthorizeNetResponseCodeDescription.ResourceManager.GetString(response.messages.message[0].code, "") };
                }
            }
            else
            {
                if (controller.GetErrorResponse().messages.message.Length > 0)
                {
                    result.Status = CustomerProfileResponse.CustomerProfileResponseStatus.Error;

                    result.ResultCode = new string[1] { response.messages.message[0].code };
                    result.Message = new string[1] { response.messages.message[0].text };
                    result.Text = new string[1] { AuthorizeNetResponseCodeMessage.ResourceManager.GetString(response.messages.message[0].code, response.messages.message[0].text) };
                    result.Description = new string[1] { AuthorizeNetResponseCodeDescription.ResourceManager.GetString(response.messages.message[0].code, "") };
                }
                else
                {
                    result.Status = CustomerProfileResponse.CustomerProfileResponseStatus.NoResponse;
                }
            }

            return result;
        }
    }
}
