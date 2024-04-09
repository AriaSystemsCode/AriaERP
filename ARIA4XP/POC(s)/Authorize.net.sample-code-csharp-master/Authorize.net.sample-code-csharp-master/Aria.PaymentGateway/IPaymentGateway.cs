using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aria.PaymentGateway
{
    public interface IPaymentGateway
    {
        CustomerProfileResponse AddCustomerProfile(ConnectionInfo connection, CustomerProfile profile);
        /*
        ManageCustomerProfileResponse UpdateCustomerProfile(CustomerProfile profile);
        ManageCustomerProfileResponse DeleteCustomerProfile(string profileId);
        CustomerProfile GetCustomerProfile(string profileId);

        ManageCustomerProfileResponse AddCustomerPaymentProfile(CustomerPaymentProfile paymentProfile);
        ManageCustomerProfileResponse UpdateCustomerPaymentProfile(CustomerPaymentProfile paymentProfile);
        ManageCustomerProfileResponse DeleteCustomerPaymentProfile(string customerProfileId, string paymentProfileId);
        CustomerPaymentProfile getCustomerPaymentProfile(string customerProfileId, string paymentProfileId);
        CustomerPaymentProfile getCustomerPaymentProfiles(string customerProfileId);

        ManageCustomerProfileResponse AddCustomerShippingProfile(CustomerShippingProfile shippingProfile);
        ManageCustomerProfileResponse UpdateCustomerShippingProfile(CustomerShippingProfile shippingProfile);
        ManageCustomerProfileResponse DeleteCustomerShippingProfile(string customerProfileId, string shippingProfileId);
        CustomerShippingProfile GetCustomerShippingProfile(string customerProfileId, string shippingProfileId);
        CustomerShippingProfile GetCustomerShippingProfiles(string customerProfileId);

        TransactionResponse AuthorizeCustomerProfile(string CustomerProfileId, string paymentProfileId, string shippingProfileId, decimal amount);
        TransactionResponse AuthorizeCustomerProfile(string CustomerProfileId, string paymentProfileId, string shippingProfileId, OrderInformation order, decimal amount);

        TransactionResponse AuthorizeCreditCard(CreditCardInformation creditCard, CustomerShippingAddress shippingAddress, decimal amount);
        TransactionResponse AuthorizeCreditCard(CreditCardInformation creditCard, CustomerShippingAddress shippingAddress, OrderInformation order, decimal amount);

        TransactionResponse CaptureCustomerProfile(string CustomerProfileId, string paymentProfileId, string shippingProfileId, string transactionId, decimal amount);
        
        TransactionResponse VoidCustomerProfile(string CustomerProfileId, string paymentProfileId, string shippingProfileId, string transactionId, decimal amount);

        TransactionResponse RefundCustomerProfile(string CustomerProfileId, string paymentProfileId, string shippingProfileId, string transactionId, decimal amount);
        */
    }
}
