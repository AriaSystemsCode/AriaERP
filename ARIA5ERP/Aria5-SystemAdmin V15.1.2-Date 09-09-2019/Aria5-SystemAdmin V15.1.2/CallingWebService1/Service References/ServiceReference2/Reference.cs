﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:4.0.30319.34014
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace CallingWebService1.ServiceReference2 {
    
    
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ServiceModel.ServiceContractAttribute(ConfigurationName="ServiceReference2.AccountWebServiceSoap")]
    public interface AccountWebServiceSoap {
        
        // CODEGEN: Generating message contract since element name accountId from namespace http://tempuri.org/ is not marked nillable
        [System.ServiceModel.OperationContractAttribute(Action="http://tempuri.org/GetAccountStatus", ReplyAction="*")]
        CallingWebService1.ServiceReference2.GetAccountStatusResponse GetAccountStatus(CallingWebService1.ServiceReference2.GetAccountStatusRequest request);
        
        [System.ServiceModel.OperationContractAttribute(Action="http://tempuri.org/GetAccountStatus", ReplyAction="*")]
        System.Threading.Tasks.Task<CallingWebService1.ServiceReference2.GetAccountStatusResponse> GetAccountStatusAsync(CallingWebService1.ServiceReference2.GetAccountStatusRequest request);
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class GetAccountStatusRequest {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="GetAccountStatus", Namespace="http://tempuri.org/", Order=0)]
        public CallingWebService1.ServiceReference2.GetAccountStatusRequestBody Body;
        
        public GetAccountStatusRequest() {
        }
        
        public GetAccountStatusRequest(CallingWebService1.ServiceReference2.GetAccountStatusRequestBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class GetAccountStatusRequestBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=0)]
        public string accountId;
        
        public GetAccountStatusRequestBody() {
        }
        
        public GetAccountStatusRequestBody(string accountId) {
            this.accountId = accountId;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class GetAccountStatusResponse {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="GetAccountStatusResponse", Namespace="http://tempuri.org/", Order=0)]
        public CallingWebService1.ServiceReference2.GetAccountStatusResponseBody Body;
        
        public GetAccountStatusResponse() {
        }
        
        public GetAccountStatusResponse(CallingWebService1.ServiceReference2.GetAccountStatusResponseBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class GetAccountStatusResponseBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=0)]
        public string GetAccountStatusResult;
        
        public GetAccountStatusResponseBody() {
        }
        
        public GetAccountStatusResponseBody(string GetAccountStatusResult) {
            this.GetAccountStatusResult = GetAccountStatusResult;
        }
    }
    
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    public interface AccountWebServiceSoapChannel : CallingWebService1.ServiceReference2.AccountWebServiceSoap, System.ServiceModel.IClientChannel {
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    public partial class AccountWebServiceSoapClient : System.ServiceModel.ClientBase<CallingWebService1.ServiceReference2.AccountWebServiceSoap>, CallingWebService1.ServiceReference2.AccountWebServiceSoap {
        
        public AccountWebServiceSoapClient() {
        }
        
        public AccountWebServiceSoapClient(string endpointConfigurationName) : 
                base(endpointConfigurationName) {
        }
        
        public AccountWebServiceSoapClient(string endpointConfigurationName, string remoteAddress) : 
                base(endpointConfigurationName, remoteAddress) {
        }
        
        public AccountWebServiceSoapClient(string endpointConfigurationName, System.ServiceModel.EndpointAddress remoteAddress) : 
                base(endpointConfigurationName, remoteAddress) {
        }
        
        public AccountWebServiceSoapClient(System.ServiceModel.Channels.Binding binding, System.ServiceModel.EndpointAddress remoteAddress) : 
                base(binding, remoteAddress) {
        }
        
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        CallingWebService1.ServiceReference2.GetAccountStatusResponse CallingWebService1.ServiceReference2.AccountWebServiceSoap.GetAccountStatus(CallingWebService1.ServiceReference2.GetAccountStatusRequest request) {
            return base.Channel.GetAccountStatus(request);
        }
        
        public string GetAccountStatus(string accountId) {
            CallingWebService1.ServiceReference2.GetAccountStatusRequest inValue = new CallingWebService1.ServiceReference2.GetAccountStatusRequest();
            inValue.Body = new CallingWebService1.ServiceReference2.GetAccountStatusRequestBody();
            inValue.Body.accountId = accountId;
            CallingWebService1.ServiceReference2.GetAccountStatusResponse retVal = ((CallingWebService1.ServiceReference2.AccountWebServiceSoap)(this)).GetAccountStatus(inValue);
            return retVal.Body.GetAccountStatusResult;
        }
        
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        System.Threading.Tasks.Task<CallingWebService1.ServiceReference2.GetAccountStatusResponse> CallingWebService1.ServiceReference2.AccountWebServiceSoap.GetAccountStatusAsync(CallingWebService1.ServiceReference2.GetAccountStatusRequest request) {
            return base.Channel.GetAccountStatusAsync(request);
        }
        
        public System.Threading.Tasks.Task<CallingWebService1.ServiceReference2.GetAccountStatusResponse> GetAccountStatusAsync(string accountId) {
            CallingWebService1.ServiceReference2.GetAccountStatusRequest inValue = new CallingWebService1.ServiceReference2.GetAccountStatusRequest();
            inValue.Body = new CallingWebService1.ServiceReference2.GetAccountStatusRequestBody();
            inValue.Body.accountId = accountId;
            return ((CallingWebService1.ServiceReference2.AccountWebServiceSoap)(this)).GetAccountStatusAsync(inValue);
        }
    }
}