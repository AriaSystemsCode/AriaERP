﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:4.0.30319.18034
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace Aria5SystemAdmin.Web.DataService {
    
    
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ServiceModel.ServiceContractAttribute(ConfigurationName="DataService.IDataService")]
    public interface IDataService {
        
        [System.ServiceModel.OperationContractAttribute(Action="http://tempuri.org/IDataService/GetSydApp", ReplyAction="http://tempuri.org/IDataService/GetSydAppResponse")]
        System.Collections.Generic.KeyValuePair<string, string>[] GetSydApp(string AriaSourcePath);
        
        [System.ServiceModel.OperationContractAttribute(Action="http://tempuri.org/IDataService/GetRoles", ReplyAction="http://tempuri.org/IDataService/GetRolesResponse")]
        System.Collections.Generic.KeyValuePair<string, string>[] GetRoles(Core.Utilites.SQLInfo SystemMasterSqlInfo);
    }
    
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    public interface IDataServiceChannel : Aria5SystemAdmin.Web.DataService.IDataService, System.ServiceModel.IClientChannel {
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    public partial class DataServiceClient : System.ServiceModel.ClientBase<Aria5SystemAdmin.Web.DataService.IDataService>, Aria5SystemAdmin.Web.DataService.IDataService {
        
        public DataServiceClient() {
        }
        
        public DataServiceClient(string endpointConfigurationName) : 
                base(endpointConfigurationName) {
        }
        
        public DataServiceClient(string endpointConfigurationName, string remoteAddress) : 
                base(endpointConfigurationName, remoteAddress) {
        }
        
        public DataServiceClient(string endpointConfigurationName, System.ServiceModel.EndpointAddress remoteAddress) : 
                base(endpointConfigurationName, remoteAddress) {
        }
        
        public DataServiceClient(System.ServiceModel.Channels.Binding binding, System.ServiceModel.EndpointAddress remoteAddress) : 
                base(binding, remoteAddress) {
        }
        
        public System.Collections.Generic.KeyValuePair<string, string>[] GetSydApp(string AriaSourcePath) {
            return base.Channel.GetSydApp(AriaSourcePath);
        }
        
        public System.Collections.Generic.KeyValuePair<string, string>[] GetRoles(Core.Utilites.SQLInfo SystemMasterSqlInfo) {
            return base.Channel.GetRoles(SystemMasterSqlInfo);
        }
    }
}
