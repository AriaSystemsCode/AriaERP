﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:4.0.30319.34014
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace WindowsFormsApplication1.ServiceReference2 {
    using System.Runtime.Serialization;
    using System;
    
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Runtime.Serialization", "4.0.0.0")]
    [System.Runtime.Serialization.DataContractAttribute(Name="SyncDataObject", Namespace="http://tempuri.org/")]
    [System.SerializableAttribute()]
    [System.Runtime.Serialization.KnownTypeAttribute(typeof(WindowsFormsApplication1.ServiceReference2.SyncDataObject[]))]
    [System.Runtime.Serialization.KnownTypeAttribute(typeof(WindowsFormsApplication1.ServiceReference2.ArrayOfString))]
    [System.Runtime.Serialization.KnownTypeAttribute(typeof(WindowsFormsApplication1.ServiceReference2.ArrayOfAnyType))]
    public partial class SyncDataObject : object, System.Runtime.Serialization.IExtensibleDataObject, System.ComponentModel.INotifyPropertyChanged {
        
        [System.NonSerializedAttribute()]
        private System.Runtime.Serialization.ExtensionDataObject extensionDataField;
        
        [System.Runtime.Serialization.OptionalFieldAttribute()]
        private WindowsFormsApplication1.ServiceReference2.ArrayOfString PropertiesField;
        
        [System.Runtime.Serialization.OptionalFieldAttribute()]
        private WindowsFormsApplication1.ServiceReference2.ArrayOfAnyType ValuesField;
        
        private System.Guid AccountOIDField;
        
        private System.Guid EntityOIDField;
        
        [System.Runtime.Serialization.OptionalFieldAttribute()]
        private string TableNameField;
        
        [System.Runtime.Serialization.OptionalFieldAttribute()]
        private string RecordStatusField;
        
        [System.Runtime.Serialization.OptionalFieldAttribute()]
        private string VersionField;
        
        [global::System.ComponentModel.BrowsableAttribute(false)]
        public System.Runtime.Serialization.ExtensionDataObject ExtensionData {
            get {
                return this.extensionDataField;
            }
            set {
                this.extensionDataField = value;
            }
        }
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false)]
        public WindowsFormsApplication1.ServiceReference2.ArrayOfString Properties {
            get {
                return this.PropertiesField;
            }
            set {
                if ((object.ReferenceEquals(this.PropertiesField, value) != true)) {
                    this.PropertiesField = value;
                    this.RaisePropertyChanged("Properties");
                }
            }
        }
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false)]
        public WindowsFormsApplication1.ServiceReference2.ArrayOfAnyType Values {
            get {
                return this.ValuesField;
            }
            set {
                if ((object.ReferenceEquals(this.ValuesField, value) != true)) {
                    this.ValuesField = value;
                    this.RaisePropertyChanged("Values");
                }
            }
        }
        
        [System.Runtime.Serialization.DataMemberAttribute(IsRequired=true, Order=2)]
        public System.Guid AccountOID {
            get {
                return this.AccountOIDField;
            }
            set {
                if ((this.AccountOIDField.Equals(value) != true)) {
                    this.AccountOIDField = value;
                    this.RaisePropertyChanged("AccountOID");
                }
            }
        }
        
        [System.Runtime.Serialization.DataMemberAttribute(IsRequired=true, Order=3)]
        public System.Guid EntityOID {
            get {
                return this.EntityOIDField;
            }
            set {
                if ((this.EntityOIDField.Equals(value) != true)) {
                    this.EntityOIDField = value;
                    this.RaisePropertyChanged("EntityOID");
                }
            }
        }
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=4)]
        public string TableName {
            get {
                return this.TableNameField;
            }
            set {
                if ((object.ReferenceEquals(this.TableNameField, value) != true)) {
                    this.TableNameField = value;
                    this.RaisePropertyChanged("TableName");
                }
            }
        }
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=5)]
        public string RecordStatus {
            get {
                return this.RecordStatusField;
            }
            set {
                if ((object.ReferenceEquals(this.RecordStatusField, value) != true)) {
                    this.RecordStatusField = value;
                    this.RaisePropertyChanged("RecordStatus");
                }
            }
        }
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=6)]
        public string Version {
            get {
                return this.VersionField;
            }
            set {
                if ((object.ReferenceEquals(this.VersionField, value) != true)) {
                    this.VersionField = value;
                    this.RaisePropertyChanged("Version");
                }
            }
        }
        
        public event System.ComponentModel.PropertyChangedEventHandler PropertyChanged;
        
        protected void RaisePropertyChanged(string propertyName) {
            System.ComponentModel.PropertyChangedEventHandler propertyChanged = this.PropertyChanged;
            if ((propertyChanged != null)) {
                propertyChanged(this, new System.ComponentModel.PropertyChangedEventArgs(propertyName));
            }
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Runtime.Serialization", "4.0.0.0")]
    [System.Runtime.Serialization.CollectionDataContractAttribute(Name="ArrayOfString", Namespace="http://tempuri.org/", ItemName="string")]
    [System.SerializableAttribute()]
    public class ArrayOfString : System.Collections.Generic.List<string> {
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Runtime.Serialization", "4.0.0.0")]
    [System.Runtime.Serialization.CollectionDataContractAttribute(Name="ArrayOfAnyType", Namespace="http://tempuri.org/", ItemName="anyType")]
    [System.SerializableAttribute()]
    public class ArrayOfAnyType : System.Collections.Generic.List<object> {
    }
    
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ServiceModel.ServiceContractAttribute(ConfigurationName="ServiceReference2.AriaSynchronizationServiceSoap")]
    public interface AriaSynchronizationServiceSoap {
        
        // CODEGEN: Generating message contract since element name fromVersion from namespace http://tempuri.org/ is not marked nillable
        [System.ServiceModel.OperationContractAttribute(Action="http://tempuri.org/PullData", ReplyAction="*")]
        WindowsFormsApplication1.ServiceReference2.PullDataResponse PullData(WindowsFormsApplication1.ServiceReference2.PullDataRequest request);
        
        // CODEGEN: Generating message contract since element name dataToUpdate from namespace http://tempuri.org/ is not marked nillable
        [System.ServiceModel.OperationContractAttribute(Action="http://tempuri.org/PushData", ReplyAction="*")]
        WindowsFormsApplication1.ServiceReference2.PushDataResponse PushData(WindowsFormsApplication1.ServiceReference2.PushDataRequest request);
        
        // CODEGEN: Generating message contract since element name GetDBVersionResult from namespace http://tempuri.org/ is not marked nillable
        [System.ServiceModel.OperationContractAttribute(Action="http://tempuri.org/GetDBVersion", ReplyAction="*")]
        WindowsFormsApplication1.ServiceReference2.GetDBVersionResponse GetDBVersion(WindowsFormsApplication1.ServiceReference2.GetDBVersionRequest request);
        
        // CODEGEN: Generating message contract since element name deviceSeginture from namespace http://tempuri.org/ is not marked nillable
        [System.ServiceModel.OperationContractAttribute(Action="http://tempuri.org/Register", ReplyAction="*")]
        WindowsFormsApplication1.ServiceReference2.RegisterResponse Register(WindowsFormsApplication1.ServiceReference2.RegisterRequest request);
        
        // CODEGEN: Generating message contract since element name txt from namespace http://tempuri.org/ is not marked nillable
        [System.ServiceModel.OperationContractAttribute(Action="http://tempuri.org/Notify", ReplyAction="*")]
        WindowsFormsApplication1.ServiceReference2.NotifyResponse Notify(WindowsFormsApplication1.ServiceReference2.NotifyRequest request);
        
        // CODEGEN: Generating message contract since element name GetMinFrontEndVersionResult from namespace http://tempuri.org/ is not marked nillable
        [System.ServiceModel.OperationContractAttribute(Action="http://tempuri.org/GetMinFrontEndVersion", ReplyAction="*")]
        WindowsFormsApplication1.ServiceReference2.GetMinFrontEndVersionResponse GetMinFrontEndVersion(WindowsFormsApplication1.ServiceReference2.GetMinFrontEndVersionRequest request);
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class PullDataRequest {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="PullData", Namespace="http://tempuri.org/", Order=0)]
        public WindowsFormsApplication1.ServiceReference2.PullDataRequestBody Body;
        
        public PullDataRequest() {
        }
        
        public PullDataRequest(WindowsFormsApplication1.ServiceReference2.PullDataRequestBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class PullDataRequestBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=0)]
        public string fromVersion;
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=1)]
        public string toVersion;
        
        [System.Runtime.Serialization.DataMemberAttribute(Order=2)]
        public System.Guid accountOid;
        
        public PullDataRequestBody() {
        }
        
        public PullDataRequestBody(string fromVersion, string toVersion, System.Guid accountOid) {
            this.fromVersion = fromVersion;
            this.toVersion = toVersion;
            this.accountOid = accountOid;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class PullDataResponse {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="PullDataResponse", Namespace="http://tempuri.org/", Order=0)]
        public WindowsFormsApplication1.ServiceReference2.PullDataResponseBody Body;
        
        public PullDataResponse() {
        }
        
        public PullDataResponse(WindowsFormsApplication1.ServiceReference2.PullDataResponseBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class PullDataResponseBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=0)]
        public WindowsFormsApplication1.ServiceReference2.SyncDataObject[] PullDataResult;
        
        public PullDataResponseBody() {
        }
        
        public PullDataResponseBody(WindowsFormsApplication1.ServiceReference2.SyncDataObject[] PullDataResult) {
            this.PullDataResult = PullDataResult;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class PushDataRequest {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="PushData", Namespace="http://tempuri.org/", Order=0)]
        public WindowsFormsApplication1.ServiceReference2.PushDataRequestBody Body;
        
        public PushDataRequest() {
        }
        
        public PushDataRequest(WindowsFormsApplication1.ServiceReference2.PushDataRequestBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class PushDataRequestBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(Order=0)]
        public System.Guid accountOid;
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=1)]
        public WindowsFormsApplication1.ServiceReference2.SyncDataObject[] dataToUpdate;
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=2)]
        public string DeviceSignature;
        
        public PushDataRequestBody() {
        }
        
        public PushDataRequestBody(System.Guid accountOid, WindowsFormsApplication1.ServiceReference2.SyncDataObject[] dataToUpdate, string DeviceSignature) {
            this.accountOid = accountOid;
            this.dataToUpdate = dataToUpdate;
            this.DeviceSignature = DeviceSignature;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class PushDataResponse {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="PushDataResponse", Namespace="http://tempuri.org/", Order=0)]
        public WindowsFormsApplication1.ServiceReference2.PushDataResponseBody Body;
        
        public PushDataResponse() {
        }
        
        public PushDataResponse(WindowsFormsApplication1.ServiceReference2.PushDataResponseBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class PushDataResponseBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=0)]
        public WindowsFormsApplication1.ServiceReference2.ArrayOfString PushDataResult;
        
        public PushDataResponseBody() {
        }
        
        public PushDataResponseBody(WindowsFormsApplication1.ServiceReference2.ArrayOfString PushDataResult) {
            this.PushDataResult = PushDataResult;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class GetDBVersionRequest {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="GetDBVersion", Namespace="http://tempuri.org/", Order=0)]
        public WindowsFormsApplication1.ServiceReference2.GetDBVersionRequestBody Body;
        
        public GetDBVersionRequest() {
        }
        
        public GetDBVersionRequest(WindowsFormsApplication1.ServiceReference2.GetDBVersionRequestBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class GetDBVersionRequestBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(Order=0)]
        public System.Guid accountOid;
        
        public GetDBVersionRequestBody() {
        }
        
        public GetDBVersionRequestBody(System.Guid accountOid) {
            this.accountOid = accountOid;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class GetDBVersionResponse {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="GetDBVersionResponse", Namespace="http://tempuri.org/", Order=0)]
        public WindowsFormsApplication1.ServiceReference2.GetDBVersionResponseBody Body;
        
        public GetDBVersionResponse() {
        }
        
        public GetDBVersionResponse(WindowsFormsApplication1.ServiceReference2.GetDBVersionResponseBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class GetDBVersionResponseBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=0)]
        public string GetDBVersionResult;
        
        public GetDBVersionResponseBody() {
        }
        
        public GetDBVersionResponseBody(string GetDBVersionResult) {
            this.GetDBVersionResult = GetDBVersionResult;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class RegisterRequest {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="Register", Namespace="http://tempuri.org/", Order=0)]
        public WindowsFormsApplication1.ServiceReference2.RegisterRequestBody Body;
        
        public RegisterRequest() {
        }
        
        public RegisterRequest(WindowsFormsApplication1.ServiceReference2.RegisterRequestBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class RegisterRequestBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(Order=0)]
        public System.Guid account;
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=1)]
        public string deviceSeginture;
        
        public RegisterRequestBody() {
        }
        
        public RegisterRequestBody(System.Guid account, string deviceSeginture) {
            this.account = account;
            this.deviceSeginture = deviceSeginture;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class RegisterResponse {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="RegisterResponse", Namespace="http://tempuri.org/", Order=0)]
        public WindowsFormsApplication1.ServiceReference2.RegisterResponseBody Body;
        
        public RegisterResponse() {
        }
        
        public RegisterResponse(WindowsFormsApplication1.ServiceReference2.RegisterResponseBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class RegisterResponseBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(Order=0)]
        public bool RegisterResult;
        
        public RegisterResponseBody() {
        }
        
        public RegisterResponseBody(bool RegisterResult) {
            this.RegisterResult = RegisterResult;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class NotifyRequest {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="Notify", Namespace="http://tempuri.org/", Order=0)]
        public WindowsFormsApplication1.ServiceReference2.NotifyRequestBody Body;
        
        public NotifyRequest() {
        }
        
        public NotifyRequest(WindowsFormsApplication1.ServiceReference2.NotifyRequestBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class NotifyRequestBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=0)]
        public string txt;
        
        [System.Runtime.Serialization.DataMemberAttribute(Order=1)]
        public System.Guid accountOid;
        
        public NotifyRequestBody() {
        }
        
        public NotifyRequestBody(string txt, System.Guid accountOid) {
            this.txt = txt;
            this.accountOid = accountOid;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class NotifyResponse {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="NotifyResponse", Namespace="http://tempuri.org/", Order=0)]
        public WindowsFormsApplication1.ServiceReference2.NotifyResponseBody Body;
        
        public NotifyResponse() {
        }
        
        public NotifyResponse(WindowsFormsApplication1.ServiceReference2.NotifyResponseBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class NotifyResponseBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(Order=0)]
        public bool NotifyResult;
        
        public NotifyResponseBody() {
        }
        
        public NotifyResponseBody(bool NotifyResult) {
            this.NotifyResult = NotifyResult;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class GetMinFrontEndVersionRequest {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="GetMinFrontEndVersion", Namespace="http://tempuri.org/", Order=0)]
        public WindowsFormsApplication1.ServiceReference2.GetMinFrontEndVersionRequestBody Body;
        
        public GetMinFrontEndVersionRequest() {
        }
        
        public GetMinFrontEndVersionRequest(WindowsFormsApplication1.ServiceReference2.GetMinFrontEndVersionRequestBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute()]
    public partial class GetMinFrontEndVersionRequestBody {
        
        public GetMinFrontEndVersionRequestBody() {
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.ServiceModel.MessageContractAttribute(IsWrapped=false)]
    public partial class GetMinFrontEndVersionResponse {
        
        [System.ServiceModel.MessageBodyMemberAttribute(Name="GetMinFrontEndVersionResponse", Namespace="http://tempuri.org/", Order=0)]
        public WindowsFormsApplication1.ServiceReference2.GetMinFrontEndVersionResponseBody Body;
        
        public GetMinFrontEndVersionResponse() {
        }
        
        public GetMinFrontEndVersionResponse(WindowsFormsApplication1.ServiceReference2.GetMinFrontEndVersionResponseBody Body) {
            this.Body = Body;
        }
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
    [System.Runtime.Serialization.DataContractAttribute(Namespace="http://tempuri.org/")]
    public partial class GetMinFrontEndVersionResponseBody {
        
        [System.Runtime.Serialization.DataMemberAttribute(EmitDefaultValue=false, Order=0)]
        public string GetMinFrontEndVersionResult;
        
        public GetMinFrontEndVersionResponseBody() {
        }
        
        public GetMinFrontEndVersionResponseBody(string GetMinFrontEndVersionResult) {
            this.GetMinFrontEndVersionResult = GetMinFrontEndVersionResult;
        }
    }
    
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    public interface AriaSynchronizationServiceSoapChannel : WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap, System.ServiceModel.IClientChannel {
    }
    
    [System.Diagnostics.DebuggerStepThroughAttribute()]
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.ServiceModel", "4.0.0.0")]
    public partial class AriaSynchronizationServiceSoapClient : System.ServiceModel.ClientBase<WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap>, WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap {
        
        public AriaSynchronizationServiceSoapClient() {
        }
        
        public AriaSynchronizationServiceSoapClient(string endpointConfigurationName) : 
                base(endpointConfigurationName) {
        }
        
        public AriaSynchronizationServiceSoapClient(string endpointConfigurationName, string remoteAddress) : 
                base(endpointConfigurationName, remoteAddress) {
        }
        
        public AriaSynchronizationServiceSoapClient(string endpointConfigurationName, System.ServiceModel.EndpointAddress remoteAddress) : 
                base(endpointConfigurationName, remoteAddress) {
        }
        
        public AriaSynchronizationServiceSoapClient(System.ServiceModel.Channels.Binding binding, System.ServiceModel.EndpointAddress remoteAddress) : 
                base(binding, remoteAddress) {
        }
        
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        WindowsFormsApplication1.ServiceReference2.PullDataResponse WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap.PullData(WindowsFormsApplication1.ServiceReference2.PullDataRequest request) {
            return base.Channel.PullData(request);
        }
        
        public WindowsFormsApplication1.ServiceReference2.SyncDataObject[] PullData(string fromVersion, string toVersion, System.Guid accountOid) {
            WindowsFormsApplication1.ServiceReference2.PullDataRequest inValue = new WindowsFormsApplication1.ServiceReference2.PullDataRequest();
            inValue.Body = new WindowsFormsApplication1.ServiceReference2.PullDataRequestBody();
            inValue.Body.fromVersion = fromVersion;
            inValue.Body.toVersion = toVersion;
            inValue.Body.accountOid = accountOid;
            WindowsFormsApplication1.ServiceReference2.PullDataResponse retVal = ((WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap)(this)).PullData(inValue);
            return retVal.Body.PullDataResult;
        }
        
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        WindowsFormsApplication1.ServiceReference2.PushDataResponse WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap.PushData(WindowsFormsApplication1.ServiceReference2.PushDataRequest request) {
            return base.Channel.PushData(request);
        }
        
        public WindowsFormsApplication1.ServiceReference2.ArrayOfString PushData(System.Guid accountOid, WindowsFormsApplication1.ServiceReference2.SyncDataObject[] dataToUpdate, string DeviceSignature) {
            WindowsFormsApplication1.ServiceReference2.PushDataRequest inValue = new WindowsFormsApplication1.ServiceReference2.PushDataRequest();
            inValue.Body = new WindowsFormsApplication1.ServiceReference2.PushDataRequestBody();
            inValue.Body.accountOid = accountOid;
            inValue.Body.dataToUpdate = dataToUpdate;
            inValue.Body.DeviceSignature = DeviceSignature;
            WindowsFormsApplication1.ServiceReference2.PushDataResponse retVal = ((WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap)(this)).PushData(inValue);
            return retVal.Body.PushDataResult;
        }
        
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        WindowsFormsApplication1.ServiceReference2.GetDBVersionResponse WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap.GetDBVersion(WindowsFormsApplication1.ServiceReference2.GetDBVersionRequest request) {
            return base.Channel.GetDBVersion(request);
        }
        
        public string GetDBVersion(System.Guid accountOid) {
            WindowsFormsApplication1.ServiceReference2.GetDBVersionRequest inValue = new WindowsFormsApplication1.ServiceReference2.GetDBVersionRequest();
            inValue.Body = new WindowsFormsApplication1.ServiceReference2.GetDBVersionRequestBody();
            inValue.Body.accountOid = accountOid;
            WindowsFormsApplication1.ServiceReference2.GetDBVersionResponse retVal = ((WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap)(this)).GetDBVersion(inValue);
            return retVal.Body.GetDBVersionResult;
        }
        
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        WindowsFormsApplication1.ServiceReference2.RegisterResponse WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap.Register(WindowsFormsApplication1.ServiceReference2.RegisterRequest request) {
            return base.Channel.Register(request);
        }
        
        public bool Register(System.Guid account, string deviceSeginture) {
            WindowsFormsApplication1.ServiceReference2.RegisterRequest inValue = new WindowsFormsApplication1.ServiceReference2.RegisterRequest();
            inValue.Body = new WindowsFormsApplication1.ServiceReference2.RegisterRequestBody();
            inValue.Body.account = account;
            inValue.Body.deviceSeginture = deviceSeginture;
            WindowsFormsApplication1.ServiceReference2.RegisterResponse retVal = ((WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap)(this)).Register(inValue);
            return retVal.Body.RegisterResult;
        }
        
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        WindowsFormsApplication1.ServiceReference2.NotifyResponse WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap.Notify(WindowsFormsApplication1.ServiceReference2.NotifyRequest request) {
            return base.Channel.Notify(request);
        }
        
        public bool Notify(string txt, System.Guid accountOid) {
            WindowsFormsApplication1.ServiceReference2.NotifyRequest inValue = new WindowsFormsApplication1.ServiceReference2.NotifyRequest();
            inValue.Body = new WindowsFormsApplication1.ServiceReference2.NotifyRequestBody();
            inValue.Body.txt = txt;
            inValue.Body.accountOid = accountOid;
            WindowsFormsApplication1.ServiceReference2.NotifyResponse retVal = ((WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap)(this)).Notify(inValue);
            return retVal.Body.NotifyResult;
        }
        
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        WindowsFormsApplication1.ServiceReference2.GetMinFrontEndVersionResponse WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap.GetMinFrontEndVersion(WindowsFormsApplication1.ServiceReference2.GetMinFrontEndVersionRequest request) {
            return base.Channel.GetMinFrontEndVersion(request);
        }
        
        public string GetMinFrontEndVersion() {
            WindowsFormsApplication1.ServiceReference2.GetMinFrontEndVersionRequest inValue = new WindowsFormsApplication1.ServiceReference2.GetMinFrontEndVersionRequest();
            inValue.Body = new WindowsFormsApplication1.ServiceReference2.GetMinFrontEndVersionRequestBody();
            WindowsFormsApplication1.ServiceReference2.GetMinFrontEndVersionResponse retVal = ((WindowsFormsApplication1.ServiceReference2.AriaSynchronizationServiceSoap)(this)).GetMinFrontEndVersion(inValue);
            return retVal.Body.GetMinFrontEndVersionResult;
        }
    }
}