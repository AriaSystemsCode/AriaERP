﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:4.0.30319.42000
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace Fedex.Properties {
    
    
    [global::System.Runtime.CompilerServices.CompilerGeneratedAttribute()]
    [global::System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.Editors.SettingsDesigner.SettingsSingleFileGenerator", "16.1.0.0")]
    internal sealed partial class Settings : global::System.Configuration.ApplicationSettingsBase {
        
        private static Settings defaultInstance = ((Settings)(global::System.Configuration.ApplicationSettingsBase.Synchronized(new Settings())));
        
        public static Settings Default {
            get {
                return defaultInstance;
            }
        }
        
        [global::System.Configuration.UserScopedSettingAttribute()]
        [global::System.Diagnostics.DebuggerNonUserCodeAttribute()]
        [global::System.Configuration.DefaultSettingValueAttribute("https://ws.fedex.com:443/web-services/track")]
        public string Fedex_FedexTrack_TrackService {
            get {
                return ((string)(this["Fedex_FedexTrack_TrackService"]));
            }
            set {
                this["Fedex_FedexTrack_TrackService"] = value;
            }
        }
        
        [global::System.Configuration.UserScopedSettingAttribute()]
        [global::System.Diagnostics.DebuggerNonUserCodeAttribute()]
        [global::System.Configuration.DefaultSettingValueAttribute("https://ws.fedex.com:443/web-services/rate")]
        public string Fedex_FedexRate_RateService {
            get {
                return ((string)(this["Fedex_FedexRate_RateService"]));
            }
            set {
                this["Fedex_FedexRate_RateService"] = value;
            }
        }
        
        [global::System.Configuration.UserScopedSettingAttribute()]
        [global::System.Diagnostics.DebuggerNonUserCodeAttribute()]
        [global::System.Configuration.DefaultSettingValueAttribute("https://wsbeta.fedex.com:443/web-services/track")]
        public string Fedex_FedexTrack_TrackService_TEST {
            get {
                return ((string)(this["Fedex_FedexTrack_TrackService_TEST"]));
            }
            set {
                this["Fedex_FedexTrack_TrackService_TEST"] = value;
            }
        }
        
        [global::System.Configuration.UserScopedSettingAttribute()]
        [global::System.Diagnostics.DebuggerNonUserCodeAttribute()]
        [global::System.Configuration.DefaultSettingValueAttribute("https://wsbeta.fedex.com:443/web-services/rate")]
        public string Fedex_FedexRate_RateService_TEST {
            get {
                return ((string)(this["Fedex_FedexRate_RateService_TEST"]));
            }
            set {
                this["Fedex_FedexRate_RateService_TEST"] = value;
            }
        }
        
        [global::System.Configuration.UserScopedSettingAttribute()]
        [global::System.Diagnostics.DebuggerNonUserCodeAttribute()]
        [global::System.Configuration.DefaultSettingValueAttribute("https://wsbeta.fedex.com:443/web-services/ship")]
        public string Fedex_FedexShip_ShipService_TEST {
            get {
                return ((string)(this["Fedex_FedexShip_ShipService_TEST"]));
            }
            set {
                this["Fedex_FedexShip_ShipService_TEST"] = value;
            }
        }
        
        [global::System.Configuration.UserScopedSettingAttribute()]
        [global::System.Diagnostics.DebuggerNonUserCodeAttribute()]
        [global::System.Configuration.DefaultSettingValueAttribute("https://ws.fedex.com:443/web-services/ship")]
        public string Fedex_FedexShip_ShipService {
            get {
                return ((string)(this["Fedex_FedexShip_ShipService"]));
            }
            set {
                this["Fedex_FedexShip_ShipService"] = value;
            }
        }
        
        [global::System.Configuration.ApplicationScopedSettingAttribute()]
        [global::System.Diagnostics.DebuggerNonUserCodeAttribute()]
        [global::System.Configuration.SpecialSettingAttribute(global::System.Configuration.SpecialSetting.WebServiceUrl)]
        [global::System.Configuration.DefaultSettingValueAttribute("https://wsbeta.fedex.com:443/web-services/ship")]
        public string Fedex_WebReference_ShipService {
            get {
                return ((string)(this["Fedex_WebReference_ShipService"]));
            }
        }
        
        [global::System.Configuration.ApplicationScopedSettingAttribute()]
        [global::System.Diagnostics.DebuggerNonUserCodeAttribute()]
        [global::System.Configuration.SpecialSettingAttribute(global::System.Configuration.SpecialSetting.WebServiceUrl)]
        [global::System.Configuration.DefaultSettingValueAttribute("https://wsbeta.fedex.com:443/web-services/rate")]
        public string Fedex_WebReference2_RateService {
            get {
                return ((string)(this["Fedex_WebReference2_RateService"]));
            }
        }
    }
}