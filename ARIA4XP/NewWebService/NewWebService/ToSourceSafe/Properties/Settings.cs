using System;
using System.CodeDom.Compiler;
using System.Configuration;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Fedex_1.Properties
{
	[CompilerGenerated]
	[GeneratedCode("Microsoft.VisualStudio.Editors.SettingsDesigner.SettingsSingleFileGenerator", "12.0.0.0")]
	public sealed class Settings : ApplicationSettingsBase
	{
		private static Settings defaultInstance;

		public static Settings Default
		{
			get
			{
				return Settings.defaultInstance;
			}
		}

		[DebuggerNonUserCode]
		[DefaultSettingValue("https://wsbeta.fedex.com:443/web-services/rate")]
		[SpecialSetting(SpecialSetting.WebServiceUrl)]
		[UserScopedSetting]
		public string Fedex_FedexRate_RateService
		{
			get
			{
				return (string)this["Fedex_FedexRate_RateService"];
			}
			set
			{
				this["Fedex_FedexRate_RateService"] = value;
			}
		}

		[DebuggerNonUserCode]
		[DefaultSettingValue("https://wsbeta.fedex.com:443/web-services/rate")]
		[SpecialSetting(SpecialSetting.WebServiceUrl)]
		[UserScopedSetting]
		public string Fedex_FedexRate_RateService_TEST
		{
			get
			{
				return (string)this["Fedex_FedexRate_RateService_TEST"];
			}
			set
			{
				this["Fedex_FedexRate_RateService_TEST"] = value;
			}
		}

		[DebuggerNonUserCode]
		[DefaultSettingValue("https://wsbeta.fedex.com:443/web-services/ship")]
		[SpecialSetting(SpecialSetting.WebServiceUrl)]
		[UserScopedSetting]
		public string Fedex_FedexShip_ShipService
		{
			get
			{
				return (string)this["Fedex_FedexShip_ShipService"];
			}
			set
			{
				this["Fedex_FedexShip_ShipService"] = value;
			}
		}

		[DebuggerNonUserCode]
		[DefaultSettingValue("https://wsbeta.fedex.com:443/web-services/ship")]
		[SpecialSetting(SpecialSetting.WebServiceUrl)]
		[UserScopedSetting]
		public string Fedex_FedexShip_ShipService_TEST
		{
			get
			{
				return (string)this["Fedex_FedexShip_ShipService_TEST"];
			}
			set
			{
				this["Fedex_FedexShip_ShipService_TEST"] = value;
			}
		}

		[DebuggerNonUserCode]
		[DefaultSettingValue("https://wsbeta.fedex.com:443/web-services/track")]
		[SpecialSetting(SpecialSetting.WebServiceUrl)]
		[UserScopedSetting]
		public string Fedex_FedexTrack_TrackService
		{
			get
			{
				return (string)this["Fedex_FedexTrack_TrackService"];
			}
			set
			{
				this["Fedex_FedexTrack_TrackService"] = value;
			}
		}

		[DebuggerNonUserCode]
		[DefaultSettingValue("https://wsbeta.fedex.com:443/web-services/track")]
		[SpecialSetting(SpecialSetting.WebServiceUrl)]
		[UserScopedSetting]
		public string Fedex_FedexTrack_TrackService_TEST
		{
			get
			{
				return (string)this["Fedex_FedexTrack_TrackService_TEST"];
			}
			set
			{
				this["Fedex_FedexTrack_TrackService_TEST"] = value;
			}
		}

		static Settings()
		{
			Settings.defaultInstance = (Settings)SettingsBase.Synchronized(new Settings());
		}

		public Settings()
		{
		}
	}
}