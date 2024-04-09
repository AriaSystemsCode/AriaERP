using System;
using System.CodeDom.Compiler;
using System.Configuration;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Endicia.Properties
{
	[CompilerGenerated]
	[GeneratedCode("Microsoft.VisualStudio.Editors.SettingsDesigner.SettingsSingleFileGenerator", "10.0.0.0")]
	internal sealed class Settings : ApplicationSettingsBase
	{
		private static Settings defaultInstance;

		public static Settings Default
		{
			get
			{
				return Settings.defaultInstance;
			}
		}

		[ApplicationScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("https://LabelServer.endicia.com/LabelService/EwsLabelService.asmx")]
		[SpecialSetting(SpecialSetting.WebServiceUrl)]
		public string Endicia_WS_EwsLabelService_Live
		{
			get
			{
				return (string)this["Endicia_WS_EwsLabelService_Live"];
			}
		}

		[ApplicationScopedSetting]
		[DebuggerNonUserCode]
		[DefaultSettingValue("https://www.envmgr.com/LabelService/EwsLabelService.asmx")]
		[SpecialSetting(SpecialSetting.WebServiceUrl)]
		public string Endicia_WS_EwsLabelService_Test
		{
			get
			{
				return (string)this["Endicia_WS_EwsLabelService_Test"];
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