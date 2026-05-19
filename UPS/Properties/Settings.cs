// Decompiled with JetBrains decompiler
// Type: UPS.Properties.Settings
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System.CodeDom.Compiler;
using System.Configuration;
using System.Diagnostics;
using System.Runtime.CompilerServices;


namespace UPS.Properties
{
    [GeneratedCode("Microsoft.VisualStudio.Editors.SettingsDesigner.SettingsSingleFileGenerator", "12.0.0.0")]
    [CompilerGenerated]
    internal sealed class Settings : ApplicationSettingsBase
    {
      private static Settings defaultInstance = (Settings) SettingsBase.Synchronized((SettingsBase) new Settings());

      public static Settings Default
      {
        get
        {
          Settings defaultInstance = Settings.defaultInstance;
          return defaultInstance;
        }
      }

      [DefaultSettingValue("https://wwwcie.ups.com/webservices/Ship")]
      [DebuggerNonUserCode]
      [ApplicationScopedSetting]
      public string UPS_UPSShip_ShipService_Test
      {
        get => (string) this[nameof (UPS_UPSShip_ShipService_Test)];
      }

      [DefaultSettingValue("https://wwwcie.ups.com/webservices/Track")]
      [ApplicationScopedSetting]
      [DebuggerNonUserCode]
      public string UPS_UPSTrack_TrackService_Test
      {
        get => (string) this[nameof (UPS_UPSTrack_TrackService_Test)];
      }

      [DebuggerNonUserCode]
      [ApplicationScopedSetting]
      [DefaultSettingValue("https://wwwcie.ups.com/webservices/Void")]
      public string UPS_UPSVoid_VoidService_Test
      {
        get => (string) this[nameof (UPS_UPSVoid_VoidService_Test)];
      }

      [DefaultSettingValue("https://wwwcie.ups.com/webservices/Ship")]
      [ApplicationScopedSetting]
      [DebuggerNonUserCode]
      [SpecialSetting(SpecialSetting.WebServiceUrl)]
      public string UPS_UPSShip_ShipService => (string) this[nameof (UPS_UPSShip_ShipService)];

      [ApplicationScopedSetting]
      [DefaultSettingValue("https://wwwcie.ups.com/webservices/Track")]
      [SpecialSetting(SpecialSetting.WebServiceUrl)]
      [DebuggerNonUserCode]
      public string UPS_UPSTrack_TrackService => (string) this[nameof (UPS_UPSTrack_TrackService)];

      [DebuggerNonUserCode]
      [SpecialSetting(SpecialSetting.WebServiceUrl)]
      [DefaultSettingValue("https://wwwcie.ups.com/webservices/Void")]
      [ApplicationScopedSetting]
      public string UPS_UPSVoid_VoidService => (string) this[nameof (UPS_UPSVoid_VoidService)];
    }
}
