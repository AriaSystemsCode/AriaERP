// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.UPSSecurityServiceAccessToken
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;


namespace UPS.UPSShip
{
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [XmlType(AnonymousType = true, Namespace = "http://www.ups.com/XMLSchema/XOLTWS/UPSS/v1.0")]
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [Serializable]
    public class UPSSecurityServiceAccessToken
    {
      private string accessLicenseNumberField;

      public string AccessLicenseNumber
      {
        get => this.accessLicenseNumberField;
        set => this.accessLicenseNumberField = value;
      }
    }
}
