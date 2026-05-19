// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ShipToType
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
    [DesignerCategory("code")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [DebuggerStepThrough]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [Serializable]
    public class ShipToType : CompanyInfoType
    {
      private string faxNumberField;
      private string eMailAddressField;
      private ShipToAddressType addressField;
      private string locationIDField;

      public string FaxNumber
      {
        get => this.faxNumberField;
        set => this.faxNumberField = value;
      }

      public string EMailAddress
      {
        get => this.eMailAddressField;
        set => this.eMailAddressField = value;
      }

      public ShipToAddressType Address
      {
        get => this.addressField;
        set => this.addressField = value;
      }

      public string LocationID
      {
        get => this.locationIDField;
        set => this.locationIDField = value;
      }
    }
}
