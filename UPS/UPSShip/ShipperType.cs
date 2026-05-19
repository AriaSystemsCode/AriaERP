// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ShipperType
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
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [Serializable]
    public class ShipperType : CompanyInfoType
    {
      private string shipperNumberField;
      private string faxNumberField;
      private string eMailAddressField;
      private ShipAddressType addressField;

      public string ShipperNumber
      {
        get => this.shipperNumberField;
        set => this.shipperNumberField = value;
      }

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

      public ShipAddressType Address
      {
        get => this.addressField;
        set => this.addressField = value;
      }
    }
}
