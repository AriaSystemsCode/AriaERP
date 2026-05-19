// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.CreditCardAddressType
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
    [DebuggerStepThrough]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DesignerCategory("code")]
    [Serializable]
    public class CreditCardAddressType
    {
      private string[] addressLineField;
      private string cityField;
      private string stateProvinceCodeField;
      private string postalCodeField;
      private string countryCodeField;

      [XmlElement("AddressLine")]
      public string[] AddressLine
      {
        get => this.addressLineField;
        set => this.addressLineField = value;
      }

      public string City
      {
        get => this.cityField;
        set => this.cityField = value;
      }

      public string StateProvinceCode
      {
        get => this.stateProvinceCodeField;
        set => this.stateProvinceCodeField = value;
      }

      public string PostalCode
      {
        get => this.postalCodeField;
        set => this.postalCodeField = value;
      }

      public string CountryCode
      {
        get => this.countryCodeField;
        set => this.countryCodeField = value;
      }
    }
}
