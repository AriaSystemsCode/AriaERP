// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ProducerType
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
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/IF/v1.0")]
    [Serializable]
    public class ProducerType
    {
      private string optionField;
      private string companyNameField;
      private string taxIdentificationNumberField;
      private AddressType addressField;

      public string Option
      {
        get => this.optionField;
        set => this.optionField = value;
      }

      public string CompanyName
      {
        get => this.companyNameField;
        set => this.companyNameField = value;
      }

      public string TaxIdentificationNumber
      {
        get => this.taxIdentificationNumberField;
        set => this.taxIdentificationNumberField = value;
      }

      public AddressType Address
      {
        get => this.addressField;
        set => this.addressField = value;
      }
    }
}
