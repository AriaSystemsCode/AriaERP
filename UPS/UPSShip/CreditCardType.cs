// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.CreditCardType
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
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [DesignerCategory("code")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [Serializable]
    public class CreditCardType
    {
      private string typeField;
      private string numberField;
      private string expirationDateField;
      private string securityCodeField;
      private CreditCardAddressType addressField;

      public string Type
      {
        get => this.typeField;
        set => this.typeField = value;
      }

      public string Number
      {
        get => this.numberField;
        set => this.numberField = value;
      }

      public string ExpirationDate
      {
        get => this.expirationDateField;
        set => this.expirationDateField = value;
      }

      public string SecurityCode
      {
        get => this.securityCodeField;
        set => this.securityCodeField = value;
      }

      public CreditCardAddressType Address
      {
        get => this.addressField;
        set => this.addressField = value;
      }
    }
}
