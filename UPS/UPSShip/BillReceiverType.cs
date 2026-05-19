// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.BillReceiverType
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
    [DesignerCategory("code")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [Serializable]
    public class BillReceiverType
    {
      private string accountNumberField;
      private BillReceiverAddressType addressField;

      public string AccountNumber
      {
        get => this.accountNumberField;
        set => this.accountNumberField = value;
      }

      public BillReceiverAddressType Address
      {
        get => this.addressField;
        set => this.addressField = value;
      }
    }
}
