// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.NegotiatedRateChargesType
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
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [Serializable]
    public class NegotiatedRateChargesType
    {
      private ShipChargeType totalChargeField;

      public ShipChargeType TotalCharge
      {
        get => this.totalChargeField;
        set => this.totalChargeField = value;
      }
    }
}
