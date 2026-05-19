// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.PaymentInfoType
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
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [Serializable]
    public class PaymentInfoType
    {
      private ShipmentChargeType[] shipmentChargeField;
      private string splitDutyVATIndicatorField;

      [XmlElement("ShipmentCharge")]
      public ShipmentChargeType[] ShipmentCharge
      {
        get => this.shipmentChargeField;
        set => this.shipmentChargeField = value;
      }

      public string SplitDutyVATIndicator
      {
        get => this.splitDutyVATIndicatorField;
        set => this.splitDutyVATIndicatorField = value;
      }
    }
}
