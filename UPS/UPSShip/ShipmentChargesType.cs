// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ShipmentChargesType
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
    public class ShipmentChargesType
    {
      private ShipChargeType transportationChargesField;
      private ShipChargeType serviceOptionsChargesField;
      private ShipChargeType totalChargesField;

      public ShipChargeType TransportationCharges
      {
        get => this.transportationChargesField;
        set => this.transportationChargesField = value;
      }

      public ShipChargeType ServiceOptionsCharges
      {
        get => this.serviceOptionsChargesField;
        set => this.serviceOptionsChargesField = value;
      }

      public ShipChargeType TotalCharges
      {
        get => this.totalChargesField;
        set => this.totalChargesField = value;
      }
    }
}
