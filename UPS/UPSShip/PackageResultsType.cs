// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.PackageResultsType
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
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [DebuggerStepThrough]
    [Serializable]
    public class PackageResultsType
    {
      private string trackingNumberField;
      private ShipChargeType serviceOptionsChargesField;
      private LabelType shippingLabelField;
      private ReceiptType shippingReceiptField;
      private string uSPSPICNumberField;

      public string TrackingNumber
      {
        get => this.trackingNumberField;
        set => this.trackingNumberField = value;
      }

      public ShipChargeType ServiceOptionsCharges
      {
        get => this.serviceOptionsChargesField;
        set => this.serviceOptionsChargesField = value;
      }

      public LabelType ShippingLabel
      {
        get => this.shippingLabelField;
        set => this.shippingLabelField = value;
      }

      public ReceiptType ShippingReceipt
      {
        get => this.shippingReceiptField;
        set => this.shippingReceiptField = value;
      }

      public string USPSPICNumber
      {
        get => this.uSPSPICNumberField;
        set => this.uSPSPICNumberField = value;
      }
    }
}
