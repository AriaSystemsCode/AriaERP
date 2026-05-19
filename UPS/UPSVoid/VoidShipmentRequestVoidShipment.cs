// Decompiled with JetBrains decompiler
// Type: UPS.UPSVoid.VoidShipmentRequestVoidShipment
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;


namespace UPS.UPSVoid
{
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DesignerCategory("code")]
    [XmlType(AnonymousType = true, Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Void/v1.1")]
    [DebuggerStepThrough]
    [Serializable]
    public class VoidShipmentRequestVoidShipment
    {
      private string shipmentIdentificationNumberField;
      private string[] trackingNumberField;

      public string ShipmentIdentificationNumber
      {
        get => this.shipmentIdentificationNumberField;
        set => this.shipmentIdentificationNumberField = value;
      }

      [XmlElement("TrackingNumber")]
      public string[] TrackingNumber
      {
        get => this.trackingNumberField;
        set => this.trackingNumberField = value;
      }
    }
}
