// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ShipmentRequest
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
    [DebuggerStepThrough]
    [XmlType(AnonymousType = true, Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [Serializable]
    public class ShipmentRequest
    {
      private RequestType requestField;
      private ShipmentType shipmentField;
      private LabelSpecificationType labelSpecificationField;

      [XmlElement(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Common/v1.0")]
      public RequestType Request
      {
        get => this.requestField;
        set => this.requestField = value;
      }

      public ShipmentType Shipment
      {
        get => this.shipmentField;
        set => this.shipmentField = value;
      }

      //public LabelSpecificationType LabelSpecification
      //{
      //  get => this.labelSpecificationField;
      //  set => this.labelSpecificationField = value;
      //}
    }
}
