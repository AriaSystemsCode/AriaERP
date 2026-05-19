// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ShipmentResponse
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using Newtonsoft.Json;
using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;


namespace UPS.UPSShip
{
    [DesignerCategory("code")]
    [XmlType(AnonymousType = true, Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [Serializable]
    
    public class ShipmentResponse
    {
      private ResponseType responseField;
      private ShipmentResultsType shipmentResultsField;
        [JsonProperty("Response")]
        [XmlElement(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Common/v1.0")]
      public ResponseType Response
      {
        get => this.responseField;
        set => this.responseField = value;
      }
        [JsonProperty("ShipmentResults")]
        public ShipmentResultsType ShipmentResults
      {
        get => this.shipmentResultsField;
        set => this.shipmentResultsField = value;
      }
    }
    public class ShipmentResponseRoot
    {
        [JsonProperty("ShipmentResponse")]
        public ShipmentResponse ShipmentResponse { get; set; }
    }
}
