// Decompiled with JetBrains decompiler
// Type: UPS.UPSVoid.VoidShipmentResponse
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;
using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;


namespace UPS.UPSVoid
{
    [XmlType(AnonymousType = true, Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Void/v1.1")]
    [DesignerCategory("code")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [Serializable]
    public class VoidShipmentResponse
    {
      private ResponseType responseField;
      private VoidShipmentResponseSummaryResult summaryResultField;
      private UPSVoid.PackageLevelResult[] packageLevelResultField;
        [JsonProperty("Response")]
        [XmlElement(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Common/v1.0")]
      
      public ResponseType Response
      {
        get => this.responseField;
        set => this.responseField = value;
      }
        [JsonProperty("SummaryResult")]
        public VoidShipmentResponseSummaryResult SummaryResult
      {
        get => this.summaryResultField;
        set => this.summaryResultField = value;
      }
        [JsonProperty("PackageLevelResult")]
        [XmlElement("PackageLevelResult")]
      public UPSVoid.PackageLevelResult[] PackageLevelResult
      {
        get => this.packageLevelResultField;
        set => this.packageLevelResultField = value;
      }
    }
}
