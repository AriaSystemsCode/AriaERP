// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.LabelSpecificationType
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
    public class LabelSpecificationType
    {
      private LabelImageFormatType labelImageFormatField;
      private string hTTPUserAgentField;
      private LabelStockSizeType labelStockSizeField;

      public LabelImageFormatType LabelImageFormat
      {
        get => this.labelImageFormatField;
        set => this.labelImageFormatField = value;
      }

      public string HTTPUserAgent
      {
        get => this.hTTPUserAgentField;
        set => this.hTTPUserAgentField = value;
      }

      public LabelStockSizeType LabelStockSize
      {
        get => this.labelStockSizeField;
        set => this.labelStockSizeField = value;
      }
    }
}
