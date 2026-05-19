// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ImageType
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
    [XmlInclude(typeof (ReceiptType))]
    [XmlInclude(typeof (LabelType))]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [Serializable]
    public class ImageType
    {
      private ImageFormatType imageFormatField;
      private string graphicImageField;

      public ImageFormatType ImageFormat
      {
        get => this.imageFormatField;
        set => this.imageFormatField = value;
      }

      public string GraphicImage
      {
        get => this.graphicImageField;
        set => this.graphicImageField = value;
      }
    }
}
