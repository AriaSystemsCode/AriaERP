// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.LabelDeliveryType
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
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [Serializable]
    public class LabelDeliveryType
    {
      private EmailDetailsType eMailField;
      private string labelLinksIndicatorField;

      public EmailDetailsType EMail
      {
        get => this.eMailField;
        set => this.eMailField = value;
      }

      public string LabelLinksIndicator
      {
        get => this.labelLinksIndicatorField;
        set => this.labelLinksIndicatorField = value;
      }
    }
}
