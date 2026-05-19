// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ReferenceNumberType
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
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [DebuggerStepThrough]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [Serializable]
    public class ReferenceNumberType
    {
      private string barCodeIndicatorField;
      private string codeField;
      private string valueField;

      public string BarCodeIndicator
      {
        get => this.barCodeIndicatorField;
        set => this.barCodeIndicatorField = value;
      }

      public string Code
      {
        get => this.codeField;
        set => this.codeField = value;
      }

      public string Value
      {
        get => this.valueField;
        set => this.valueField = value;
      }
    }
}
