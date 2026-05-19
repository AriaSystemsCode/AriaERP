// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.PackageDeclaredValueType
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
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [Serializable]
    public class PackageDeclaredValueType
    {
      private DeclaredValueType typeField;
      private string currencyCodeField;
      private string monetaryValueField;

      public DeclaredValueType Type
      {
        get => this.typeField;
        set => this.typeField = value;
      }

      public string CurrencyCode
      {
        get => this.currencyCodeField;
        set => this.currencyCodeField = value;
      }

      public string MonetaryValue
      {
        get => this.monetaryValueField;
        set => this.monetaryValueField = value;
      }
    }
}
