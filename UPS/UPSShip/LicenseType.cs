// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.LicenseType
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
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/IF/v1.0")]
    [DebuggerStepThrough]
    [Serializable]
    public class LicenseType
    {
      private string numberField;
      private string dateField;
      private string exceptionCodeField;

      public string Number
      {
        get => this.numberField;
        set => this.numberField = value;
      }

      public string Date
      {
        get => this.dateField;
        set => this.dateField = value;
      }

      public string ExceptionCode
      {
        get => this.exceptionCodeField;
        set => this.exceptionCodeField = value;
      }
    }
}
