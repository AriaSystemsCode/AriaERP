// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.NMFCType
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
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DesignerCategory("code")]
    [DebuggerStepThrough]
    [Serializable]
    public class NMFCType
    {
      private string primeCodeField;
      private string subCodeField;

      public string PrimeCode
      {
        get => this.primeCodeField;
        set => this.primeCodeField = value;
      }

      public string SubCode
      {
        get => this.subCodeField;
        set => this.subCodeField = value;
      }
    }
}
