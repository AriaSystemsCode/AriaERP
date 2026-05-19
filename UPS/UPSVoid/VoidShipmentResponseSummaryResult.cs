// Decompiled with JetBrains decompiler
// Type: UPS.UPSVoid.VoidShipmentResponseSummaryResult
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;


namespace UPS.UPSVoid
{
    [DebuggerStepThrough]
    [XmlType(AnonymousType = true, Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Void/v1.1")]
    [DesignerCategory("code")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [Serializable]
    public class VoidShipmentResponseSummaryResult
    {
      private CodeDescriptionType statusField;

      public CodeDescriptionType Status
      {
        get => this.statusField;
        set => this.statusField = value;
      }
    }
}
