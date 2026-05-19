// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.CODType
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;


namespace UPS.UPSTrack
{
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Track/v1.1")]
    [DesignerCategory("code")]
    [DebuggerStepThrough]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [Serializable]
    public class CODType
    {
      private AmountType amountField;
      private CODStatusType statusField;
      private string controlNumberField;

      public AmountType Amount
      {
        get => this.amountField;
        set => this.amountField = value;
      }

      public CODStatusType Status
      {
        get => this.statusField;
        set => this.statusField = value;
      }

      public string ControlNumber
      {
        get => this.controlNumberField;
        set => this.controlNumberField = value;
      }
    }
}
