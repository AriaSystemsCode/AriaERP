// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.PickupDateRangeType
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
    [DebuggerStepThrough]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Track/v1.1")]
    [DesignerCategory("code")]
    [Serializable]
    public class PickupDateRangeType
    {
      private string beginDateField;
      private string endDateField;

      public string BeginDate
      {
        get => this.beginDateField;
        set => this.beginDateField = value;
      }

      public string EndDate
      {
        get => this.endDateField;
        set => this.endDateField = value;
      }
    }
}
