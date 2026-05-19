// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.AppointmentType
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
    [DesignerCategory("code")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Track/v1.1")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [Serializable]
    public class AppointmentType
    {
      private DateTime madeField;
      private DateTime requestedField;
      private string beginTimeField;
      private string endTimeField;

      public DateTime Made
      {
        get => this.madeField;
        set => this.madeField = value;
      }

      public DateTime Requested
      {
        get => this.requestedField;
        set => this.requestedField = value;
      }

      public string BeginTime
      {
        get => this.beginTimeField;
        set => this.beginTimeField = value;
      }

      public string EndTime
      {
        get => this.endTimeField;
        set => this.endTimeField = value;
      }
    }
}
