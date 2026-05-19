// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.ActivityType
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
    [DesignerCategory("code")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Track/v1.1")]
    [Serializable]
    public class ActivityType
    {
      private ActivityLocationType activityLocationField;
      private StatusType statusField;
      private string dateField;
      private string timeField;

      public ActivityLocationType ActivityLocation
      {
        get => this.activityLocationField;
        set => this.activityLocationField = value;
      }

      public StatusType Status
      {
        get => this.statusField;
        set => this.statusField = value;
      }

      public string Date
      {
        get => this.dateField;
        set => this.dateField = value;
      }

      public string Time
      {
        get => this.timeField;
        set => this.timeField = value;
      }
    }
}
