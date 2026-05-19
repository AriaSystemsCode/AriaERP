// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.DateTime
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
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [Serializable]
    public class DateTime
    {
      private string dateField;
      private string timeField;

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
