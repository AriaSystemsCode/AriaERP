// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.DestinationPortDetailsType
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
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Track/v1.1")]
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [Serializable]
    public class DestinationPortDetailsType
    {
      private string destinationPortField;
      private DateTime estimatedArrivalField;

      public string DestinationPort
      {
        get => this.destinationPortField;
        set => this.destinationPortField = value;
      }

      public DateTime EstimatedArrival
      {
        get => this.estimatedArrivalField;
        set => this.estimatedArrivalField = value;
      }
    }
}
