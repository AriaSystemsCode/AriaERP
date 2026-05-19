// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.CarrierActivityInformationType
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
    public class CarrierActivityInformationType
    {
      private string carrierIdField;
      private string descriptionField;
      private string statusField;
      private DateTime arrivalField;
      private DateTime departureField;
      private string originPortField;
      private string destinationPortField;

      public string CarrierId
      {
        get => this.carrierIdField;
        set => this.carrierIdField = value;
      }

      public string Description
      {
        get => this.descriptionField;
        set => this.descriptionField = value;
      }

      public string Status
      {
        get => this.statusField;
        set => this.statusField = value;
      }

      public DateTime Arrival
      {
        get => this.arrivalField;
        set => this.arrivalField = value;
      }

      public DateTime Departure
      {
        get => this.departureField;
        set => this.departureField = value;
      }

      public string OriginPort
      {
        get => this.originPortField;
        set => this.originPortField = value;
      }

      public string DestinationPort
      {
        get => this.destinationPortField;
        set => this.destinationPortField = value;
      }
    }
}
