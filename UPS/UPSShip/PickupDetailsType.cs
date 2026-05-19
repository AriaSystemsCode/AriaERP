// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.PickupDetailsType
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
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [Serializable]
    public class PickupDetailsType
    {
      private string districtCodeField;
      private string pickupDateField;
      private string earliestTimeReadyField;
      private string latestTimeReadyField;
      private string suiteRoomIDField;
      private string floorIDField;
      private string locationField;
      private ContactInfoType contactInfoField;

      public string DistrictCode
      {
        get => this.districtCodeField;
        set => this.districtCodeField = value;
      }

      public string PickupDate
      {
        get => this.pickupDateField;
        set => this.pickupDateField = value;
      }

      public string EarliestTimeReady
      {
        get => this.earliestTimeReadyField;
        set => this.earliestTimeReadyField = value;
      }

      public string LatestTimeReady
      {
        get => this.latestTimeReadyField;
        set => this.latestTimeReadyField = value;
      }

      public string SuiteRoomID
      {
        get => this.suiteRoomIDField;
        set => this.suiteRoomIDField = value;
      }

      public string FloorID
      {
        get => this.floorIDField;
        set => this.floorIDField = value;
      }

      public string Location
      {
        get => this.locationField;
        set => this.locationField = value;
      }

      public ContactInfoType ContactInfo
      {
        get => this.contactInfoField;
        set => this.contactInfoField = value;
      }
    }
}
