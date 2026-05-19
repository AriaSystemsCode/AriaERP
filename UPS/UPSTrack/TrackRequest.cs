// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.TrackRequest
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
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [XmlType(AnonymousType = true, Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Track/v1.1")]
    [Serializable]
    public class TrackRequest
    {
      private RequestType requestField;
      private string inquiryNumberField;
      private string trackingOptionField;
      private string candidateBookmarkField;
      private ReferenceNumberType referenceNumberField;
      private PickupDateRangeType pickupDateRangeField;
      private string shipperNumberField;
      private ShipFromRequestType shipFromField;
      private ShipToRequestType shipToField;
      private RefShipmentType shipmentTypeField;
      private ShipperAccountInfoType shipperAccountInfoField;

      [XmlElement(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Common/v1.0")]
      public RequestType Request
      {
        get => this.requestField;
        set => this.requestField = value;
      }

      public string InquiryNumber
      {
        get => this.inquiryNumberField;
        set => this.inquiryNumberField = value;
      }

      public string TrackingOption
      {
        get => this.trackingOptionField;
        set => this.trackingOptionField = value;
      }

      public string CandidateBookmark
      {
        get => this.candidateBookmarkField;
        set => this.candidateBookmarkField = value;
      }

      public ReferenceNumberType ReferenceNumber
      {
        get => this.referenceNumberField;
        set => this.referenceNumberField = value;
      }

      public PickupDateRangeType PickupDateRange
      {
        get => this.pickupDateRangeField;
        set => this.pickupDateRangeField = value;
      }

      public string ShipperNumber
      {
        get => this.shipperNumberField;
        set => this.shipperNumberField = value;
      }

      public ShipFromRequestType ShipFrom
      {
        get => this.shipFromField;
        set => this.shipFromField = value;
      }

      public ShipToRequestType ShipTo
      {
        get => this.shipToField;
        set => this.shipToField = value;
      }

      public RefShipmentType ShipmentType
      {
        get => this.shipmentTypeField;
        set => this.shipmentTypeField = value;
      }

      public ShipperAccountInfoType ShipperAccountInfo
      {
        get => this.shipperAccountInfoField;
        set => this.shipperAccountInfoField = value;
      }
    }
}
