// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.PackageType
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
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [Serializable]
    public class PackageType
    {
      private string trackingNumberField;
      private DeliveryType rescheduledDeliveryField;
      private ReturnToType returnToField;
      private ReRouteType reRouteField;
      private PackageServiceOptionsType packageServiceOptionsField;
      private ActivityType[] activityField;
      private MessageType[] messageField;
      private WeightType packageWeightField;
      private ReferenceNumberType[] referenceNumberField;
      private ProductCodeDescriptionType productTypeField;
      private string locationAssuredField;
      private string[] alternateTrackingNumberField;

      public string TrackingNumber
      {
        get => this.trackingNumberField;
        set => this.trackingNumberField = value;
      }

      public DeliveryType RescheduledDelivery
      {
        get => this.rescheduledDeliveryField;
        set => this.rescheduledDeliveryField = value;
      }

      public ReturnToType ReturnTo
      {
        get => this.returnToField;
        set => this.returnToField = value;
      }

      public ReRouteType ReRoute
      {
        get => this.reRouteField;
        set => this.reRouteField = value;
      }

      public PackageServiceOptionsType PackageServiceOptions
      {
        get => this.packageServiceOptionsField;
        set => this.packageServiceOptionsField = value;
      }

      [XmlElement("Activity")]
      public ActivityType[] Activity
      {
        get => this.activityField;
        set => this.activityField = value;
      }

      [XmlElement("Message")]
      public MessageType[] Message
      {
        get => this.messageField;
        set => this.messageField = value;
      }

      public WeightType PackageWeight
      {
        get => this.packageWeightField;
        set => this.packageWeightField = value;
      }

      [XmlElement("ReferenceNumber")]
      public ReferenceNumberType[] ReferenceNumber
      {
        get => this.referenceNumberField;
        set => this.referenceNumberField = value;
      }

      public ProductCodeDescriptionType ProductType
      {
        get => this.productTypeField;
        set => this.productTypeField = value;
      }

      public string LocationAssured
      {
        get => this.locationAssuredField;
        set => this.locationAssuredField = value;
      }

      [XmlElement("AlternateTrackingNumber")]
      public string[] AlternateTrackingNumber
      {
        get => this.alternateTrackingNumberField;
        set => this.alternateTrackingNumberField = value;
      }
    }
}
