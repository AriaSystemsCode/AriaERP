// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.ShipmentType
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
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Track/v1.1")]
    [Serializable]
    public class ShipmentType
    {
      private CodeDescriptionValueType inquiryNumberField;
      private RefShipmentType shipmentType1Field;
      private string candidateBookmarkField;
      private ShipperType shipperField;
      private ShipToType shipToField;
      private WeightType shipmentWeightField;
      private ServiceType serviceField;
      private ShipmentReferenceNumberType[] referenceNumberField;
      private CodeDescriptionType1 currentStatusField;
      private string pickupDateField;
      private DeliveryDetailsType deliveryDetailsField;
      private CodeDescriptionValueType volumeField;
      private string billToNameField;
      private ServiceCenterType pickUpServiceCenterField;
      private string numberOfPiecesField;
      private string numberOfPalletsField;
      private ShipmentServiceOptionsType shipmentServiceOptionsField;
      private EstimatedDeliveryDetailsType estimatedDeliveryDetailsField;
      private string signedForByNameField;
      private ShipmentActivityType[] activityField;
      private OriginPortDetailsType originPortDetailsField;
      private DestinationPortDetailsType destinationPortDetailsField;
      private string descriptionOfGoodsField;
      private DateTime cargoReadyField;
      private DateTime manifestField;
      private CarrierActivityInformationType[] carrierActivityInformationField;
      private DocumentType[] documentField;
      private DeliveryType scheduledDeliveryField;
      private string fileNumberField;
      private AppointmentType appointmentField;
      private PackageType[] packageField;

      public CodeDescriptionValueType InquiryNumber
      {
        get => this.inquiryNumberField;
        set => this.inquiryNumberField = value;
      }

      [XmlElement("ShipmentType")]
      public RefShipmentType ShipmentType1
      {
        get => this.shipmentType1Field;
        set => this.shipmentType1Field = value;
      }

      public string CandidateBookmark
      {
        get => this.candidateBookmarkField;
        set => this.candidateBookmarkField = value;
      }

      public ShipperType Shipper
      {
        get => this.shipperField;
        set => this.shipperField = value;
      }

      public ShipToType ShipTo
      {
        get => this.shipToField;
        set => this.shipToField = value;
      }

      public WeightType ShipmentWeight
      {
        get => this.shipmentWeightField;
        set => this.shipmentWeightField = value;
      }

      public ServiceType Service
      {
        get => this.serviceField;
        set => this.serviceField = value;
      }

      [XmlElement("ReferenceNumber")]
      public ShipmentReferenceNumberType[] ReferenceNumber
      {
        get => this.referenceNumberField;
        set => this.referenceNumberField = value;
      }

      public CodeDescriptionType1 CurrentStatus
      {
        get => this.currentStatusField;
        set => this.currentStatusField = value;
      }

      public string PickupDate
      {
        get => this.pickupDateField;
        set => this.pickupDateField = value;
      }

      public DeliveryDetailsType DeliveryDetails
      {
        get => this.deliveryDetailsField;
        set => this.deliveryDetailsField = value;
      }

      public CodeDescriptionValueType Volume
      {
        get => this.volumeField;
        set => this.volumeField = value;
      }

      public string BillToName
      {
        get => this.billToNameField;
        set => this.billToNameField = value;
      }

      public ServiceCenterType PickUpServiceCenter
      {
        get => this.pickUpServiceCenterField;
        set => this.pickUpServiceCenterField = value;
      }

      public string NumberOfPieces
      {
        get => this.numberOfPiecesField;
        set => this.numberOfPiecesField = value;
      }

      public string NumberOfPallets
      {
        get => this.numberOfPalletsField;
        set => this.numberOfPalletsField = value;
      }

      public ShipmentServiceOptionsType ShipmentServiceOptions
      {
        get => this.shipmentServiceOptionsField;
        set => this.shipmentServiceOptionsField = value;
      }

      public EstimatedDeliveryDetailsType EstimatedDeliveryDetails
      {
        get => this.estimatedDeliveryDetailsField;
        set => this.estimatedDeliveryDetailsField = value;
      }

      public string SignedForByName
      {
        get => this.signedForByNameField;
        set => this.signedForByNameField = value;
      }

      [XmlElement("Activity")]
      public ShipmentActivityType[] Activity
      {
        get => this.activityField;
        set => this.activityField = value;
      }

      public OriginPortDetailsType OriginPortDetails
      {
        get => this.originPortDetailsField;
        set => this.originPortDetailsField = value;
      }

      public DestinationPortDetailsType DestinationPortDetails
      {
        get => this.destinationPortDetailsField;
        set => this.destinationPortDetailsField = value;
      }

      public string DescriptionOfGoods
      {
        get => this.descriptionOfGoodsField;
        set => this.descriptionOfGoodsField = value;
      }

      public DateTime CargoReady
      {
        get => this.cargoReadyField;
        set => this.cargoReadyField = value;
      }

      public DateTime Manifest
      {
        get => this.manifestField;
        set => this.manifestField = value;
      }

      [XmlElement("CarrierActivityInformation")]
      public CarrierActivityInformationType[] CarrierActivityInformation
      {
        get => this.carrierActivityInformationField;
        set => this.carrierActivityInformationField = value;
      }

      [XmlElement("Document")]
      public DocumentType[] Document
      {
        get => this.documentField;
        set => this.documentField = value;
      }

      public DeliveryType ScheduledDelivery
      {
        get => this.scheduledDeliveryField;
        set => this.scheduledDeliveryField = value;
      }

      public string FileNumber
      {
        get => this.fileNumberField;
        set => this.fileNumberField = value;
      }

      public AppointmentType Appointment
      {
        get => this.appointmentField;
        set => this.appointmentField = value;
      }

      [XmlElement("Package")]
      public PackageType[] Package
      {
        get => this.packageField;
        set => this.packageField = value;
      }
    }
}
