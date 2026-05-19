// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ShipmentType
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
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [Serializable]
    public class ShipmentType
    {
      private string descriptionField;
      private ReturnServiceType returnServiceField;
      private string documentsOnlyIndicatorField;
      private ShipperType shipperField;
      private ShipToType shipToField;
      private ShipFromType shipFromField;
      private PaymentInfoType paymentInformationField;
      private FRSPaymentInfoType fRSPaymentInformationField;
      private string goodsNotInFreeCirculationIndicatorField;
      private RateInfoType shipmentRatingOptionsField;
      private string movementReferenceNumberField;
      private ReferenceNumberType[] referenceNumberField;
      private ServiceType serviceField;
      private CurrencyMonetaryType invoiceLineTotalField;
      private ShipmentTypeShipmentServiceOptions shipmentServiceOptionsField;
      private PackageType[] packageField;

      public string Description
      {
        get => this.descriptionField;
        set => this.descriptionField = value;
      }

      public ReturnServiceType ReturnService
      {
        get => this.returnServiceField;
        set => this.returnServiceField = value;
      }

      public string DocumentsOnlyIndicator
      {
        get => this.documentsOnlyIndicatorField;
        set => this.documentsOnlyIndicatorField = value;
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

      public ShipFromType ShipFrom
      {
        get => this.shipFromField;
        set => this.shipFromField = value;
      }

      public PaymentInfoType PaymentInformation
      {
        get => this.paymentInformationField;
        set => this.paymentInformationField = value;
      }

      public FRSPaymentInfoType FRSPaymentInformation
      {
        get => this.fRSPaymentInformationField;
        set => this.fRSPaymentInformationField = value;
      }

      public string GoodsNotInFreeCirculationIndicator
      {
        get => this.goodsNotInFreeCirculationIndicatorField;
        set => this.goodsNotInFreeCirculationIndicatorField = value;
      }

      public RateInfoType ShipmentRatingOptions
      {
        get => this.shipmentRatingOptionsField;
        set => this.shipmentRatingOptionsField = value;
      }

      public string MovementReferenceNumber
      {
        get => this.movementReferenceNumberField;
        set => this.movementReferenceNumberField = value;
      }

      [XmlElement("ReferenceNumber")]
      public ReferenceNumberType[] ReferenceNumber
      {
        get => this.referenceNumberField;
        set => this.referenceNumberField = value;
      }

      public ServiceType Service
      {
        get => this.serviceField;
        set => this.serviceField = value;
      }

      public CurrencyMonetaryType InvoiceLineTotal
      {
        get => this.invoiceLineTotalField;
        set => this.invoiceLineTotalField = value;
      }

      public ShipmentTypeShipmentServiceOptions ShipmentServiceOptions
      {
        get => this.shipmentServiceOptionsField;
        set => this.shipmentServiceOptionsField = value;
      }

      [XmlElement("Package")]
      public PackageType[] Package
      {
        get => this.packageField;
        set => this.packageField = value;
      }
    }
}
