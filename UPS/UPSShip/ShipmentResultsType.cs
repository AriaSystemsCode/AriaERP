// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ShipmentResultsType
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
    [DesignerCategory("code")]
    [DebuggerStepThrough]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [Serializable]
    public class ShipmentResultsType
    {
      private ShipmentChargesType shipmentChargesField;
      private NegotiatedRateChargesType negotiatedRateChargesField;
      private FRSShipmentDataType fRSShipmentDataField;
      private BillingWeightType billingWeightField;
      private string shipmentIdentificationNumberField;
      private string shipmentDigestField;
      private string pickupRequestNumberField;
      private PackageResultsType[] packageResultsField;
      private ImageType[] controlLogReceiptField;
      private FormType formField;
      private SCReportType cODTurnInPageField;
      private HighValueReportType highValueReportField;
      private string labelURLField;
      private string localLanguageLabelURLField;
      private string receiptURLField;
      private string localLanguageReceiptURLField;

      public ShipmentChargesType ShipmentCharges
      {
        get => this.shipmentChargesField;
        set => this.shipmentChargesField = value;
      }

      public NegotiatedRateChargesType NegotiatedRateCharges
      {
        get => this.negotiatedRateChargesField;
        set => this.negotiatedRateChargesField = value;
      }

      public FRSShipmentDataType FRSShipmentData
      {
        get => this.fRSShipmentDataField;
        set => this.fRSShipmentDataField = value;
      }

      public BillingWeightType BillingWeight
      {
        get => this.billingWeightField;
        set => this.billingWeightField = value;
      }

      public string ShipmentIdentificationNumber
      {
        get => this.shipmentIdentificationNumberField;
        set => this.shipmentIdentificationNumberField = value;
      }

      public string ShipmentDigest
      {
        get => this.shipmentDigestField;
        set => this.shipmentDigestField = value;
      }

      public string PickupRequestNumber
      {
        get => this.pickupRequestNumberField;
        set => this.pickupRequestNumberField = value;
      }

      [XmlElement("PackageResults")]
      public PackageResultsType[] PackageResults
      {
        get => this.packageResultsField;
        set => this.packageResultsField = value;
      }

      [XmlElement("ControlLogReceipt")]
      public ImageType[] ControlLogReceipt
      {
        get => this.controlLogReceiptField;
        set => this.controlLogReceiptField = value;
      }

      public FormType Form
      {
        get => this.formField;
        set => this.formField = value;
      }

      public SCReportType CODTurnInPage
      {
        get => this.cODTurnInPageField;
        set => this.cODTurnInPageField = value;
      }

      public HighValueReportType HighValueReport
      {
        get => this.highValueReportField;
        set => this.highValueReportField = value;
      }

      public string LabelURL
      {
        get => this.labelURLField;
        set => this.labelURLField = value;
      }

      public string LocalLanguageLabelURL
      {
        get => this.localLanguageLabelURLField;
        set => this.localLanguageLabelURLField = value;
      }

      public string ReceiptURL
      {
        get => this.receiptURLField;
        set => this.receiptURLField = value;
      }

      public string LocalLanguageReceiptURL
      {
        get => this.localLanguageReceiptURLField;
        set => this.localLanguageReceiptURLField = value;
      }
    }
}
