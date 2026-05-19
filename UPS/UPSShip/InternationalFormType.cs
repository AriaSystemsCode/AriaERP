// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.InternationalFormType
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
    [DebuggerStepThrough]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/IF/v1.0")]
    [DesignerCategory("code")]
    [Serializable]
    public class InternationalFormType
    {
      private string[] formTypeField;
      private string additionalDocumentIndicatorField;
      private string formGroupIdNameField;
      private string sEDFilingOptionField;
      private ContactType contactsField;
      private ProductType[] productField;
      private string invoiceNumberField;
      private string invoiceDateField;
      private string purchaseOrderNumberField;
      private string termsOfShipmentField;
      private string reasonForExportField;
      private string commentsField;
      private string declarationStatementField;
      private IFChargesType discountField;
      private IFChargesType freightChargesField;
      private IFChargesType insuranceChargesField;
      private OtherChargesType otherChargesField;
      private string currencyCodeField;
      private BlanketPeriodType blanketPeriodField;
      private string exportDateField;
      private string exportingCarrierField;
      private string carrierIDField;
      private string inBondCodeField;
      private string entryNumberField;
      private string pointOfOriginField;
      private string modeOfTransportField;
      private string portOfExportField;
      private string portOfUnloadingField;
      private string loadingPierField;
      private string partiesToTransactionField;
      private string routedExportTransactionIndicatorField;
      private string containerizedIndicatorField;
      private LicenseType licenseField;
      private string eCCNNumberField;

      [XmlElement("FormType")]
      public string[] FormType
      {
        get => this.formTypeField;
        set => this.formTypeField = value;
      }

      public string AdditionalDocumentIndicator
      {
        get => this.additionalDocumentIndicatorField;
        set => this.additionalDocumentIndicatorField = value;
      }

      public string FormGroupIdName
      {
        get => this.formGroupIdNameField;
        set => this.formGroupIdNameField = value;
      }

      public string SEDFilingOption
      {
        get => this.sEDFilingOptionField;
        set => this.sEDFilingOptionField = value;
      }

      public ContactType Contacts
      {
        get => this.contactsField;
        set => this.contactsField = value;
      }

      [XmlElement("Product")]
      public ProductType[] Product
      {
        get => this.productField;
        set => this.productField = value;
      }

      public string InvoiceNumber
      {
        get => this.invoiceNumberField;
        set => this.invoiceNumberField = value;
      }

      public string InvoiceDate
      {
        get => this.invoiceDateField;
        set => this.invoiceDateField = value;
      }

      public string PurchaseOrderNumber
      {
        get => this.purchaseOrderNumberField;
        set => this.purchaseOrderNumberField = value;
      }

      public string TermsOfShipment
      {
        get => this.termsOfShipmentField;
        set => this.termsOfShipmentField = value;
      }

      public string ReasonForExport
      {
        get => this.reasonForExportField;
        set => this.reasonForExportField = value;
      }

      public string Comments
      {
        get => this.commentsField;
        set => this.commentsField = value;
      }

      public string DeclarationStatement
      {
        get => this.declarationStatementField;
        set => this.declarationStatementField = value;
      }

      public IFChargesType Discount
      {
        get => this.discountField;
        set => this.discountField = value;
      }

      public IFChargesType FreightCharges
      {
        get => this.freightChargesField;
        set => this.freightChargesField = value;
      }

      public IFChargesType InsuranceCharges
      {
        get => this.insuranceChargesField;
        set => this.insuranceChargesField = value;
      }

      public OtherChargesType OtherCharges
      {
        get => this.otherChargesField;
        set => this.otherChargesField = value;
      }

      public string CurrencyCode
      {
        get => this.currencyCodeField;
        set => this.currencyCodeField = value;
      }

      public BlanketPeriodType BlanketPeriod
      {
        get => this.blanketPeriodField;
        set => this.blanketPeriodField = value;
      }

      public string ExportDate
      {
        get => this.exportDateField;
        set => this.exportDateField = value;
      }

      public string ExportingCarrier
      {
        get => this.exportingCarrierField;
        set => this.exportingCarrierField = value;
      }

      public string CarrierID
      {
        get => this.carrierIDField;
        set => this.carrierIDField = value;
      }

      public string InBondCode
      {
        get => this.inBondCodeField;
        set => this.inBondCodeField = value;
      }

      public string EntryNumber
      {
        get => this.entryNumberField;
        set => this.entryNumberField = value;
      }

      public string PointOfOrigin
      {
        get => this.pointOfOriginField;
        set => this.pointOfOriginField = value;
      }

      public string ModeOfTransport
      {
        get => this.modeOfTransportField;
        set => this.modeOfTransportField = value;
      }

      public string PortOfExport
      {
        get => this.portOfExportField;
        set => this.portOfExportField = value;
      }

      public string PortOfUnloading
      {
        get => this.portOfUnloadingField;
        set => this.portOfUnloadingField = value;
      }

      public string LoadingPier
      {
        get => this.loadingPierField;
        set => this.loadingPierField = value;
      }

      public string PartiesToTransaction
      {
        get => this.partiesToTransactionField;
        set => this.partiesToTransactionField = value;
      }

      public string RoutedExportTransactionIndicator
      {
        get => this.routedExportTransactionIndicatorField;
        set => this.routedExportTransactionIndicatorField = value;
      }

      public string ContainerizedIndicator
      {
        get => this.containerizedIndicatorField;
        set => this.containerizedIndicatorField = value;
      }

      public LicenseType License
      {
        get => this.licenseField;
        set => this.licenseField = value;
      }

      public string ECCNNumber
      {
        get => this.eCCNNumberField;
        set => this.eCCNNumberField = value;
      }
    }
}
