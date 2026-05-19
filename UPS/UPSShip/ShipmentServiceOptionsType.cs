// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ShipmentServiceOptionsType
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
    public class ShipmentServiceOptionsType
    {
      private string saturdayDeliveryIndicatorField;
      private OnCallType onCallField;
      private CODType cODField;
      private NotificationType[] notificationField;
      private LabelDeliveryType labelDeliveryField;
      private InternationalFormType internationalFormsField;
      private DeliveryConfirmationType deliveryConfirmationField;
      private string returnOfDocumentIndicatorField;
      private string importControlIndicatorField;
      private LabelMethodType labelMethodField;
      private string commercialInvoiceRemovalIndicatorField;
      private string uPScarbonneutralIndicatorField;

      public string SaturdayDeliveryIndicator
      {
        get => this.saturdayDeliveryIndicatorField;
        set => this.saturdayDeliveryIndicatorField = value;
      }

      public OnCallType OnCall
      {
        get => this.onCallField;
        set => this.onCallField = value;
      }

      public CODType COD
      {
        get => this.cODField;
        set => this.cODField = value;
      }

      [XmlElement("Notification")]
      public NotificationType[] Notification
      {
        get => this.notificationField;
        set => this.notificationField = value;
      }

      public LabelDeliveryType LabelDelivery
      {
        get => this.labelDeliveryField;
        set => this.labelDeliveryField = value;
      }

      public InternationalFormType InternationalForms
      {
        get => this.internationalFormsField;
        set => this.internationalFormsField = value;
      }

      public DeliveryConfirmationType DeliveryConfirmation
      {
        get => this.deliveryConfirmationField;
        set => this.deliveryConfirmationField = value;
      }

      public string ReturnOfDocumentIndicator
      {
        get => this.returnOfDocumentIndicatorField;
        set => this.returnOfDocumentIndicatorField = value;
      }

      public string ImportControlIndicator
      {
        get => this.importControlIndicatorField;
        set => this.importControlIndicatorField = value;
      }

      public LabelMethodType LabelMethod
      {
        get => this.labelMethodField;
        set => this.labelMethodField = value;
      }

      public string CommercialInvoiceRemovalIndicator
      {
        get => this.commercialInvoiceRemovalIndicatorField;
        set => this.commercialInvoiceRemovalIndicatorField = value;
      }

      public string UPScarbonneutralIndicator
      {
        get => this.uPScarbonneutralIndicatorField;
        set => this.uPScarbonneutralIndicatorField = value;
      }
    }
}
