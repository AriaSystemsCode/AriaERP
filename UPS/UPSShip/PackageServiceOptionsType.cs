// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.PackageServiceOptionsType
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
    [DesignerCategory("code")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [Serializable]
    public class PackageServiceOptionsType
    {
      private DeliveryConfirmationType deliveryConfirmationField;
      private PackageDeclaredValueType declaredValueField;
      private PSOCODType cODField;
      private VerbalConfirmationType verbalConfirmationField;
      private string shipperReleaseIndicatorField;
      private PSONotificationType notificationField;
      private string returnsFlexibleAccessIndicatorField;

      public DeliveryConfirmationType DeliveryConfirmation
      {
        get => this.deliveryConfirmationField;
        set => this.deliveryConfirmationField = value;
      }

      public PackageDeclaredValueType DeclaredValue
      {
        get => this.declaredValueField;
        set => this.declaredValueField = value;
      }

      public PSOCODType COD
      {
        get => this.cODField;
        set => this.cODField = value;
      }

      public VerbalConfirmationType VerbalConfirmation
      {
        get => this.verbalConfirmationField;
        set => this.verbalConfirmationField = value;
      }

      public string ShipperReleaseIndicator
      {
        get => this.shipperReleaseIndicatorField;
        set => this.shipperReleaseIndicatorField = value;
      }

      public PSONotificationType Notification
      {
        get => this.notificationField;
        set => this.notificationField = value;
      }

      public string ReturnsFlexibleAccessIndicator
      {
        get => this.returnsFlexibleAccessIndicatorField;
        set => this.returnsFlexibleAccessIndicatorField = value;
      }
    }
}
