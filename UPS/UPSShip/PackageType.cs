// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.PackageType
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
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [Serializable]
    public class PackageType
    {
      private string descriptionField;
      private PackagingType packagingField;
      private DimensionsType dimensionsField;
      private PackageWeightType packageWeightField;
      private string largePackageIndicatorField;
      private ReferenceNumberType[] referenceNumberField;
      private string additionalHandlingIndicatorField;
      private PackageServiceOptionsType packageServiceOptionsField;
      private CommodityType commodityField;

      public string Description
      {
        get => this.descriptionField;
        set => this.descriptionField = value;
      }

      public PackagingType Packaging
      {
        get => this.packagingField;
        set => this.packagingField = value;
      }

      public DimensionsType Dimensions
      {
        get => this.dimensionsField;
        set => this.dimensionsField = value;
      }

      public PackageWeightType PackageWeight
      {
        get => this.packageWeightField;
        set => this.packageWeightField = value;
      }

      public string LargePackageIndicator
      {
        get => this.largePackageIndicatorField;
        set => this.largePackageIndicatorField = value;
      }

      [XmlElement("ReferenceNumber")]
      public ReferenceNumberType[] ReferenceNumber
      {
        get => this.referenceNumberField;
        set => this.referenceNumberField = value;
      }

      public string AdditionalHandlingIndicator
      {
        get => this.additionalHandlingIndicatorField;
        set => this.additionalHandlingIndicatorField = value;
      }

      public PackageServiceOptionsType PackageServiceOptions
      {
        get => this.packageServiceOptionsField;
        set => this.packageServiceOptionsField = value;
      }

      public CommodityType Commodity
      {
        get => this.commodityField;
        set => this.commodityField = value;
      }
    }
}
