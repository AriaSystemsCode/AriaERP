// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ProductType
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
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DesignerCategory("code")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/IF/v1.0")]
    [Serializable]
    public class ProductType
    {
      private string[] descriptionField;
      private UnitType unitField;
      private string commodityCodeField;
      private string partNumberField;
      private string originCountryCodeField;
      private string jointProductionIndicatorField;
      private string netCostCodeField;
      private NetCostDateType netCostDateRangeField;
      private string preferenceCriteriaField;
      private string producerInfoField;
      private string marksAndNumbersField;
      private string numberOfPackagesPerCommodityField;
      private ProductWeightType productWeightField;
      private string vehicleIDField;
      private ScheduleBType scheduleBField;
      private string exportTypeField;
      private string sEDTotalValueField;

      [XmlElement("Description")]
      public string[] Description
      {
        get => this.descriptionField;
        set => this.descriptionField = value;
      }

      public UnitType Unit
      {
        get => this.unitField;
        set => this.unitField = value;
      }

      public string CommodityCode
      {
        get => this.commodityCodeField;
        set => this.commodityCodeField = value;
      }

      public string PartNumber
      {
        get => this.partNumberField;
        set => this.partNumberField = value;
      }

      public string OriginCountryCode
      {
        get => this.originCountryCodeField;
        set => this.originCountryCodeField = value;
      }

      public string JointProductionIndicator
      {
        get => this.jointProductionIndicatorField;
        set => this.jointProductionIndicatorField = value;
      }

      public string NetCostCode
      {
        get => this.netCostCodeField;
        set => this.netCostCodeField = value;
      }

      public NetCostDateType NetCostDateRange
      {
        get => this.netCostDateRangeField;
        set => this.netCostDateRangeField = value;
      }

      public string PreferenceCriteria
      {
        get => this.preferenceCriteriaField;
        set => this.preferenceCriteriaField = value;
      }

      public string ProducerInfo
      {
        get => this.producerInfoField;
        set => this.producerInfoField = value;
      }

      public string MarksAndNumbers
      {
        get => this.marksAndNumbersField;
        set => this.marksAndNumbersField = value;
      }

      public string NumberOfPackagesPerCommodity
      {
        get => this.numberOfPackagesPerCommodityField;
        set => this.numberOfPackagesPerCommodityField = value;
      }

      public ProductWeightType ProductWeight
      {
        get => this.productWeightField;
        set => this.productWeightField = value;
      }

      public string VehicleID
      {
        get => this.vehicleIDField;
        set => this.vehicleIDField = value;
      }

      public ScheduleBType ScheduleB
      {
        get => this.scheduleBField;
        set => this.scheduleBField = value;
      }

      public string ExportType
      {
        get => this.exportTypeField;
        set => this.exportTypeField = value;
      }

      public string SEDTotalValue
      {
        get => this.sEDTotalValueField;
        set => this.sEDTotalValueField = value;
      }
    }
}
