﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:2.0.50727.3603
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

using System.Xml.Serialization;

// 
// This source code was auto-generated by xsd, Version=2.0.50727.3038.
// 


/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public partial class Address {
    
    private string nameField;
    
    private string addressFieldOneField;
    
    private string addressFieldTwoField;
    
    private string addressFieldThreeField;
    
    private string cityField;
    
    private string countyField;
    
    private string stateOrRegionField;
    
    private string postalCodeField;
    
    private string countryCodeField;
    
    private string phoneNumberField;
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string Name {
        get {
            return this.nameField;
        }
        set {
            this.nameField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string AddressFieldOne {
        get {
            return this.addressFieldOneField;
        }
        set {
            this.addressFieldOneField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string AddressFieldTwo {
        get {
            return this.addressFieldTwoField;
        }
        set {
            this.addressFieldTwoField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string AddressFieldThree {
        get {
            return this.addressFieldThreeField;
        }
        set {
            this.addressFieldThreeField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string City {
        get {
            return this.cityField;
        }
        set {
            this.cityField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string County {
        get {
            return this.countyField;
        }
        set {
            this.countyField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string StateOrRegion {
        get {
            return this.stateOrRegionField;
        }
        set {
            this.stateOrRegionField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string PostalCode {
        get {
            return this.postalCodeField;
        }
        set {
            this.postalCodeField = value;
        }
    }
    
    /// <remarks/>
    public string CountryCode {
        get {
            return this.countryCodeField;
        }
        set {
            this.countryCodeField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string PhoneNumber {
        get {
            return this.phoneNumberField;
        }
        set {
            this.phoneNumberField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public partial class Battery {
    
    private bool areBatteriesIncludedField;
    
    private bool areBatteriesIncludedFieldSpecified;
    
    private bool areBatteriesRequiredField;
    
    private bool areBatteriesRequiredFieldSpecified;
    
    private BatteryBatterySubgroup[] batterySubgroupField;
    
    /// <remarks/>
    public bool AreBatteriesIncluded {
        get {
            return this.areBatteriesIncludedField;
        }
        set {
            this.areBatteriesIncludedField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlIgnoreAttribute()]
    public bool AreBatteriesIncludedSpecified {
        get {
            return this.areBatteriesIncludedFieldSpecified;
        }
        set {
            this.areBatteriesIncludedFieldSpecified = value;
        }
    }
    
    /// <remarks/>
    public bool AreBatteriesRequired {
        get {
            return this.areBatteriesRequiredField;
        }
        set {
            this.areBatteriesRequiredField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlIgnoreAttribute()]
    public bool AreBatteriesRequiredSpecified {
        get {
            return this.areBatteriesRequiredFieldSpecified;
        }
        set {
            this.areBatteriesRequiredFieldSpecified = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute("BatterySubgroup")]
    public BatteryBatterySubgroup[] BatterySubgroup {
        get {
            return this.batterySubgroupField;
        }
        set {
            this.batterySubgroupField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public partial class BatteryBatterySubgroup {
    
    private BatteryBatterySubgroupBatteryType batteryTypeField;
    
    private string numberOfBatteriesField;
    
    /// <remarks/>
    public BatteryBatterySubgroupBatteryType BatteryType {
        get {
            return this.batteryTypeField;
        }
        set {
            this.batteryTypeField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="positiveInteger")]
    public string NumberOfBatteries {
        get {
            return this.numberOfBatteriesField;
        }
        set {
            this.numberOfBatteriesField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public enum BatteryBatterySubgroupBatteryType {
    
    /// <remarks/>
    battery_type_9v,
    
    /// <remarks/>
    battery_type_a,
    
    /// <remarks/>
    battery_type_aa,
    
    /// <remarks/>
    battery_type_aaa,
    
    /// <remarks/>
    battery_type_c,
    
    /// <remarks/>
    battery_type_cr123a,
    
    /// <remarks/>
    battery_type_cr2,
    
    /// <remarks/>
    battery_type_cr5,
    
    /// <remarks/>
    battery_type_d,
    
    /// <remarks/>
    battery_type_lithium_ion,
    
    /// <remarks/>
    battery_type_p76,
    
    /// <remarks/>
    battery_type_product_specific,
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public enum FulfillmentMethod {
    
    /// <remarks/>
    Ship,
    
    /// <remarks/>
    InStorePickup,
    
    /// <remarks/>
    MerchantDelivery,
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public enum FulfillmentServiceLevel {
    
    /// <remarks/>
    Standard,
    
    /// <remarks/>
    Expedited,
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public enum CarrierCode {
    
    /// <remarks/>
    USPS,
    
    /// <remarks/>
    UPS,
    
    /// <remarks/>
    FedEx,
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public partial class StandardProductID {
    
    private StandardProductIDType typeField;
    
    private string valueField;
    
    /// <remarks/>
    public StandardProductIDType Type {
        get {
            return this.typeField;
        }
        set {
            this.typeField = value;
        }
    }
    
    /// <remarks/>
    public string Value {
        get {
            return this.valueField;
        }
        set {
            this.valueField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public enum StandardProductIDType {
    
    /// <remarks/>
    ISBN,
    
    /// <remarks/>
    UPC,
    
    /// <remarks/>
    EAN,
    
    /// <remarks/>
    ASIN,
    
    /// <remarks/>
    GTIN,
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public enum ConditionType {
    
    /// <remarks/>
    New,
    
    /// <remarks/>
    UsedLikeNew,
    
    /// <remarks/>
    UsedVeryGood,
    
    /// <remarks/>
    UsedGood,
    
    /// <remarks/>
    UsedAcceptable,
    
    /// <remarks/>
    CollectibleLikeNew,
    
    /// <remarks/>
    CollectibleVeryGood,
    
    /// <remarks/>
    CollectibleGood,
    
    /// <remarks/>
    CollectibleAcceptable,
    
    /// <remarks/>
    Refurbished,
    
    /// <remarks/>
    Club,
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public partial class ComputerPlatform {
    
    private ComputerPlatformType typeField;
    
    private string systemRequirementsField;
    
    /// <remarks/>
    public ComputerPlatformType Type {
        get {
            return this.typeField;
        }
        set {
            this.typeField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string SystemRequirements {
        get {
            return this.systemRequirementsField;
        }
        set {
            this.systemRequirementsField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public enum ComputerPlatformType {
    
    /// <remarks/>
    windows,
    
    /// <remarks/>
    mac,
    
    /// <remarks/>
    linux,
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public partial class Rebate {
    
    private System.DateTime rebateStartDateField;
    
    private System.DateTime rebateEndDateField;
    
    private string rebateMessageField;
    
    private string rebateNameField;
    
    /// <remarks/>
    public System.DateTime RebateStartDate {
        get {
            return this.rebateStartDateField;
        }
        set {
            this.rebateStartDateField = value;
        }
    }
    
    /// <remarks/>
    public System.DateTime RebateEndDate {
        get {
            return this.rebateEndDateField;
        }
        set {
            this.rebateEndDateField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string RebateMessage {
        get {
            return this.rebateMessageField;
        }
        set {
            this.rebateMessageField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string RebateName {
        get {
            return this.rebateNameField;
        }
        set {
            this.rebateNameField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public partial class ColorSpecification {
    
    private string colorField;
    
    private ColorMap colorMapField;
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string Color {
        get {
            return this.colorField;
        }
        set {
            this.colorField = value;
        }
    }
    
    /// <remarks/>
    public ColorMap ColorMap {
        get {
            return this.colorMapField;
        }
        set {
            this.colorMapField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public enum ColorMap {
    
    /// <remarks/>
    beige,
    
    /// <remarks/>
    black,
    
    /// <remarks/>
    blue,
    
    /// <remarks/>
    bronze,
    
    /// <remarks/>
    brown,
    
    /// <remarks/>
    clear,
    
    /// <remarks/>
    gold,
    
    /// <remarks/>
    gray,
    
    /// <remarks/>
    green,
    
    /// <remarks/>
    metallic,
    
    /// <remarks/>
    [System.Xml.Serialization.XmlEnumAttribute("multi-colored")]
    multicolored,
    
    /// <remarks/>
    [System.Xml.Serialization.XmlEnumAttribute("off-white")]
    offwhite,
    
    /// <remarks/>
    orange,
    
    /// <remarks/>
    pink,
    
    /// <remarks/>
    purple,
    
    /// <remarks/>
    red,
    
    /// <remarks/>
    silver,
    
    /// <remarks/>
    white,
    
    /// <remarks/>
    yellow,
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public enum DeliveryChannel {
    
    /// <remarks/>
    in_store,
    
    /// <remarks/>
    direct_ship,
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace="", IsNullable=false)]
public partial class OrderFulfillment {
    
    private string itemField;
    
    private ItemChoiceType itemElementNameField;
    
    private string merchantFulfillmentIDField;
    
    private System.DateTime fulfillmentDateField;
    
    private OrderFulfillmentFulfillmentData fulfillmentDataField;
    
    private OrderFulfillmentItem[] item1Field;
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute("AmazonOrderID", typeof(string))]
    [System.Xml.Serialization.XmlElementAttribute("MerchantOrderID", typeof(string), DataType="normalizedString")]
    [System.Xml.Serialization.XmlChoiceIdentifierAttribute("ItemElementName")]
    public string Item {
        get {
            return this.itemField;
        }
        set {
            this.itemField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlIgnoreAttribute()]
    public ItemChoiceType ItemElementName {
        get {
            return this.itemElementNameField;
        }
        set {
            this.itemElementNameField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="positiveInteger")]
    public string MerchantFulfillmentID {
        get {
            return this.merchantFulfillmentIDField;
        }
        set {
            this.merchantFulfillmentIDField = value;
        }
    }
    
    /// <remarks/>
    public System.DateTime FulfillmentDate {
        get {
            return this.fulfillmentDateField;
        }
        set {
            this.fulfillmentDateField = value;
        }
    }
    
    /// <remarks/>
    public OrderFulfillmentFulfillmentData FulfillmentData {
        get {
            return this.fulfillmentDataField;
        }
        set {
            this.fulfillmentDataField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute("Item")]
    public OrderFulfillmentItem[] Item1 {
        get {
            return this.item1Field;
        }
        set {
            this.item1Field = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Xml.Serialization.XmlTypeAttribute(IncludeInSchema=false)]
public enum ItemChoiceType {
    
    /// <remarks/>
    AmazonOrderID,
    
    /// <remarks/>
    MerchantOrderID,
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public partial class OrderFulfillmentFulfillmentData {
    
    private object itemField;
    
    private string shippingMethodField;
    
    private string shipperTrackingNumberField;
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute("CarrierCode", typeof(CarrierCode))]
    [System.Xml.Serialization.XmlElementAttribute("CarrierName", typeof(string), DataType="normalizedString")]
    public object Item {
        get {
            return this.itemField;
        }
        set {
            this.itemField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string ShippingMethod {
        get {
            return this.shippingMethodField;
        }
        set {
            this.shippingMethodField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="normalizedString")]
    public string ShipperTrackingNumber {
        get {
            return this.shipperTrackingNumberField;
        }
        set {
            this.shipperTrackingNumberField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Diagnostics.DebuggerStepThroughAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType=true)]
public partial class OrderFulfillmentItem {
    
    private string itemField;
    
    private ItemChoiceType1 itemElementNameField;
    
    private string merchantFulfillmentItemIDField;
    
    private string quantityField;
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute("AmazonOrderItemCode", typeof(string))]
    [System.Xml.Serialization.XmlElementAttribute("MerchantOrderItemID", typeof(string), DataType="normalizedString")]
    [System.Xml.Serialization.XmlChoiceIdentifierAttribute("ItemElementName")]
    public string Item {
        get {
            return this.itemField;
        }
        set {
            this.itemField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlIgnoreAttribute()]
    public ItemChoiceType1 ItemElementName {
        get {
            return this.itemElementNameField;
        }
        set {
            this.itemElementNameField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="positiveInteger")]
    public string MerchantFulfillmentItemID {
        get {
            return this.merchantFulfillmentItemIDField;
        }
        set {
            this.merchantFulfillmentItemIDField = value;
        }
    }
    
    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute(DataType="positiveInteger")]
    public string Quantity {
        get {
            return this.quantityField;
        }
        set {
            this.quantityField = value;
        }
    }
}

/// <remarks/>
[System.CodeDom.Compiler.GeneratedCodeAttribute("xsd", "2.0.50727.3038")]
[System.SerializableAttribute()]
[System.Xml.Serialization.XmlTypeAttribute(IncludeInSchema=false)]
public enum ItemChoiceType1 {
    
    /// <remarks/>
    AmazonOrderItemCode,
    
    /// <remarks/>
    MerchantOrderItemID,
}