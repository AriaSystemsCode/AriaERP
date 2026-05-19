// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.CompanyInfoType1
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
    [XmlType(TypeName = "CompanyInfoType", Namespace = "http://www.ups.com/XMLSchema/XOLTWS/IF/v1.0")]
    [DesignerCategory("code")]
    [XmlInclude(typeof (SoldToType))]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [Serializable]
    public class CompanyInfoType1
    {
      private string nameField;
      private string attentionNameField;
      private string taxIdentificationNumberField;
      private PhoneType phoneField;

      public string Name
      {
        get => this.nameField;
        set => this.nameField = value;
      }

      public string AttentionName
      {
        get => this.attentionNameField;
        set => this.attentionNameField = value;
      }

      public string TaxIdentificationNumber
      {
        get => this.taxIdentificationNumberField;
        set => this.taxIdentificationNumberField = value;
      }

      public PhoneType Phone
      {
        get => this.phoneField;
        set => this.phoneField = value;
      }
    }
}
