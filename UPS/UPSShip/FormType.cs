// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.FormType
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
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [Serializable]
    public class FormType
    {
      private string codeField;
      private string descriptionField;
      private FormImageType imageField;
      private string formGroupIdField;
      private string formGroupIdNameField;

      public string Code
      {
        get => this.codeField;
        set => this.codeField = value;
      }

      public string Description
      {
        get => this.descriptionField;
        set => this.descriptionField = value;
      }

      public FormImageType Image
      {
        get => this.imageField;
        set => this.imageField = value;
      }

      public string FormGroupId
      {
        get => this.formGroupIdField;
        set => this.formGroupIdField = value;
      }

      public string FormGroupIdName
      {
        get => this.formGroupIdNameField;
        set => this.formGroupIdNameField = value;
      }
    }
}
