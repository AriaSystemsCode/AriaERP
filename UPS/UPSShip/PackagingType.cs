// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.PackagingType
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
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DesignerCategory("code")]
    [Serializable]
    public class PackagingType
    {
      private string codeField;
      private string descriptionField;

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
    }
}
