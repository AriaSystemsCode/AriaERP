// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.DeliveryConfirmationType
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
    [DesignerCategory("code")]
    [DebuggerStepThrough]
    [Serializable]
    public class DeliveryConfirmationType
    {
      private string dCISTypeField;
      private string dCISNumberField;

      public string DCISType
      {
        get => this.dCISTypeField;
        set => this.dCISTypeField = value;
      }

      public string DCISNumber
      {
        get => this.dCISNumberField;
        set => this.dCISNumberField = value;
      }
    }
}
