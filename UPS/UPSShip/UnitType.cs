// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.UnitType
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
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/IF/v1.0")]
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [Serializable]
    public class UnitType
    {
      private string numberField;
      private UnitOfMeasurementType unitOfMeasurementField;
      private string valueField;

      public string Number
      {
        get => this.numberField;
        set => this.numberField = value;
      }

      public UnitOfMeasurementType UnitOfMeasurement
      {
        get => this.unitOfMeasurementField;
        set => this.unitOfMeasurementField = value;
      }

      public string Value
      {
        get => this.valueField;
        set => this.valueField = value;
      }
    }
}
