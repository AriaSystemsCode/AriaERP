// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.DimensionsType
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
    [DesignerCategory("code")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [Serializable]
    public class DimensionsType
    {
      private ShipUnitOfMeasurementType unitOfMeasurementField;
      private string lengthField;
      private string widthField;
      private string heightField;

      public ShipUnitOfMeasurementType UnitOfMeasurement
      {
        get => this.unitOfMeasurementField;
        set => this.unitOfMeasurementField = value;
      }

      public string Length
      {
        get => this.lengthField;
        set => this.lengthField = value;
      }

      public string Width
      {
        get => this.widthField;
        set => this.widthField = value;
      }

      public string Height
      {
        get => this.heightField;
        set => this.heightField = value;
      }
    }
}
