// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.ServiceCenterType
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;


namespace UPS.UPSTrack
{
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Track/v1.1")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [Serializable]
    public class ServiceCenterType
    {
      private string cityField;
      private string stateProvinceCodeField;

      public string City
      {
        get => this.cityField;
        set => this.cityField = value;
      }

      public string StateProvinceCode
      {
        get => this.stateProvinceCodeField;
        set => this.stateProvinceCodeField = value;
      }
    }
}
