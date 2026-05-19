// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.PackageServiceOptionsType
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
    [DesignerCategory("code")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Track/v1.1")]
    [DebuggerStepThrough]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [Serializable]
    public class PackageServiceOptionsType
    {
      private CODType cODField;
      private CodeDescriptionType1 signatureRequiredField;

      public CODType COD
      {
        get => this.cODField;
        set => this.cODField = value;
      }

      public CodeDescriptionType1 SignatureRequired
      {
        get => this.signatureRequiredField;
        set => this.signatureRequiredField = value;
      }
    }
}
