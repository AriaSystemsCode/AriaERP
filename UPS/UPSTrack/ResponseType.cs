// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.ResponseType
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
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Common/v1.0")]
    [Serializable]
    public class ResponseType
    {
      private CodeDescriptionType responseStatusField;
      private CodeDescriptionType[] alertField;
      private TransactionReferenceType transactionReferenceField;

      public CodeDescriptionType ResponseStatus
      {
        get => this.responseStatusField;
        set => this.responseStatusField = value;
      }

      [XmlElement("Alert")]
      public CodeDescriptionType[] Alert
      {
        get => this.alertField;
        set => this.alertField = value;
      }

      public TransactionReferenceType TransactionReference
      {
        get => this.transactionReferenceField;
        set => this.transactionReferenceField = value;
      }
    }
}
