// Decompiled with JetBrains decompiler
// Type: UPS.UPSVoid.TransactionReferenceType
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;


namespace UPS.UPSVoid
{
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/Common/v1.0")]
    [DesignerCategory("code")]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DebuggerStepThrough]
    [Serializable]
    public class TransactionReferenceType
    {
      private string customerContextField;
      private string transactionIdentifierField;

      public string CustomerContext
      {
        get => this.customerContextField;
        set => this.customerContextField = value;
      }

      public string TransactionIdentifier
      {
        get => this.transactionIdentifierField;
        set => this.transactionIdentifierField = value;
      }
    }
}
