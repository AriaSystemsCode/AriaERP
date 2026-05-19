// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.EmailDetailsType
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
    [DebuggerStepThrough]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [DesignerCategory("code")]
    [Serializable]
    public class EmailDetailsType
    {
      private string[] eMailAddressField;
      private string undeliverableEMailAddressField;
      private string fromEMailAddressField;
      private string fromNameField;
      private string memoField;
      private string subjectField;
      private string subjectCodeField;

      [XmlElement("EMailAddress")]
      public string[] EMailAddress
      {
        get => this.eMailAddressField;
        set => this.eMailAddressField = value;
      }

      public string UndeliverableEMailAddress
      {
        get => this.undeliverableEMailAddressField;
        set => this.undeliverableEMailAddressField = value;
      }

      public string FromEMailAddress
      {
        get => this.fromEMailAddressField;
        set => this.fromEMailAddressField = value;
      }

      public string FromName
      {
        get => this.fromNameField;
        set => this.fromNameField = value;
      }

      public string Memo
      {
        get => this.memoField;
        set => this.memoField = value;
      }

      public string Subject
      {
        get => this.subjectField;
        set => this.subjectField = value;
      }

      public string SubjectCode
      {
        get => this.subjectCodeField;
        set => this.subjectCodeField = value;
      }
    }
}
