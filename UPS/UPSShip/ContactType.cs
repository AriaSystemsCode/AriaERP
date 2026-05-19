// Decompiled with JetBrains decompiler
// Type: UPS.UPSShip.ContactType
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
    [XmlType(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/IF/v1.0")]
    [DebuggerStepThrough]
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [Serializable]
    public class ContactType
    {
      private ForwardAgentType forwardAgentField;
      private UltimateConsigneeType ultimateConsigneeField;
      private IntermediateConsigneeType intermediateConsigneeField;
      private ProducerType producerField;
      private SoldToType soldToField;

      public ForwardAgentType ForwardAgent
      {
        get => this.forwardAgentField;
        set => this.forwardAgentField = value;
      }

      public UltimateConsigneeType UltimateConsignee
      {
        get => this.ultimateConsigneeField;
        set => this.ultimateConsigneeField = value;
      }

      public IntermediateConsigneeType IntermediateConsignee
      {
        get => this.intermediateConsigneeField;
        set => this.intermediateConsigneeField = value;
      }

      public ProducerType Producer
      {
        get => this.producerField;
        set => this.producerField = value;
      }

      public SoldToType SoldTo
      {
        get => this.soldToField;
        set => this.soldToField = value;
      }
    }
}
