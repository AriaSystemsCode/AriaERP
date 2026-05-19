// Decompiled with JetBrains decompiler
// Type: UPS.UPSTrack.UPSSecurity
// Assembly: UPS, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: FE2C527D-E7B1-4D14-9A59-DD5F5EB261A9
// Assembly location: D:\Aria4xp\DLLS\UPS.dll

using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Web.Services.Protocols;
using System.Xml.Serialization;


namespace UPS.UPSTrack
{
    [GeneratedCode("System.Xml", "4.6.1087.0")]
    [XmlRoot(Namespace = "http://www.ups.com/XMLSchema/XOLTWS/UPSS/v1.0", IsNullable = false)]
    [DebuggerStepThrough]
    [DesignerCategory("code")]
    [XmlType(AnonymousType = true, Namespace = "http://www.ups.com/XMLSchema/XOLTWS/UPSS/v1.0")]
    [Serializable]
    public class UPSSecurity : SoapHeader
    {
      //private UPSSecurityUsernameToken usernameTokenField;
      private UPSSecurityServiceAccessToken serviceAccessTokenField;

      //public UPSSecurityUsernameToken UsernameToken
      //{
      //  get => this.usernameTokenField;
      //  set => this.usernameTokenField = value;
      //}

      public UPSSecurityServiceAccessToken ServiceAccessToken
      {
        get => this.serviceAccessTokenField;
        set => this.serviceAccessTokenField = value;
      }
    }
}
