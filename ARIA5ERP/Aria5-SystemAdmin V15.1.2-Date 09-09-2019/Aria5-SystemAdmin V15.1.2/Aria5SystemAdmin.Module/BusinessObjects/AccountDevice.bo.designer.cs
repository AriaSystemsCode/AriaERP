//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Persistent.Validation;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-AccountDevice")]

  public partial class AccountDevice : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.Account _account;
    public AccountDevice(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private string iD;
    [RuleRequiredField]
    public string ID
    {
      get
      {
        return iD;
      }
      set
      {
        SetPropertyValue<string>("ID", ref iD, value);
      }
    }
    //Property of Account
    private Aria5SystemAdmin.Module.BusinessObjects.Account account;
    [RuleRequiredField]
    [DevExpress.Xpo.AssociationAttribute("AccountDevices-Account")]
    public Aria5SystemAdmin.Module.BusinessObjects.Account Account
    {
      get
      {
        return account;
      }
      set
      {
        SetPropertyValue<Aria5SystemAdmin.Module.BusinessObjects.Account>("account", ref account, value);
      }
    }
    //Property of Account ID
    private string accountId;
    [RuleRequiredField]
    public string AccountId
    {
      get
      {
        return accountId;
      }
      set
      {
        SetPropertyValue<string>("AccountId", ref accountId, value);
      }
    }
    //Property of Account Name
    private string accountName;
    [RuleRequiredField]
    public string AccountName
    {
      get
      {
        return accountName;
      }
      set
      {
        SetPropertyValue<string>("AccountName", ref accountName, value);
      }
    }
    //Property of Device Name
    private string name;
    [RuleRequiredField]
    public string Name
    {
      get
      {
        return name;
      }
      set
      {
        SetPropertyValue<string>("Name", ref name, value);
      }
    }
    //Property of Media Access Control Address
    private string mACAddress;
    public string MACAddress
    {
      get
      {
        return mACAddress;
      }
      set
      {
        SetPropertyValue<string>("MACAddress", ref mACAddress, value);
      }
    }
    //Property of App Specific Hardware ID
    private string aSHWID;
    public string ASHWID
    {
      get
      {
        return aSHWID;
      }
      set
      {
        SetPropertyValue<string>("ASHWID", ref aSHWID, value);
      }
    }
    //Property of Screen Size
    private string screenSize;
    public string ScreenSize
    {
      get
      {
        return screenSize;
      }
      set
      {
        SetPropertyValue<string>("ScreenSize", ref screenSize, value);
      }
    }
    //Property of Device Signature
    private string deviceSignature;
    public string DeviceSignature
    {
      get
      {
        return deviceSignature;
      }
      set
      {
        SetPropertyValue<string>("DeviceSignature", ref deviceSignature, value);
      }
    }
    //Property of Operation System
    private string operatingSystem;
    public string OperatingSystem
    {
      get
      {
        return operatingSystem;
      }
      set
      {
        SetPropertyValue<string>("OperatingSystem", ref operatingSystem, value);
      }
    }
    //Property of Operation System
    private Guid location;
    public Guid Location
    {
      get
      {
        return location;
      }
      set
      {
        SetPropertyValue<Guid>("Location", ref location, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ConfigurationItems-Device")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> ConfigurationItems
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>("ConfigurationItems");
      }
    }
    //public Aria5SystemAdmin.Module.BusinessObjects.Account Account
    //{
    //  get
    //  {
    //    return _account;
    //  }
    //  set
    //  {
    //    SetPropertyValue("Account", ref _account, value);
    //  }
    //}
  }
}