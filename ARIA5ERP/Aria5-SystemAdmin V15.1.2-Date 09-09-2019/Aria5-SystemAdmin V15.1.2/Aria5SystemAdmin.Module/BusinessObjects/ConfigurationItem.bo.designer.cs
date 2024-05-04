using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.ExpressApp.Editors;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-ConfigurationItem")]


  public partial class ConfigurationItem : Entity
  {
    private Aria5SystemAdmin.Module.BusinessObjects.Account _account;
    private Aria5SystemAdmin.Module.BusinessObjects.AccountDevice _device;
    private System.Guid _demoAccount;
    private System.Guid _contract;
    private System.DateTime _endDate;
    private System.DateTime _startDate;
    private Version_Types _versionType;
    private System.String _applicationName;
    private System.String _applicationId;
    private System.String _deviceName;
    private System.String _deviceSignature;
    private System.String _contactName;
    //private Aria5SystemAdmin.Module.BusinessObjects.Contact _account;
    //private Aria5SystemAdmin.Module.BusinessObjects.Contact _account;
    private Contact _contact;
    //private Account _account;
    private System.String _accountName;
    private System.String _accountId;
    private System.String _referenceTitle;
    private System.String _referenceNumber;
    private System.String _serialNo;
    private System.DateTime _expireDate;
    private System.DateTime _installationDate;
    private System.String _installedBy;
    private System.Int16 _numberOfUsers;
    private System.Int16 _trialDays;
    private production_types _productionType;
    public ConfigurationItem(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    //public System.String InstallationType
    //{
    //    get
    //    {
    //        return _installationType;
    //    }
    //    set
    //    {
    //        SetPropertyValue("InstallationType", ref _installationType, value);
    //    }
    //}
    private installation_types installationType;
    [RuleRequiredField]
    public installation_types InstallationType
    {
      get
      {
        return installationType;
      }
      set
      {
        SetPropertyValue("InstallationTypes", ref installationType, value);
      }
    }
    public production_types ProductionType
    {
      get
      {
        return _productionType;
      }
      set
      {
        SetPropertyValue("ProductionType", ref _productionType, value);
      }
    }
    [RuleRequiredField]
    public System.Int16 NumberOfUsers
    {
      get
      {
        return _numberOfUsers;
      }
      set
      {
        SetPropertyValue("NumberOfUsers", ref _numberOfUsers, value);
      }
    }
    private Application_T application;
    [RuleRequiredField]
    public Application_T Application
    {
      get
      {
        return application;
      }
      set
      {
        SetPropertyValue("Application", ref application, value);
        if (Application != null)
        {
          ApplicationId = Application.Id;
          ApplicationName = Application.Name;
        }
      }
    }
    //private Person clientcontact;
    ////[DataSourceCriteria("Business = 'E80C619E-E05C-42C4-90D2-E3F8062B3292'")] 
    //[DataSourceProperty("ClientPersons")]
    //public Person ClientContact
    //{
    //  get
    //  {
    //    return clientcontact;
    //  }
    //  set
    //  {
    //    SetPropertyValue("ClientContact", ref clientcontact, value);
    //  }
    //}
    //public List<Person> ClientPersons
    //{
    //  get
    //  {
    //    XPCollection<Person> col = new XPCollection<Person>(Session);
    //    return col.Where(r => r.Business.Oid == Account.Oid).ToList();
    //  }
    //}
    public System.Int16 TrialDays
    {
      get
      {
        return _trialDays;
      }
      set
      {
        SetPropertyValue("TrialDays", ref _trialDays, value);
      }
    }
    public System.String InstalledBy
    {
      get
      {
        return _installedBy;
      }
      set
      {
        SetPropertyValue("InstalledBy", ref _installedBy, value);
      }
    }
    public System.DateTime InstallationDate
    {
      get
      {
        return _installationDate;
      }
      set
      {
        SetPropertyValue("InstallationDate", ref _installationDate, value);
      }
    }
    public System.DateTime ExpireDate
    {
      get
      {
        return _expireDate;
      }
      set
      {
        SetPropertyValue("ExpireDate", ref _expireDate, value);
      }
    }
    public System.String SerialNo
    {
      get
      {
        return _serialNo;
      }
      set
      {
        SetPropertyValue("SerialNo", ref _serialNo, value);
      }
    }
    public System.String ReferenceNumber
    {
      get
      {
        return _referenceNumber;
      }
      set
      {
        SetPropertyValue("ReferenceNumber", ref _referenceNumber, value);
      }
    }
    public System.String ReferenceTitle
    {
      get
      {
        return _referenceTitle;
      }
      set
      {
        SetPropertyValue("ReferenceTitle", ref _referenceTitle, value);
      }
    }
    //[DevExpress.Xpo.AssociationAttribute("ConfigurationItems-Client")]
    //public Account Account
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
    //[RuleRequiredField]
    //public Aria5SystemAdmin.Module.BusinessObjects.Contact Account
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
    //[RuleRequiredField]
    //public Client Account
    //{
    //    get
    //    {
    //        return _account;
    //    }
    //    set
    //    {
    //        SetPropertyValue("Account", ref _account, value);
    //    }
    //}
    [RuleRequiredField]
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String AccountId
    {
      get
      {
        return _accountId;
      }
      set
      {
        SetPropertyValue("AccountId", ref _accountId, value);
      }
    }
    [RuleRequiredField]
    [DevExpress.Xpo.SizeAttribute(100)]
    public System.String AccountName
    {
      get
      {
        //if ( _accountId != null)
        //{
        //    var account = Session.FindObject<Account>("ID = '" + _accountId + "'");
        //    if (account != null) _accountName = account.Name;
        //}  
        return _accountName;
      }
      set
      {
        //if (Account != null) AccountName = Account.Name;
        SetPropertyValue("AccountName", ref _accountName, value);
        // if (Account != null) AccountName = Account.Name;
      }
    }
    public Contact Contact
    {
      get
      {
        return _contact;
      }
      set
      {
        SetPropertyValue("Contact", ref _contact, value);
        if (Contact != null)
        {
          ContactName = _contact.Name;
        }
      }
    }
    private System.String _activationKeyID;
    [RuleRequiredField]
    [DevExpress.Xpo.SizeAttribute(100)]
    public System.String ContactName
    {
      get
      {
        return _contactName;
      }
      set
      {
        //   if (Contact != null) ContactName = Contact.Name;
        SetPropertyValue("ContactName", ref _contactName, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String ActivationKeyID
    {
      get
      {
        return _activationKeyID;
      }
      set
      {
        SetPropertyValue("ActivationKeyID", ref _activationKeyID, value);
      }
    }
    private System.String _serialNumber;
    [DevExpress.Xpo.SizeAttribute(100)]
    public System.String SerialNumber
    {
      get
      {
        return _serialNumber;
      }
      set
      {
        SetPropertyValue("SerialNumber", ref _serialNumber, value);
      }
    }
    private System.String _serviceOrBundle;
    [DevExpress.Xpo.SizeAttribute(100)]
    public System.String ServiceOrBundle
    {
      get
      {
        return _serviceOrBundle;
      }
      set
      {
        SetPropertyValue("ServiceOrBundle", ref _serviceOrBundle, value);
      }
    }
    private System.String _contractName;
    [DevExpress.Xpo.SizeAttribute(100)]
    public System.String ContractName
    {
      get
      {
        return _contractName;
      }
      set
      {
        SetPropertyValue("ContractName", ref _contractName, value);
      }
    }
    public System.Guid Contract
    {
      get
      {
        return _contract;
      }
      set
      {
        SetPropertyValue("Contract", ref _contract, value);
      }
    }
    public System.String DeviceSignature
    {
      get
      {
        return _deviceSignature;
      }
      set
      {
        SetPropertyValue("DeviceSignature", ref _deviceSignature, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ConfigurationItems-Device")]
    [DevExpress.Persistent.Base.DataSourcePropertyAttribute("Account.AccountDevices")]
    public Aria5SystemAdmin.Module.BusinessObjects.AccountDevice Device
    {
      get
      {
        return _device;
      }
      set
      {
        SetPropertyValue("Device", ref _device, value);
        if (Device != null)
        {
          DeviceName = _device.Name;
          DeviceSignature = _device.DeviceSignature;
        }
      }
    }
    public System.String DeviceName
    {
      get
      {
        return _deviceName;
      }
      set
      {
        SetPropertyValue("DeviceName", ref _deviceName, value);
      }
    }
    [RuleRequiredField]
    public System.String ApplicationId
    {
      get
      {
        return _applicationId;
      }
      set
      {
        SetPropertyValue("ApplicationId", ref _applicationId, value);
      }
    }
    [RuleRequiredField]
    public System.String ApplicationName
    {
      get
      {
        return _applicationName;
      }
      set
      {
        SetPropertyValue("ApplicationName", ref _applicationName, value);
      }
    }
    [RuleRequiredField]
    public Version_Types VersionType
    {
      get
      {
        return _versionType;
      }
      set
      {
        SetPropertyValue("VersionType", ref _versionType, value);
      }
    }
    [RuleRequiredField]
    public System.DateTime StartDate
    {
      get
      {
        return _startDate;
      }
      set
      {
        SetPropertyValue("StartDate", ref _startDate, value);
      }
    }
    [RuleRequiredField]
    public System.DateTime EndDate
    {
      get
      {
        return _endDate;
      }
      set
      {
        SetPropertyValue("EndDate", ref _endDate, value);
      }
    }
    public System.Guid DemoAccount
    {
      get
      {
        return _demoAccount;
      }
      set
      {
        SetPropertyValue("DemoAccount", ref _demoAccount, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ActivationKeies-ConfigurationItem")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ActivationKey> ActivationKeies
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ActivationKey>("ActivationKeies");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ConfigurationItems-Account")]
    public Aria5SystemAdmin.Module.BusinessObjects.Account Account
    {
      get
      {
        return _account;
      }
      set
      {
        SetPropertyValue("Account", ref _account, value);
        if (_account != null)
        {
          if (_account.Id != null)
            AccountId = _account.Id;
          if (_account.Name != null)
            AccountName = _account.Name;
        }
      }
    }
  }
  public enum production_types
  {
    Trial = 0,
    Production = 1,
  }
  public enum installation_types
  {
    SaaS = 0,
    OnPremise = 1,
  }
  public enum Version_Types
  {
    Demo = 0,
    Trial = 1,
    Paying = 2,
  }
}
