using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using System.ComponentModel;
using DevExpress.Data.Filtering;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.Model;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [DevExpress.ExpressApp.DC.XafDisplayNameAttribute("Contact")]
  [RelatedEntity("Aria5-Windows8Xaml-Contact")]
  [XafDefaultProperty("Name")]
  public partial class Contact : Entity
  {
    private System.Guid _parentContact;
    private System.String _industry;
    private System.String _priority;
    private System.String _currencyCode; 
    private System.String _languageCode;
    private System.String _name;
    private System.String _webPageAddress;
    private System.String _eMailAddress;
    private System.Decimal _currentBalance;
    private System.Decimal _creditLimit;
    public Contact(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private Language language;
    [RuleRequiredField]
    public System.String Name
    {
      get
      {
        return _name;
      }
      set
      {
        SetPropertyValue("Name", ref _name, value);
      }
    }
      //ATA get the langaugecode from language directly [Start]
      [ImmediatePostData]
    public Language Language
    {
      get
      {
          if (language != null)
          {
              _languageCode = language.Code;
          }
        return language;
          //ATA get the langaugecode from language directly [End]

      }
      set
      {
        SetPropertyValue("Language", ref language, value);
      }
    }
    private Currency currency;
    public Currency Currency
    {
      get
      {
        return currency;
      }
      set
      {
        SetPropertyValue("Currency", ref currency, value);
      }
    }
    public System.Decimal CreditLimit
    {
      get
      {
        return _creditLimit;
      }
      set
      {
        SetPropertyValue("CreditLimit", ref _creditLimit, value);
      }
    }
    public System.Decimal CurrentBalance
    {
      get
      {
        return _currentBalance;
      }
      set
      {
        SetPropertyValue("CurrentBalance", ref _currentBalance, value);
      }
    }
    //[RuleRequiredField]
    public System.String EMailAddress
    {
      get
      {
        return _eMailAddress;
      }
      set
      {
        SetPropertyValue("EMailAddress", ref _eMailAddress, value);
      }
    }
    public System.String WebPageAddress
    {
      get
      {
        return _webPageAddress;
      }
      set
      {
        SetPropertyValue("WebPageAddress", ref _webPageAddress, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ContactPhones-Contact")]
    public XPCollection<ContactPhone> ContactPhones
    {
      get
      {
        return GetCollection<ContactPhone>("ContactPhones");
      }
    }
    public System.String LanguageCode
    {
      get
      {
        return _languageCode;
      }
      set
      {
        SetPropertyValue("LanguageCode", ref _languageCode, value);
      }
    }
    public System.String CurrencyCode
    {
      get
      {
        return _currencyCode;
      }
      set
      {
        SetPropertyValue("CurrencyCode", ref _currencyCode, value);
      }
    }
    public System.String Priority
    {
      get
      {
        return _priority;
      }
      set
      {
        SetPropertyValue("Priority", ref _priority, value);
      }
    }
    public System.String Industry
    {
      get
      {
        return _industry;
      }
      set
      {
        SetPropertyValue("Industry", ref _industry, value);
      }
    }
    public System.Guid ParentContact
    {
      get
      {
        return _parentContact;
      }
      set
      {
        SetPropertyValue("ParentContact", ref _parentContact, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ContactAddresses-Contact")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ContactAddress> ContactAddresses
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ContactAddress>("ContactAddresses");
      }
    }



    //private System.String _SchemaName;
    //public System.String SchemaName
    //{
    //  get
    //  {
    //    return _SchemaName;
    //  }
    //  set
    //  {
    //    SetPropertyValue("SchemaName", ref _SchemaName, value);
    //  }
    //}

    //private System.String _ShareLevel;
    //public System.String ShareLevel
    //{
    //    get
    //    {
    //        return _ShareLevel;
    //    }
    //    set
    //    {
    //        SetPropertyValue("ShareLevel", ref _ShareLevel, value);
    //    }
    //}

  }
}