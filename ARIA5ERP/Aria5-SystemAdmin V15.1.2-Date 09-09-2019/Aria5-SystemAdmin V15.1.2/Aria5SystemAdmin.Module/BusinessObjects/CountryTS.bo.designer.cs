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
using DevExpress.Data.Filtering;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  public partial class CountryTS : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.Region _region;
    private Aria5SystemAdmin.Module.BusinessObjects.Currency _currency;
    private Aria5SystemAdmin.Module.BusinessObjects.Language _language;
    private System.String _name;
    private System.String _iD;
    public CountryTS(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public override void AfterConstruction()
    {
      base.AfterConstruction();
      // XPCollection<Region> Regions = new XPCollection<Region>(session, (CriteriaOperator.Parse("[Account] = '" + account + "' and [IsActive]='True' ")));
      try
      {
        Region = (Region)Session.FindObject<Entity>(CriteriaOperator.Parse("[Id] = 'NORTH'"));
      }
      catch (Exception)
      {
        //throw;
      }
    }
    public System.String ID
    {
      get
      {
        return _iD;
      }
      set
      {
        SetPropertyValue("ID", ref _iD, value);
      }
    }
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
    [DevExpress.Xpo.AssociationAttribute("Countries-Language")]
    public Aria5SystemAdmin.Module.BusinessObjects.Language Language
    {
      get
      {
        return _language;
      }
      set
      {
        SetPropertyValue("Language", ref _language, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Countries-Currency")]
    public Aria5SystemAdmin.Module.BusinessObjects.Currency Currency
    {
      get
      {
        return _currency;
      }
      set
      {
        SetPropertyValue("Currency", ref _currency, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Addresses-CountryTS")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Address> Addresses
    {
        get
        {
            return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Address>("Addresses");
        }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityAddresses-CountryTS")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityAddress> EntityAddresses
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityAddress>("EntityAddresses");
      }
    }

    //[DevExpress.Xpo.AssociationAttribute("ContactAddresses-CountryTS")]
    //public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ContactAddress> ContactAddresses
    //{
    //    get
    //    {
    //        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ContactAddress>("ContactAddresses");
    //    }
    //}
    [RuleRequiredField]
    [DevExpress.Xpo.AssociationAttribute("CountryTSs-Region")]public Aria5SystemAdmin.Module.BusinessObjects.Region Region
    {
      get
      {
        return _region;
      }
      set
      {
        SetPropertyValue("Region", ref _region, value);
      }
    }
  }
}