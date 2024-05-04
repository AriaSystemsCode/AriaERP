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
  [RelatedEntity("Aria5-SystemAdmin-Address")]
    [MapInheritance(MapInheritanceType.ParentTable)]

  public partial class Address : Entity
  {
    private Aria5SystemAdmin.Module.BusinessObjects.CountryTS _countryTS;
    //private System.String _newProperty2;
    //private System.String _newProperty1;
    //private System.String _newProperty;
    //private Aria5SystemAdmin.Module.BusinessObjects.Region _region;
    private System.String _name;
    // private System.String _region;
    private System.String _postalCode;
    private System.String _state;
    private System.String _city;
    private System.String _addressLine3;
    private System.String _addressLine2;
    private System.String _addressLine1;
    public Address(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    //Sara.N 6-01-2015 Modify ddress Entity[Start]
    //[RuleRequiredField]
    //Sara.N 6-01-2015 Modify ddress Entity[End]
    public override void AfterConstruction()
    {
      Id = "N/A";
      //HIA,1 Aria 5 - 1TouchAway - Iteration# 2015- 05 - Tracking# n - Aria5-DevExpress-Address - Entity Specification Document - 2015 June 04 [Begin]
      XPCollection<Region> regionList = new XPCollection<Region>(Session);
      regionList.Load();
      if (regionList != null)
      {
        Region = regionList[0];
      }
      //HIA,1 Aria 5 - 1TouchAway - Iteration# 2015- 05 - Tracking# n - Aria5-DevExpress-Address - Entity Specification Document - 2015 June 04 [End]
      base.AfterConstruction();
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
    public System.String AddressLine1
    {
      get
      {
        return _addressLine1;
      }
      set
      {
        SetPropertyValue("AddressLine1", ref _addressLine1, value);
      }
    }
    public System.String AddressLine2
    {
      get
      {
        return _addressLine2;
      }
      set
      {
        SetPropertyValue("AddressLine2", ref _addressLine2, value);
      }
    }
    public System.String AddressLine3
    {
      get
      {
        return _addressLine3;
      }
      set
      {
        SetPropertyValue("AddressLine3", ref _addressLine3, value);
      }
    }
    public System.String City
    {
      get
      {
        return _city;
      }
      set
      {
        SetPropertyValue("City", ref _city, value);
      }
    }
    public System.String State
    {
      get
      {
        return _state;
      }
      set
      {
        SetPropertyValue("State", ref _state, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(15)]
    public System.String PostalCode
    {
      get
      {
        return _postalCode;
      }
      set
      {
        SetPropertyValue("PostalCode", ref _postalCode, value);
      }
    }
    private Region region;
    public Region Region
    {
      get
      {
        return region;
      }
      set
      {
        SetPropertyValue("Region", ref region, value);
        //HIA,1 Aria 5 - 1TouchAway - Iteration# 2015- 05 - Tracking# n - Aria5-DevExpress-Address - Entity Specification Document - 2015 June 04 [Begin]
        if (region != null)
        {
          regionName = region.Description;
          if (Region.CountryTSs.Count > 0)
          {
            CountryTS = Region.CountryTSs[0];
          }
          else
          {
            CountryCode = "";
          }
        }
        //HIA,1 Aria 5 - 1TouchAway - Iteration# 2015- 05 - Tracking# n - Aria5-DevExpress-Address - Entity Specification Document - 2015 June 04 [End]
      }
    }
    //Sara.N 6-01-2015 Modify ddress Entity[Start]
    [RuleRequiredField]
    public System.String RegionName
    {
      get
      {
        return regionName;
      }
      set
      {
        SetPropertyValue("RegionName", ref regionName, value);
      }
    }
    private Contact contact;
    //Sara.N 6-01-2015 Modify ddress Entity[Start]
    //[RuleRequiredField]
    //Sara.N 6-01-2015 Modify ddress Entity[End]
    public Contact Contact
    {
      get
      {
        return contact;
      }
      set
      {
        SetPropertyValue("Contact", ref contact, value);
      }
    }
    private System.String countryCode;
    [RuleRequiredField]
    public System.String CountryCode
    {
      get
      {
        return countryCode;
      }
      set
      {
        //Sara.N 5-14-2015 Modify ddress Entity [Start]
        if (_countryTS != null)
        {
          countryCode = _countryTS.ID;
        }
        //Sara.N Sara.N 5-14-2015[END]
        SetPropertyValue("CountryCode", ref countryCode, value);
      }
    }
    private System.String regionName;
    //Sara.N 6-01-2015 Modify ddress Entity[Start]
    //public Aria5SystemAdmin.Module.BusinessObjects.Region Region
    //{
    //  get
    //  {
    //    return _region;
    //  }
    //  set
    //  {
    //    if (_region == value)
    //      return;
    //    Aria5SystemAdmin.Module.BusinessObjects.Region prevRegion = _region;
    //    _region = value;
    //    if (IsLoading)
    //      return;
    //    if (prevRegion != null && prevRegion.Address == this)
    //      prevRegion.Address = null;
    //    if (_region != null)
    //      _region.Address = this;
    //    OnChanged("Region");
    //  }
    //}
    //public System.String NewProperty
    //{
    //  get
    //  {
    //    return _newProperty;
    //  }
    //  set
    //  {
    //    SetPropertyValue("NewProperty", ref _newProperty, value);
    //  }
    //}
    //public System.String NewProperty1
    //{
    //  get
    //  {
    //    return _newProperty1;
    //  }
    //  set
    //  {
    //    SetPropertyValue("NewProperty1", ref _newProperty1, value);
    //  }
    //}
    //public System.String NewProperty2
    //{
    //  get
    //  {
    //    return _newProperty2;
    //  }
    //  set
    //  {
    //    SetPropertyValue("NewProperty2", ref _newProperty2, value);
    //  }
    //}
    [DevExpress.Xpo.AssociationAttribute("Addresses-CountryTS")]
    [ImmediatePostData]
    [DataSourceProperty("Region.CountryTSs", DataSourcePropertyIsNullMode.SelectAll)]
    public Aria5SystemAdmin.Module.BusinessObjects.CountryTS CountryTS
    {
      get
      {
        return _countryTS;
      }
      set
      {
        SetPropertyValue("CountryTS", ref _countryTS, value);
        //Sara.N 5-14-2015 Modify ddress Entity[Start]
        if (_countryTS != null)
        {
          CountryCode = _countryTS.ID;
        }
        //Sara.N 5-14-2015 Modify ddress Entity[END]
      }
    }
  }
}
