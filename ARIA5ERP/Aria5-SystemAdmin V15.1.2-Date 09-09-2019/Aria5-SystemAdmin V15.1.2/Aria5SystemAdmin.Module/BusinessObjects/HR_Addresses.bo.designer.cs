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
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
    [NonPersistent]
  public partial class HR_Addresses : DevExpress.Persistent.BaseImpl.BaseObject
  {
    public HR_Addresses(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private string _name;
    public string Name
    {
      get
      {
        return _name;
      }
      set
      {
        _name = value;
      }
    }
    //private HR_Entity _hrentitiy;
    //[Association("HR_Entitiy-HR_Addresses")]
    //public HR_Entity HREntitiy
    //{
    //  get
    //  {
    //    return _hrentitiy;
    //  }
    //  set
    //  {
    //    _hrentitiy = value;
    //  }
    //}
    private Country _country;
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public Country Country
    {
      get
      {
          if (_countryname == null && _country != null)
          {
              _countryname = _country.Name;
          }
        return _country;
      }
      set
      {
        _country = value;
      }
    }
    private string _countryname;
    public string CountryName
    {
      get
      {
         
        return _countryname;
      }
      set
      {
        _countryname = value;
      }
    }
    private System.String _addressLine1;
    public System.String AddressLine1
    {
      get
      {
        return _addressLine1;
      }
      set
      {
        _addressLine1 = value;
      }
    }
    private System.String _addressLine2;
    public System.String AddressLine2
    {
      get
      {
        return _addressLine2;
      }
      set
      {
        _addressLine2 = value;
      }
    }
    private System.String _addressLine3;
    public System.String AddressLine3
    {
      get
      {
        return _addressLine3;
      }
      set
      {
        _addressLine3 = value;
      }
    }
    private System.String _city;
    public System.String City
    {
      get
      {
        return _city;
      }
      set
      {
        _city = value;
      }
    }
    private System.String _state;
    public System.String State
    {
      get
      {
        return _state;
      }
      set
      {
        _state = value;
      }
    }
    private System.String _postalCode;
    [Size(15)]
    public System.String PostalCode
    {
      get
      {
        return _postalCode;
      }
      set
      {
        _postalCode = value;
      }
    }
    private System.String _region;
    public System.String Region
    {
      get
      {
        return _region;
      }
      set
      {
        _region = value;
      }
    }
  }
}
