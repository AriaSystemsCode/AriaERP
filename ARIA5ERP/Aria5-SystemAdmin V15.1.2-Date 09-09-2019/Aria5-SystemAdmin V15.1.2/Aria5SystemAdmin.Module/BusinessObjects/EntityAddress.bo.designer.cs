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
  public partial class EntityAddress : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.CountryTS _countryTS;
    private System.String _region;
    private System.String _country;
    private System.String _postalCode;
    private System.String _state;
    private System.String _city;
    private System.String _addressLine3;
    private System.String _addressLine2;
    private System.String _addressLine1;
    private System.String _name;
    private Entity _entity;
    public EntityAddress(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.AssociationAttribute("EntityAddresses-Entity")]
    public Entity Entity
    {
      get
      {
        return _entity;
      }
      set
      {
        SetPropertyValue("Entity", ref _entity, value);
      }
    }
    private AddressType addressType;
    public AddressType AddressType
    {
      get
      {
        return addressType;
      }
      set
      {
        SetPropertyValue("AddressType", ref addressType, value);
      }
    }
    private Address address;
    public Address Address
    {
      get
      {
        return address;
      }
      set
      {
        SetPropertyValue("Address", ref address, value);
        try
        {
          Name = address.Name;
          AddressLine1 = address.AddressLine1;
          AddressLine2 = address.AddressLine2;
          AddressLine3 = address.AddressLine3;
          City = address.City;
          State = address.State;
          PostalCode = address.PostalCode;
          Country = address.Country;
        }
        catch (Exception ex)
        {
        }
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
    public System.String Country
    {
      get
      {
        return _country;
      }
      set
      {
        SetPropertyValue("Country", ref _country, value);
      }
    }
    public System.String Region
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
    [DevExpress.Xpo.AssociationAttribute("EntityAddresses-CountryTS")]
    public Aria5SystemAdmin.Module.BusinessObjects.CountryTS CountryTS
    {
      get
      {
        return _countryTS;
      }
      set
      {
        SetPropertyValue("CountryTS", ref _countryTS, value);
      }
    }
  }
}
