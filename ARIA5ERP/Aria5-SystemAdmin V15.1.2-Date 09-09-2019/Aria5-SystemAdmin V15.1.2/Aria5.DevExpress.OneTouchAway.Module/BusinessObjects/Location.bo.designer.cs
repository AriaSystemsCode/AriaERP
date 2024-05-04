using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using Aria5SystemAdmin.Module;
namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
    [DefaultClassOptions]
    [IsClient(true, true)]
    [RelatedEntity("Aria5-Windows8Xaml-Location")]
  public partial class ClientLocation : ClientBaseObject
  {
    private Business _business;
    private ClientEntity _entity;
    private ClientLocation _relativeToLocation;
   private Profile _profile;
    private ClientLocation _parentLocation;
  private ClientLocationType _locationType;
    private ClientEntityCategory _entityCategory;
    private ClientEntityType _entityType;
    private System.Decimal _yCoordinate;
    private System.Decimal _xCoordinate;
    private System.Int32 _latitude;
    private System.Int32 _longitude;
    private System.String _address;
    private System.String _description;
    private System.String _name;
    private System.String _locationID;
    public ClientLocation(Session session)
      : base(session)
    {
    }
    [SizeAttribute(30)]
    public System.String LocationID
    {
      get
      {
        return _locationID;
      }
      set
      {
        SetPropertyValue("LocationID", ref _locationID, value);
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
    public ClientLocationType LocationType
    {
        get
        {
            return _locationType;
        }
        set
        {
            SetPropertyValue("LocationType", ref _locationType, value);
        }
    }
    [SizeAttribute(200)]
    public System.String Address
    {
      get
      {
        return _address;
      }
      set
      {
        SetPropertyValue("Address", ref _address, value);
      }
    }
    public System.String Description
    {
      get
      {
        return _description;
      }
      set
      {
        SetPropertyValue("Description", ref _description, value);
      }
    }
    [ImmediatePostData]
    [AssociationAttribute("Locations-Location")]
    public ClientLocation ParentLocation
    {
      get
      {
        return _parentLocation;
      }
      set
      {
        SetPropertyValue("ParentLocation", ref _parentLocation, value);
        // By Default, a location inherit the profile assigned to the parent location
        //if (_parentLocation != null)
        //{
        //    this.Profile = _parentLocation.Profile;
        //};
      }
    }
    public Profile Profile
    {
        get
        {
            return _profile;
        }
        set
        {
            SetPropertyValue("Profile", ref _profile, value);
        }
    }
    public System.Int32 Longitude
    {
      get
      {
        return _longitude;
      }
      set
      {
        SetPropertyValue("Longitude", ref _longitude, value);
      }
    }
    public System.Int32 Latitude
    {
      get
      {
        return _latitude;
      }
      set
      {
        SetPropertyValue("Latitude", ref _latitude, value);
      }
    }
    [SizeAttribute(9)]
    public System.Decimal XCoordinate
    {
      get
      {
        return _xCoordinate;
      }
      set
      {
        SetPropertyValue("XCoordinate", ref _xCoordinate, value);
      }
    }
    public System.Decimal YCoordinate
    {
      get
      {
        return _yCoordinate;
      }
      set
      {
        SetPropertyValue("YCoordinate", ref _yCoordinate, value);
      }
    }

    [ImmediatePostData]
    public ClientLocation RelativeToLocation
    {
      get
      {
        return _relativeToLocation;
      }
      set
      {
        SetPropertyValue("RelativeToLocation", ref _relativeToLocation, value);
        if (_relativeToLocation == null)
        {
            SetPropertyValue("YCoordinate", ref _yCoordinate, 0);
            SetPropertyValue("XCoordinate", ref _xCoordinate, 0); 
        }
      }
    }
    [ImmediatePostData]
    public ClientEntityType EntityType
    {
      get
      {
        return _entityType;
      }
      set
      {
        SetPropertyValue("EntityType", ref _entityType, value);
      }
    }

    [DataSourceProperty("EntityCategoryFiltered")]
    [ImmediatePostData]
    public ClientEntityCategory EntityCategory
    {
      get
      {
        return _entityCategory;
      }
      set
      {
        SetPropertyValue("EntityCategory", ref _entityCategory, value);
      }
    }
    [AssociationAttribute("Locations-Location")]
    public XPCollection<ClientLocation> Locations
    {
      get
      {
        return GetCollection<ClientLocation>("Locations");
      }
    }
    [DataSourceProperty("EntityFiltered")]
    public ClientEntity Entity
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
    public XPCollection<ClientEntityCategory> EntityCategoryFiltered
    {
      get
      {
          XPCollection<ClientEntityCategory> EntityCategoryTmp = new XPCollection<ClientEntityCategory>(Session);
        if (this.EntityType != null)
        {
          EntityCategoryTmp.Criteria = CriteriaOperator.Parse("[EntityType] = '" + this.EntityType.Oid.ToString() + "'");
        }
        return EntityCategoryTmp;
      }
    }
    public XPCollection<ClientEntity> EntityFiltered
    {
      get
      {
          XPCollection<ClientEntity> EntityFilteredTmp = new XPCollection<ClientEntity>(Session);
        string whereString = "";

        if (this.EntityType != null)
        {

            whereString = "[EntityType] = '" + this.EntityType.Oid.ToString() + "'";

        }

        if (this.EntityCategory != null)
        {
            if (whereString!="" ){whereString = whereString + "And "; };
            whereString = whereString + " [EntityCategory]='" + this.EntityCategory.Oid.ToString() + "'";
        }



        if (String.IsNullOrEmpty(whereString)==false)
        {

            EntityFilteredTmp.Criteria = CriteriaOperator.Parse(whereString);
        }


        return EntityFilteredTmp;
      }
    }
    //[AssociationAttribute("Locations-Business")]
    public Business Business
    {
      get
      {
        return _business;
      }
      set
      {
        SetPropertyValue("Business", ref _business, value);
      }
    }
  }
}
