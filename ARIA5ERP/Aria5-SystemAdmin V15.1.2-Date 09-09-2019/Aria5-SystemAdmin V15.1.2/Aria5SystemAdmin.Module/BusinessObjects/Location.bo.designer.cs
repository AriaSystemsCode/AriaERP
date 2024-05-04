using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  public partial class Location : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.Business _business;
    private Aria5SystemAdmin.Module.BusinessObjects.Entity _entity;
    private Aria5SystemAdmin.Module.BusinessObjects.Location _relativeToLocation;
    //private Aria5SystemAdmin.Module.BusinessObjects.//Profile _profile;
    private Aria5SystemAdmin.Module.BusinessObjects.Location _parentLocation;
  //  private Aria5SystemAdmin.Module.BusinessObjects.LocationType _locationType;
    private Aria5SystemAdmin.Module.BusinessObjects.EntityCategory _entityCategory;
    private Aria5SystemAdmin.Module.BusinessObjects.EntityType _entityType;
    private System.Decimal _yCoordinate;
    private System.Decimal _xCoordinate;
    private System.Int32 _latitude;
    private System.Int32 _longitude;
    private System.String _address;
    private System.String _description;
    private System.String _name;
    private System.String _locationID;
    public Location(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(30)]
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
    //public Aria5SystemAdmin.Module.BusinessObjects.LocationType LocationType
    //{
    //  get
    //  {
    //    return _locationType;
    //  }
    //  set
    //  {
    //    SetPropertyValue("LocationType", ref _locationType, value);
    //  }
    //}
    [DevExpress.Xpo.SizeAttribute(200)]
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
    [DevExpress.Xpo.AssociationAttribute("Locations-Location")]
    public Aria5SystemAdmin.Module.BusinessObjects.Location ParentLocation
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
    //public Aria5SystemAdmin.Module.BusinessObjects.Profile Profile
    //{
    //  get
    //  {
    //    return _profile;
    //  }
    //  set
    //  {
    //    SetPropertyValue("Profile", ref _profile, value);
    //  }
    //}
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
    [DevExpress.Xpo.SizeAttribute(9)]
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
    public Aria5SystemAdmin.Module.BusinessObjects.Location RelativeToLocation
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
    public Aria5SystemAdmin.Module.BusinessObjects.EntityType EntityType
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
    public Aria5SystemAdmin.Module.BusinessObjects.EntityCategory EntityCategory
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
    [DevExpress.Xpo.AssociationAttribute("Locations-Location")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Location> Locations
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Location>("Locations");
      }
    }
    [DataSourceProperty("EntityFiltered")]
    public Aria5SystemAdmin.Module.BusinessObjects.Entity Entity
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
    public XPCollection<EntityCategory> EntityCategoryFiltered
    {
      get
      {
        XPCollection<EntityCategory> EntityCategoryTmp = new XPCollection<EntityCategory>(Session);
        if (this.EntityType != null)
        {
          EntityCategoryTmp.Criteria = CriteriaOperator.Parse("[EntityType] = '" + this.EntityType.Oid.ToString() + "'");
        }
        return EntityCategoryTmp;
      }
    }
    public XPCollection<Entity> EntityFiltered
    {
      get
      {
        XPCollection<Entity> EntityFilteredTmp = new XPCollection<Entity>(Session);
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
    [DevExpress.Xpo.AssociationAttribute("Locations-Business")]
    public Aria5SystemAdmin.Module.BusinessObjects.Business Business
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
