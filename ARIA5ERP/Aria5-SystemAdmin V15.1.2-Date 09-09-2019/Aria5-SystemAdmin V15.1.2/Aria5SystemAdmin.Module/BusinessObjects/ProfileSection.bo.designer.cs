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
  public partial class ProfileSection : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.Profile _profile;
    private Aria5SystemAdmin.Module.BusinessObjects.EntityCategory _entityCategory;
    private Aria5SystemAdmin.Module.BusinessObjects.EntityType _entityType;
    private System.String _order;
    private System.String _name;
    private System.String _sectionId;
    public ProfileSection(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public System.String SectionId
    {
      get
      {
        return _sectionId;
      }
      set
      {
        SetPropertyValue("SectionId", ref _sectionId, value);
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
    public System.String Order
    {
      get
      {
        return _order;
      }
      set
      {
        SetPropertyValue("Order", ref _order, value);
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
        if (IsLoading == false)
        {
          this.EntityCategory = null;
        }
      }
    }
    [DataSourceProperty("Categories")]
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
    [DevExpress.Xpo.AssociationAttribute("ProfileSectionTiles-ProfileSection")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ProfileSectionTile> ProfileSectionTiles
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ProfileSectionTile>("ProfileSectionTiles");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ProfileSections-Profile")]
    public Aria5SystemAdmin.Module.BusinessObjects.Profile Profile
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
    public XPCollection<EntityCategory> Categories
    {
      get
      {
        XPCollection<EntityCategory> _categories = new XPCollection<EntityCategory>(Session);
        if (this.EntityType != null)
        {
          _categories = this.EntityType.EntityCategories;
        }
        return _categories;
      }
    }

    //private DataFilter _Filter;
    //public DataFilter Filter
    //{
    //    get
    //    {
    //        return _Filter;
    //    }
    //    set
    //    {
    //        SetPropertyValue("Filter", ref _Filter, value);
    //    }
    //}

  }
}
