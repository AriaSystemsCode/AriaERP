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
  public partial class EntityCategory : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.EntityType _entityType;
    private Aria5SystemAdmin.Module.BusinessObjects.EntityCategory _parentCategory;
    private System.String _name;
    private System.String _categoryId;
    public EntityCategory(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String CategoryId
    {
      get
      {
        return _categoryId;
      }
      set
      {
        SetPropertyValue("CategoryId", ref _categoryId, value);
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
    [DevExpress.Xpo.AssociationAttribute("EntityCategories-ParentCategory")]
    public Aria5SystemAdmin.Module.BusinessObjects.EntityCategory ParentCategory
    {
      get
      {
        return _parentCategory;
      }
      set
      {
        SetPropertyValue("ParentCategory", ref _parentCategory, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityCategories-ParentCategory")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityCategory> EntityCategories
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityCategory>("EntityCategories");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityCategories-EntityType")]
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
    //protected override void OnDeleting()
    //{
    //  if (Session.GetObjectsFromQuery<Entity>("select * from entity where entity.Category='" + Oid.ToString() + "'") != null)
    //  {
    //    base.OnDeleting();
    //  }
    //  else
    //  {
    //  }
    //  ;
    //}
    [DevExpress.Xpo.AssociationAttribute("Entity-EntityCategory")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Entity> Entities
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Entity>("Entities");
      }
    }
  }
}
