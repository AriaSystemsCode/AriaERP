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
  public partial class AttachmentCategory : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.AttachmentCategory _parentCategory;
    private System.String _name;
    private System.String _categoryId;
    public AttachmentCategory(DevExpress.Xpo.Session session)
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
    [DevExpress.Xpo.AssociationAttribute("AttachmentCategories-ParentCategory")]
    public Aria5SystemAdmin.Module.BusinessObjects.AttachmentCategory ParentCategory
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
    [DevExpress.Xpo.AssociationAttribute("AttachmentCategories-ParentCategory")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AttachmentCategory> AttachmentCategories
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AttachmentCategory>("AttachmentCategories");
      }
    }
  }
}
