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
  [RelatedEntity("Aria5-SystemAdmin-ApplicationCategory")]
  public partial class ApplicationCategory : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.ApplicationCategory _parentCategoryId;
    private System.String _name;
    private System.String _categoryId;
    public ApplicationCategory(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(50)]
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
    public Aria5SystemAdmin.Module.BusinessObjects.ApplicationCategory ParentCategoryId
    {
      get
      {
        return _parentCategoryId;
      }
      set
      {
        SetPropertyValue("ParentCategoryId", ref _parentCategoryId, value);
      }
    }
  }
}
