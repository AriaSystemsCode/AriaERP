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
  public partial class DataFilter : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.EntityType _entityType;
    private System.String _name;
    private System.String _filterId;
    public DataFilter(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String FilterId
    {
      get
      {
        return _filterId;
      }
      set
      {
        SetPropertyValue("FilterId", ref _filterId, value);
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
    //[DevExpress.Xpo.AssociationAttribute("DataFilters-EntityType")]
    //public Aria5SystemAdmin.Module.BusinessObjects.EntityType EntityType
    //{
    //  get
    //  {
    //    return _entityType;
    //  }
    //  set
    //  {
    //    SetPropertyValue("EntityType", ref _entityType, value);
    //  }
    //}
    //[DevExpress.Xpo.AssociationAttribute("DataFilterColumns-Filter")]
    //public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.DataFilterColumn> DataFilterColumns
    //{
    //  get
    //  {
    //    return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.DataFilterColumn>("DataFilterColumns");
    //  }
    //}
  }
}
