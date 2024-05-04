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
  public partial class DataSort : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.EntityType _entityType;
    private System.String _name;
    private System.String _sortID;
    public DataSort(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String SortID
    {
      get
      {
        return _sortID;
      }
      set
      {
        SetPropertyValue("SortID", ref _sortID, value);
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
    [DevExpress.Xpo.AssociationAttribute("DataSorts-EntityType")]
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
    [DevExpress.Xpo.AssociationAttribute("DataSortColumns-Sort")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.DataSortColumn> DataSortColumns
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.DataSortColumn>("DataSortColumns");
      }
    }
  }
}
