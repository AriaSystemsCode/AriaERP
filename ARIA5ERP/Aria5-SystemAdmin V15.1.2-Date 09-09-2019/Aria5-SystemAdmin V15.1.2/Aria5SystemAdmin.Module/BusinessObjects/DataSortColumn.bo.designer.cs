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
  public partial class DataSortColumn : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.DataSort _sort;
    private System.String _direction;
    private System.String _name;
    private System.Int32 _order;
    public DataSortColumn(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public System.Int32 Order
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
    [DevExpress.Xpo.SizeAttribute(10)]
    public System.String Direction
    {
      get
      {
        return _direction;
      }
      set
      {
        SetPropertyValue("Direction", ref _direction, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("DataSortColumns-Sort")]
    public Aria5SystemAdmin.Module.BusinessObjects.DataSort Sort
    {
      get
      {
        return _sort;
      }
      set
      {
        SetPropertyValue("Sort", ref _sort, value);
      }
    }
  }
}
