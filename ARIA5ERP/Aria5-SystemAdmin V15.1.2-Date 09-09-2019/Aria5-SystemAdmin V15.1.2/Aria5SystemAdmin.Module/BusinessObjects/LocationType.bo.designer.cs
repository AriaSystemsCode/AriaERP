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
  public partial class LocationType : Aria5SystemAdmin.Module.BusinessObjects.Entity
  {
    public LocationType(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private System.String _Name;
    public System.String Name
    {
      get
      {
        return _Name;
      }
      set
      {
        SetPropertyValue("Name", ref _Name, value);
      }
    }
  }
}
