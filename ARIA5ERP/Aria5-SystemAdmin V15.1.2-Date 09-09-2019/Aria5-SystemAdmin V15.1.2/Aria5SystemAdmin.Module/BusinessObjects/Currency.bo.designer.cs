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
  [System.ComponentModel.DefaultProperty("Description")]
  [MapInheritance(MapInheritanceType.ParentTable)]
  public partial class Currency : Entity
  {
    public Currency(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.AssociationAttribute("Countries-Currency")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.CountryTS> Countries
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.CountryTS>("Countries");
      }
    }
  }
}
