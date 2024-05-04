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
  [MapInheritance(MapInheritanceType.ParentTable)]
  [RelatedEntity("Aria5-SystemAdmin-Region")]

  [System.ComponentModel.DefaultProperty("Description")]
  public partial class Region : Entity
  {
    public Region(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.AssociationAttribute("CountryTSs-Region")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.CountryTS> CountryTSs
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.CountryTS>("CountryTSs");
      }
    }
    //[DevExpress.Xpo.AssociationAttribute("EntityAddresses-Region")]
    //public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityAddress> EntityAddresses
    //{
    //  get
    //  {
    //    return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityAddress>("EntityAddresses");
    //  }
    //}
    //[DevExpress.Xpo.AssociationAttribute("ContactAddresses-Region")]
    //public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ContactAddress> ContactAddresses
    //{
    //  get
    //  {
    //    return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ContactAddress>("ContactAddresses");
    //  }
    //}
  }
}
