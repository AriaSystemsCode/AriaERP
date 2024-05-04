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
  [RelatedEntity("Aria5-SystemAdmin-AddressType")]

  [MapInheritance(MapInheritanceType.ParentTable)]
  [System.ComponentModel.DefaultProperty("Description")]
  public partial class AddressType : Entity
  {
    public AddressType(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
  }
}
