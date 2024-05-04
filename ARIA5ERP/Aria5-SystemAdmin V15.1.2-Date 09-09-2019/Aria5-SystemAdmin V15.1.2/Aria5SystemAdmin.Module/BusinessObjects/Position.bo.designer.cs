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
  [System.ComponentModel.DefaultProperty("Description")]
    public partial class Position : Entity
  {
    public Position(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
  }
}
