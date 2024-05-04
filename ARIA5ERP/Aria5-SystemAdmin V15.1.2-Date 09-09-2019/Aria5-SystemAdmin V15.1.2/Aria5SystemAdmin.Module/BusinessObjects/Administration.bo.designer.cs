using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-Administration")]

  public partial class Administration : Aria5SystemAdmin.Module.BusinessObjects.Person
  {
    public Administration(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public override void AfterConstruction()
    {
      EntityType = Session.FindObject<EntityType>(CriteriaOperator.Parse("[TypeId] = 'PERSON'"));
      base.AfterConstruction();
    }
  }
}
