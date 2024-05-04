using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using Aria5SystemAdmin.Module;
 
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [RelatedEntity("Aria5-Windows8Xaml-Business")]
  [DivisionAttribute(true)]
  public partial class Shopping : Aria5SystemAdmin.Module.BusinessObjects.Business
  {
    public Shopping(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public override void AfterConstruction()
    {
        this.EntityType = Session.FindObject<EntityType>(CriteriaOperator.Parse("[TypeId] = 'SHOPPING'"));
        base.AfterConstruction();
    }
  }
}
