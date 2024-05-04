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
namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
  [DefaultClassOptions]
  [IsClient(true, false)]
  [RelatedEntity("Aria5-Windows8Xaml-Ads")]
  [MapInheritance(MapInheritanceType.ParentTable)]

    public partial class ADS : ClientEntity
  {
    private System.String _subject;
    public ADS(Session session)
      : base(session)
    {
    }
    [NonPersistentAttribute]
    public System.String Subject
    {
      get
      {
        return _subject;
      }
      set
      {
        SetPropertyValue("Subject", ref _subject, value);
      }
    }
    public XPCollection<ClientEntitiesRelationship> Entities
    {
      get
      {
          XPCollection<ClientEntitiesRelationship> tmpEntities = new XPCollection<ClientEntitiesRelationship>(Session);
        tmpEntities.Criteria = CriteriaOperator.Parse("[RelatedEntity] = '" + this.Oid.ToString() + "'");
        return tmpEntities;
      }
    }
    public override void AfterConstruction()
    {
        this.Type = Session.FindObject<ClientEntityType>(CriteriaOperator.Parse("[TypeId] = 'ADS'"));
      base.AfterConstruction();
    }
  }
}
