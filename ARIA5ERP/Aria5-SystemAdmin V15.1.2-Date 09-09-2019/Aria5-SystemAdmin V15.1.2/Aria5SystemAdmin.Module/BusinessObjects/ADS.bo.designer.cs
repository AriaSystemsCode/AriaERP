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
  [RelatedEntity("Aria5-Windows8Xaml-Ads")]
  [DivisionAttribute(true)]
  public partial class ADS : Aria5SystemAdmin.Module.BusinessObjects.Entity
  {
    private System.String _subject;
    public ADS(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.NonPersistentAttribute]
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
    public XPCollection<EntityRelationship> Entities
    {
      get
      {
        XPCollection<EntityRelationship> tmpEntities = new XPCollection<EntityRelationship>(Session);
        tmpEntities.Criteria = CriteriaOperator.Parse("[RelatedEntity] = '" + Oid.ToString() + "'");
        return tmpEntities;
      }
    }
    public override void AfterConstruction()
    {
      EntityType = Session.FindObject<EntityType>(CriteriaOperator.Parse("[TypeId] = 'ADS'"));
      base.AfterConstruction();
    }
  }
}
