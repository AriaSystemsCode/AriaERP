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
  [RelatedEntity("Aria5-Windows8Xaml-Information")]
  [DivisionAttribute(true)]
  public partial class Information : Aria5SystemAdmin.Module.BusinessObjects.Entity
  {
    private System.String _article;
    private System.String _subject;
    public Information(DevExpress.Xpo.Session session)
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
    [DevExpress.Xpo.SizeAttribute(-1)]
    [DevExpress.Xpo.NonPersistentAttribute]
    public System.String Article
    {
      get
      {
        return _article;
      }
      set
      {
        SetPropertyValue("Article", ref _article, value);
      }
    }
    public override void AfterConstruction()
    {
      this.EntityType = Session.FindObject<EntityType>(CriteriaOperator.Parse("[TypeId] = 'INFORMATION'"));
      base.AfterConstruction();
    }


    public XPCollection<EntityRelationship> Entities
    {
        get
        {
            XPCollection<EntityRelationship> tmpEntities = new XPCollection<EntityRelationship>(Session);
            tmpEntities.Criteria = CriteriaOperator.Parse("[RelatedEntity] = '" + this.Oid.ToString() + "'");
            return tmpEntities;
        }
    }
  }
}
