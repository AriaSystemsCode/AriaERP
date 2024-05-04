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
  [RelatedEntity("Aria5-Windows8Xaml-Events")]
  public partial class Events : Aria5SystemAdmin.Module.BusinessObjects.Entity
  {
    private System.DateTime _startTime;
    private System.String _article;
    private System.DateTime _eventDate = DateTime.Today;
    private System.String _subject;
    public Events(DevExpress.Xpo.Session session)
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
    [DevExpress.Xpo.NonPersistentAttribute]
    public System.DateTime EventDate
    {
      get
      {
        return _eventDate;
      }
      set
      {
        SetPropertyValue("EventDate", ref _eventDate, value);
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
      EntityType = Session.FindObject<EntityType>(CriteriaOperator.Parse("[TypeId] = 'EVENTS'"));
      base.AfterConstruction();
    }
  
    [DevExpress.Xpo.NonPersistentAttribute]
    public System.DateTime StartTime
    {
      get
      {
        return _startTime;
      }
      set
      {
        SetPropertyValue("StartTime", ref _startTime, value);
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
  }
}
