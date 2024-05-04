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
  [RelatedEntity("Aria5-Windows8Xaml-Events")]
  [MapInheritance(MapInheritanceType.ParentTable)]
    public partial class Event  : ClientEntity
  {
    private System.DateTime _startTime;
    private System.String _article;
    private System.DateTime _eventDate = DateTime.Today;
    private System.String _subject;
    public Event(Session session)
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
    [NonPersistentAttribute]
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
    [SizeAttribute(-1)]
    [NonPersistentAttribute]
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
        this.Type = Session.FindObject<ClientEntityType>(CriteriaOperator.Parse("[TypeId] = 'EVENTS'"));
      base.AfterConstruction();
    }
  
    [NonPersistentAttribute]
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

    public XPCollection<ClientEntitiesRelationship> Entities
    {
        get
        {
            XPCollection<ClientEntitiesRelationship> tmpEntities = new XPCollection<ClientEntitiesRelationship>(Session);
            tmpEntities.Criteria = CriteriaOperator.Parse("[RelatedEntity] = '" + this.Oid.ToString() + "'");
            return tmpEntities;
        }
    }
  }
}
