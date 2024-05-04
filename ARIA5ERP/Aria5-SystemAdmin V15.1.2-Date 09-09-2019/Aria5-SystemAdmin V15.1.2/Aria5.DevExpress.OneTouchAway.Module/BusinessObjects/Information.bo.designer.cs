using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using Aria5.DevExpress.OneTouchAway.Module.BusinessObjects;
using Aria5SystemAdmin.Module;
namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
  [DefaultClassOptions]
  [IsClient(true, false)]
  [RelatedEntity("Aria5-Windows8Xaml-Information")]
  [MapInheritance(MapInheritanceType.ParentTable)]
  [DivisionAttribute(true)]
    public partial class Information : ClientEntity
  {
    private System.String _article;
    private System.String _subject;
    public Information(Session session)
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
  }
}
