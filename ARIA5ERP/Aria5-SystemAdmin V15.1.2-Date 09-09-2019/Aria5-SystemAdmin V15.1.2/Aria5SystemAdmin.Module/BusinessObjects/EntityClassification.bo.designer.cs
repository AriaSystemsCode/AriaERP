using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Persistent.Validation;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
    public partial class EntityClassification : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.EntityType _entityType;
    private Aria5SystemAdmin.Module.BusinessObjects.EntityClassification _parentClassification;
    private System.String _name;
    private System.String _classificationID;
    public EntityClassification(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String ClassificationID
    {
      get
      {
        return _classificationID;
      }
      set
      {
        SetPropertyValue("ClassificationID", ref _classificationID, value);
      }
    }
    public System.String Name
    {
      get
      {
        return _name;
      }
      set
      {
        SetPropertyValue("Name", ref _name, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityClassifications-ParentClassification")]
    public Aria5SystemAdmin.Module.BusinessObjects.EntityClassification ParentClassification
    {
      get
      {
        return _parentClassification;
      }
      set
      {
        SetPropertyValue("ParentClassification", ref _parentClassification, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityClassifications-ParentClassification")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityClassification> EntityClassifications
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityClassification>("EntityClassifications");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityClassifications-EntityType")]
    public Aria5SystemAdmin.Module.BusinessObjects.EntityType EntityType
    {
      get
      {
        return _entityType;
      }
      set
      {
        SetPropertyValue("EntityType", ref _entityType, value);
      }
    }
    //protected override void OnDeleting()
    //{
    //  if (Session.GetObjectsFromQuery<Entity>("select * from entity where entity.Classification='" + Oid.ToString() + "'") != null)
    //  {
    //    base.OnDeleting();
    //  }
    //  else
    //  {
    //  }
    //  ;
    //}
      //ATA , 2 , can't delete account issue [start]
   // [RuleCriteria("RuleCriteriaObject_Collection_RuleCriteria",DefaultContexts.Delete, @"EntityClassification.Entities.Count() < 1")]
    //ATA , 2 , can't delete account issue [end]
   
    [DevExpress.Xpo.AssociationAttribute("Entities-EntityClassification")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Entity> Entities
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Entity>("Entities");
      }
    }
  }
}
