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
    [XafDefaultProperty ("Description")]
    public partial class EntityStatus :  DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.EntityType _entityType;
    private System.String _description;
    private System.String _statusID;
    public EntityStatus(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String StatusID
    {
      get
      {
        return _statusID;
      }
      set
      {
        SetPropertyValue("StatusID", ref _statusID, value);
      }
    }
    public System.String Description
    {
      get
      {
        return _description;
      }
      set
      {
        SetPropertyValue("Description", ref _description, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityStatuses-EntityType")]
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

    [DevExpress.Xpo.AssociationAttribute("Entities-EntityStatus")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Entity> Entities
    {
        get
        {
            return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Entity>("Entities");
        }
    }
  }
}
