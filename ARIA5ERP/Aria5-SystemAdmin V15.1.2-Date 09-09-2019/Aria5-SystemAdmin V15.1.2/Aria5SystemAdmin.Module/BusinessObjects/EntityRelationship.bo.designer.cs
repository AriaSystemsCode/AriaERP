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
    public partial class EntityRelationship : Aria5SystemAdmin.Module.BaseObject1// DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.Entity _entity;
    private Aria5SystemAdmin.Module.BusinessObjects.Entity _relatedEntity;
    private Aria5SystemAdmin.Module.BusinessObjects.EntityType _relatedEntityType;
    private Aria5SystemAdmin.Module.BusinessObjects.EntityType _entityType;
    public EntityRelationship(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
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
    public Aria5SystemAdmin.Module.BusinessObjects.EntityType RelatedEntityType
    {
      get
      {
        return _relatedEntityType;
      }
      set
      {
        SetPropertyValue("RelatedEntityType", ref _relatedEntityType, value);
      }
    }
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public Aria5SystemAdmin.Module.BusinessObjects.Entity RelatedEntity
    {
      get
      {
        return _relatedEntity;
      }
      set
      {
        SetPropertyValue("RelatedEntity", ref _relatedEntity, value);
        if (IsLoading == false && _relatedEntity != null)
        {
          RelatedEntityType = _relatedEntity.EntityType;
        }
        ;
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityRelationships-Entity")]
    public Aria5SystemAdmin.Module.BusinessObjects.Entity Entity
    {
      get
      {
        return _entity;
      }
      set
      {
        SetPropertyValue("Entity", ref _entity, value);
        if (IsLoading == false && _entity != null)
        {
          EntityType = _entity.EntityType;
        }
        ;
      }
    }

    private System.String _Description;
    public System.String Description
    {
        get
        {
            return _Description;
        }
        set
        {
            SetPropertyValue("Description", ref _Description, value);
        }
    }

    private System.String _RelatedDescription;
    public System.String RelatedDescription
    {
        get
        {
            return _RelatedDescription;
        }
        set
        {
            SetPropertyValue("RelatedDescription", ref _RelatedDescription, value);
        }
    }

  }
}
