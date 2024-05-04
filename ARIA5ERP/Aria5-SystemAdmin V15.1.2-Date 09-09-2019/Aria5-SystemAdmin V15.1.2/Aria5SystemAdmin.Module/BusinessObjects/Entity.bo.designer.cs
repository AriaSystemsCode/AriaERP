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
  //[DevExpress.Xpo.NonPersistentAttribute]
  [DefaultClassOptions]
  [DevExpress.ExpressApp.DC.XafDefaultPropertyAttribute("Id")]
    [RelatedEntity("Aria5-SystemAdmin-Entity")]
  public partial class Entity :DevExpress.Persistent.BaseImpl.BaseObject 
  {
    private System.Guid _division;
    private Aria5SystemAdmin.Module.BusinessObjects.EntityCategory _entityCategory;
    private System.Drawing.Image _iMAGE;
    private System.String _statusId;
    private System.String _categoryId;
    private System.String _classificationId;
    private System.String _typeId;
    private Aria5SystemAdmin.Module.BusinessObjects.EntityType _entityType;
    private Aria5SystemAdmin.Module.BusinessObjects.EntityClassification _entityClassification;
    private Attachment _defaultAttachment;
    private System.String _notes;
    private System.String _id;
    private System.DateTime _enteredDate;
    private System.String _extraData;
    private System.String _enteredBy;
    private System.String _description;
    public Entity(DevExpress.Xpo.Session session) 
      : base(session)
    {
    }
    public System.String Id
    {
      get
      {
        return _id;
      }
      set
      {
        SetPropertyValue("Id", ref _id, value);
      }
    }
    [ImmediatePostData]
    [DataSourceProperty("EntityStatusFiltered")]
    [DevExpress.Xpo.AssociationAttribute("Entities-EntityStatus")]
    public EntityStatus Status
    {
      get
      {
        return entitystatus;
      }
      set
      {
        SetPropertyValue("Status", ref entitystatus, value);
        if (entitystatus != null)
        {
          StatusId = entitystatus.StatusID;
        }
      }
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String StatusId
    {
      get
      {
        return _statusId;
      }
      set
      {
        SetPropertyValue("StatusId", ref _statusId, value);
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
    public System.String EnteredBy
    {
      get
      {
        return _enteredBy;
      }
      set
      {
        SetPropertyValue("EnteredBy", ref _enteredBy, value);
      }
    }
    public System.DateTime EnteredDate
    {
      get
      {
        return _enteredDate;
      }
      set
      {
        SetPropertyValue("EnteredDate", ref _enteredDate, value);
      }
    }
    [Size(SizeAttribute.Unlimited)]
    public System.String Notes
    {
      get
      {
        return _notes;
      }
      set
      {
        SetPropertyValue("Notes", ref _notes, value);
      }
    }
    [Size(SizeAttribute.Unlimited)]
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    public System.String ExtraData
    {
      get
      {
        return _extraData;
      }
      set
      {
        SetPropertyValue("ExtraData", ref _extraData, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityAddresses-Entity")]
    public XPCollection<EntityAddress> EntityAddresses
    {
      get
      {
        return GetCollection<EntityAddress>("EntityAddresses");
      }
    }
    private EntityStatus entitystatus;
    [ImmediatePostData]
    public Attachment DefaultAttachment
    {
      get
      {
        return _defaultAttachment;
      }
      set
      {
        SetPropertyValue("DefaultAttachment", ref _defaultAttachment, value);
      }
    }
    [ImmediatePostData]
    [DevExpress.Xpo.AssociationAttribute("Entities-EntityType")]
    public Aria5SystemAdmin.Module.BusinessObjects.EntityType EntityType
    {
      get
      {
        return _entityType;
      }
      set
      {
        SetPropertyValue("EntityType", ref _entityType, value);
        if (_entityType != null)
        {
          TypeId = _entityType.TypeId;
        }
      }
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String TypeId
    {
      get
      {
        return _typeId;
      }
      set
      {
        SetPropertyValue("TypeId", ref _typeId, value);
      }
    }
    [ImmediatePostData]
    [DevExpress.Xpo.AssociationAttribute("Entities-EntityClassification")]
    [DataSourceProperty("EntityClassificationFiltered")]
    public Aria5SystemAdmin.Module.BusinessObjects.EntityClassification EntityClassification
    {
      get
      {
        return _entityClassification;
      }
      set
      {
        SetPropertyValue("EntityClassification", ref _entityClassification, value);
        if (_entityClassification != null)
        {
          ClassificationId = _entityClassification.ClassificationID;
        }
      }
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String ClassificationId
    {
      get
      {
        return _classificationId;
      }
      set
      {
        SetPropertyValue("ClassificationId", ref _classificationId, value);
      }
    }
    [ImmediatePostData]
    [DevExpress.Xpo.AssociationAttribute("Entity-EntityCategory")]
    [DataSourceProperty("EntityCategoryFiltered")]
    public Aria5SystemAdmin.Module.BusinessObjects.EntityCategory EntityCategory
    {
      get
      {
        return _entityCategory;
      }
      set
      {
        SetPropertyValue("EntityCategory", ref _entityCategory, value);
        if (_entityCategory != null)
        {
          CategoryId = _entityCategory.CategoryId;
        }
      }
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String CategoryId
    {
      get
      {
        return _categoryId;
      }
      set
      {
        SetPropertyValue("CategoryId", ref _categoryId, value);
      }
    }
    public XPCollection<EntityClassification> EntityClassificationFiltered
    {
      get
      {
        XPCollection<EntityClassification> EntityClassificationTmp = new XPCollection<EntityClassification>(Session);
        if (EntityType != null)
        {
          EntityClassificationTmp.Criteria = CriteriaOperator.Parse("[EntityType] = '" + EntityType.Oid.ToString() + "'");
        }
        return EntityClassificationTmp;
      }
    }
    public XPCollection<EntityStatus> EntityStatusFiltered
    {
      get
      {
        XPCollection<EntityStatus> EntityStatusTmp = new XPCollection<EntityStatus>(Session);
        if (EntityType != null)
        {
          EntityStatusTmp.Criteria = CriteriaOperator.Parse("[EntityType] = '" + EntityType.Oid.ToString() + "'");
        }
        return EntityStatusTmp;
      }
    }
    public XPCollection<EntityCategory> EntityCategoryFiltered
    {
      get
      {
        XPCollection<EntityCategory> EntityCategoryTmp = new XPCollection<EntityCategory>(Session);
        if (EntityType != null)
        {
          EntityCategoryTmp.Criteria = CriteriaOperator.Parse("[EntityType] = '" + EntityType.Oid.ToString() + "'");
        }
        return EntityCategoryTmp;
      }
    }
    [DevExpress.Xpo.ValueConverterAttribute(typeof(DevExpress.Xpo.Metadata.ImageValueConverter))]
    [DevExpress.Xpo.NonPersistentAttribute]
    public System.Drawing.Image IMAGE
    {
      get
      {
        return _iMAGE;
      }
      set
      {
        SetPropertyValue("IMAGE", ref _iMAGE, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityAttachments-Entity")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityAttachment> EntityAttachments
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityAttachment>("EntityAttachments");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityRelationships-Entity")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityRelationship> EntityRelationships
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityRelationship>("EntityRelationships");
      }
    }
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityRelationship> ADS
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityRelationship>("EntityRelationships");
      }
    }
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityRelationship> Events
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityRelationship>("EntityRelationships");
      }
    }
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityRelationship> Information
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityRelationship>("EntityRelationships");
      }
    }
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityRelationship> News
    {
      get
      {
          return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityRelationship>("EntityRelationships");
      }
    }
    public System.Guid Division
    {
      get
      {
        return _division;
      }
      set
      {
        SetPropertyValue("Division", ref _division, value);
      }
    }

     //ATA Add notes table insetad of being field [Start]
    [Association("Entity-Notes")]
    public XPCollection<Notes> NotesList
    {
        get
        {
            return GetCollection<Notes>("NotesList");
        }
    }
      //ATA Add notes table insetad of being field [End]

    
  }
}
