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
  public partial class EntityAttachment : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.Drawing.Image _image;
    private Aria5SystemAdmin.Module.BusinessObjects.Entity _entity;
    private Aria5SystemAdmin.Module.BusinessObjects.Attachment _attachment;
    private System.Boolean _isDefault;
    private Aria5SystemAdmin.Module.BusinessObjects.AttachmentCategory _category;
    public EntityAttachment(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public System.Boolean IsDefault
    {
      get
      {
        return _isDefault;
      }
      set
      {
        SetPropertyValue("IsDefault", ref _isDefault, value);
      }
    }
    public Aria5SystemAdmin.Module.BusinessObjects.AttachmentCategory Category
    {
      get
      {
        return _category;
      }
      set
      {
        SetPropertyValue("Category", ref _category, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityAttachments-Attachment")]
    [ImmediatePostData]
    public Aria5SystemAdmin.Module.BusinessObjects.Attachment Attachment
    {
      get
      {
        return _attachment;
      }
      set
      {
        SetPropertyValue("Attachment", ref _attachment, value);
        if (!IsLoading && !IsSaving && this.AttachmentID == null)
        {
            this.AttachmentID = value.Name;
            this.Name = value.Name;
        }
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityAttachments-Entity")]
    public Aria5SystemAdmin.Module.BusinessObjects.Entity Entity
    {
      get
      {
        return _entity;
      }
      set
      {
        SetPropertyValue("Entity", ref _entity, value);
      }
    }
    // Set default image for the entity
    [Action(ToolTip = "Set As Default Image")]
    public void SetAsDefaultImage()
    {
      // Wael 
      // 1. Only one attachment should be checked
      // 2. The checked attachment should be of type Image 
      // Update the field DefaultAttachment in the Entity table with the OID of the selected attachment
      Entity relatedEntity = Session.FindObject<Entity>(CriteriaOperator.Parse("[Oid] = '" + Entity.Oid + "'"));
      relatedEntity.DefaultAttachment = Attachment;
      relatedEntity.IMAGE = Attachment.AttachmentFile;
      relatedEntity.Save();
      for (int i = 1; i <= relatedEntity.EntityAttachments.Count; i++)
      {
        relatedEntity.EntityAttachments[i - 1].IsDefault = false;
      }
      relatedEntity.Save();
      IsDefault = true;
      Save();
    }
    [DevExpress.Xpo.ValueConverterAttribute(typeof(DevExpress.Xpo.Metadata.ImageValueConverter))]
    [DevExpress.Xpo.NonPersistentAttribute]
    public System.Drawing.Image Image
    {
      get
      {
        try
        {
          return Attachment.AttachmentFile;
        }
        catch (Exception ex)
        {
          return _image;
        }
      }
    }
    private System.String _CategoryId;
    public System.String CategoryId
    {
      get
      {
        return _CategoryId;
      }
      set
      {
        SetPropertyValue("CategoryId", ref _CategoryId, value);
      }
    }
    private System.String _Name;
    public System.String Name
    {
      get
      {
        return _Name;
      }
      set
      {
        SetPropertyValue("Name", ref _Name, value);
      }
    }
    private System.String _AttachmentID;
    public System.String AttachmentID
    {
      get
      {
        return _AttachmentID;
      }
      set
      {
        SetPropertyValue("AttachmentID", ref _AttachmentID, value);
      }
    }
  }
}
