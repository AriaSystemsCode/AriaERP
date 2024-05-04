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
  public partial class TrackingEntryAttachment : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.String _notes;
    private Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry _trackingEntry;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObject _ariaObject;
    private System.String _objectRevision;
    private System.String _objectName;
    private System.Int32 _objectID;
    private Aria5SystemAdmin.Module.BusinessObjects.ObjectType _objectType;
    public TrackingEntryAttachment(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public System.Int32 ObjectID
    {
      get
      {
        return _objectID;
      }
      set
      {
        SetPropertyValue("ObjectID", ref _objectID, value);
      }
    }
    public System.String ObjectName
    {
      get
      {
        return _objectName;
      }
      set
      {
        SetPropertyValue("ObjectName", ref _objectName, value);
      }
    }
    public System.String ObjectRevision
    {
      get
      {
        return _objectRevision;
      }
      set
      {
        SetPropertyValue("ObjectRevision", ref _objectRevision, value);
      }
    }
    public Aria5SystemAdmin.Module.BusinessObjects.ObjectType ObjectType
    {
      get
      {
        return _objectType;
      }
      set
      {
        SetPropertyValue("ObjectType", ref _objectType, value);
      }
    }
    //  [DataSourceCriteria("ParentObjectID = '@This.TrackingEntry.Id'")]
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObject AriaObject
    {
      get
      {
        return _ariaObject;
      }
      set
      {
        SetPropertyValue("AriaObject", ref _ariaObject, value);
        try
        {
          this.ObjectID = (int)value.ObjectID;
          this.ObjectName = value.ObjectName;
          this.ObjectType = value.ObjectType;
          this.ObjectRevision = value.ActiveRevision;
        }
        catch (Exception ex)
        {
        }
      }
    }
    [DevExpress.Xpo.AssociationAttribute("TrackingEntryAttachments-TrackingEntry")]
    public Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry TrackingEntry
    {
      get
      {
        return _trackingEntry;
      }
      set
      {
        SetPropertyValue("TrackingEntry", ref _trackingEntry, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(-1)]
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
    public override void AfterConstruction()
    {
      base.AfterConstruction();
    }
  }
}
