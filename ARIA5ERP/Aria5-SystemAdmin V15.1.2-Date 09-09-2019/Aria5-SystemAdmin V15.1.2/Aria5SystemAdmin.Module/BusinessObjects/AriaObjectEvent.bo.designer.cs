using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.Persistent.Base;
using System.IO;
using DevExpress.Data.Filtering;
using DevExpress.Xpo.DB;
using System.Xml.Serialization;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
       [Serializable]
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-AriaObjectEvent")]

  public partial class AriaObjectEvent : DevExpress.Persistent.BaseImpl.BaseObject
  {
          
    public enum modificationType
    {
      Add,
      Modify,
      Delete
    };
    private modificationType _modificationType;
    private System.String _eventDescription;
    private System.String _eventName;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObject _ariaObject;
    private System.String _objectRevision;
    public AriaObjectEvent(DevExpress.Xpo.Session session)
      : base(session)
    {
    }

    public AriaObjectEvent()
    {
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
    [XmlIgnore] 
    [DevExpress.Xpo.AssociationAttribute("AriaObjectEvents-AriaObject")]
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObject AriaObject
    {
      get
      {
        return _ariaObject;
      }
      set
      {
        SetPropertyValue("AriaObject", ref _ariaObject, value);
      }
    }
    public System.String EventName
    {
      get
      {
        return _eventName;
      }
      set
      {
        SetPropertyValue("EventName", ref _eventName, value);
      }
    }
    public System.String EventDescription
    {
      get
      {
        return _eventDescription;
      }
      set
      {
        SetPropertyValue("EventDescription", ref _eventDescription, value);
      }
    }
           [XmlIgnore]
    public modificationType ModificationType
    {
      get
      {
        return _modificationType;
      }
      set
      {
        SetPropertyValue("ModificationType", ref _modificationType, value);
      }
    }

    [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("AriaObjectEventParameters-AriaObjectEvent")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEventParameter> AriaObjectEventParameters
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEventParameter>("AriaObjectEventParameters");
      }
    }
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEventParameter[] _ariaObjectEventParametersArray;
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEventParameter[] AriaObjectEventParametersArray
    {
        get
        {
            return _ariaObjectEventParametersArray;
        }
        set
        {
            _ariaObjectEventParametersArray = value;
        }
    }
  }
}
