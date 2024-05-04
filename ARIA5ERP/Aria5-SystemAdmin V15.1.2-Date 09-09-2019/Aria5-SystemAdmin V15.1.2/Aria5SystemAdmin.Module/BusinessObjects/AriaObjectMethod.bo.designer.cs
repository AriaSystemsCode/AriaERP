using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using System.IO;
using DevExpress.Data.Filtering;
using DevExpress.Xpo.DB;
using System.Xml.Serialization;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
    [Serializable]
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-AriaObjectMethod")]

  public partial class AriaObjectMethod : DevExpress.Persistent.BaseImpl.BaseObject
  {
       // [XmlType(Namespace = "Method.ModificationType")]
    public enum modificationType
    {
      Add,
      Modify,
      Delete
    };
    private modificationType _modificationType;
    private System.String _methodDescription;
    private System.String _methodName;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObject _ariaObject;
    private System.String _objectRevision;
    public AriaObjectMethod(DevExpress.Xpo.Session session)
      : base(session)
    {
    }

    public AriaObjectMethod()
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
    [DevExpress.Xpo.AssociationAttribute("AriaObjectMethods-AriaObject")]
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
    public System.String MethodName
    {
      get
      {
        return _methodName;
      }
      set
      {
        SetPropertyValue("MethodName", ref _methodName, value);
      }
    }
    public System.String MethodDescription
    {
      get
      {
        return _methodDescription;
      }
      set
      {
        SetPropertyValue("MethodDescription", ref _methodDescription, value);
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
    [DevExpress.Xpo.AssociationAttribute("AriaObjectMethodParameters-AriaObjectMethod")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethodParameter> AriaObjectMethodParameters
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethodParameter>("AriaObjectMethodParameters");
      }
    }
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethodParameter[] _ariaObjectMethodParametersArray;

    public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethodParameter[] AriaObjectMethodParametersArray
    {
        get
        {
            return _ariaObjectMethodParametersArray;
        }
        set
        {
            _ariaObjectMethodParametersArray = value;
        }
    }
  }
}
