//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Persistent.Validation;
using Aria5SystemAdmin.Module.Controllers;
using DevExpress.Data.Filtering;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  public partial class AriaObjectShelveMethod : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelve _ariaObjectShelve;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethod.ModificationTypes _modificationType;
    private System.Boolean _isNew;
    private System.Boolean _saveObject;
    private System.String _methodDescription;
    private System.String _methodName;
    public AriaObjectShelveMethod(DevExpress.Xpo.Session session)
      : base(session)
    {
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
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethod.ModificationTypes ModificationType
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
    public System.Boolean SaveObject
    {
      get
      {
        return _saveObject;
      }
      set
      {
        SetPropertyValue("SaveObject", ref _saveObject, value);
      }
    }
    public System.Boolean IsNew
    {
      get
      {
        return _isNew;
      }
      set
      {
        SetPropertyValue("IsNew", ref _isNew, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("AriaObjectShelveMethods-AriaObjectShelve")]
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelve AriaObjectShelve
    {
      get
      {
        return _ariaObjectShelve;
      }
      set
      {
        SetPropertyValue("AriaObjectShelve", ref _ariaObjectShelve, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("AriaObjectShelveMethodParameters-AriaObjectShelveMethod")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethodParameter> AriaObjectShelveMethodParameters
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethodParameter>("AriaObjectShelveMethodParameters");
      }
    }
  }
}