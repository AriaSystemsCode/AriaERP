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
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  public partial class AriaObjectShelveEvent : DevExpress.Persistent.BaseImpl.BaseObject
  {
    //private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelve _ariaObjectShelve;
    private System.Boolean _isNew;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelve _ariaObjectShelve;
    private System.Boolean _saveObject;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEvent.ModificationTypes _modificationType;
    private System.String _eventDescription;
    private System.String _eventName;
    public AriaObjectShelveEvent(DevExpress.Xpo.Session session)
      : base(session)
    {
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
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEvent.ModificationTypes ModificationType
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
    [DevExpress.Xpo.AssociationAttribute("AriaObjectShelveEventParameters-AriaObjectShelveEvent")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEventParameter> AriaObjectShelveEventParameters
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEventParameter>("AriaObjectShelveEventParameters");
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
    [DevExpress.Xpo.AssociationAttribute("AriaObjectShelveEvents-AriaObjectShelve")]
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
  }
  //[CodeRule]
  //public class AriaObjectShelveEventCodeRule : RuleBase<AriaObjectShelveEvent>
  //{
  //  protected override bool IsValidInternal(AriaObjectShelveEvent target, out string errorMessageTemplate)
  //  {
  //    foreach (AriaObjectShelveEvent p in AriaObjectShelveDetailViewController.CurrentAriaObjectShelve.AriaObjectShelveEvents)
  //    {
  //      if (p.EventName == target.EventName && p.Oid != target.Oid)
  //      {
  //        errorMessageTemplate = "Can't Save, Event is already exists";
  //        return false;
  //      }
  //    }
  //    errorMessageTemplate = "";
  //    return true;
  //  }
  //  public AriaObjectShelveEventCodeRule()
  //    : base("", "Save")
  //  {
  //  }
  //  public AriaObjectShelveEventCodeRule(IRuleBaseProperties properties)
  //    : base(properties)
  //  {
  //  }
  //}
}