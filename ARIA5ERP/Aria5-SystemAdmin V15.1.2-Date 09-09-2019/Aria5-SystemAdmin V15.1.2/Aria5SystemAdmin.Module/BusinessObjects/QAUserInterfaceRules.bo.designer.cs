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
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-QAUserInterfaceRules")]
  [XafDefaultProperty("UserInterfaceDesignName")]
  public partial class QAUserInterfaceRules : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity _projectEntity;
      //ATA  8/5/216 moving uiroles under tracking [start]
    private Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry _trackingentry;
    //ATA  8/5/216 moving uiroles under tracking [end]

    public QAUserInterfaceRules(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private string _UserInterfaceDesignName;
    [Size(4000)]
      [RuleRequiredField]
    public string UserInterfaceDesignName
    {
      get
      {
        return _UserInterfaceDesignName;
      }
      set
      {
        SetPropertyValue("UserInterfaceDesignName", ref _UserInterfaceDesignName, value);
      }
    }
    private string _UIObject;
    [Size(4000)]
      [RuleRequiredField]
    public string UIObject
    {
      get
      {
        return _UIObject;
      }
      set
      {
        SetPropertyValue("UIObject", ref _UIObject, value);
      }
    }
    private string _UIMethod;
    [Size(4000)]
      [RuleRequiredField]
    public string UIMethod
    {
      get
      {
        return _UIMethod;
      }
      set
      {
        SetPropertyValue("UIMethod", ref _UIMethod, value);
      }
    }
    private string _UIRole;
    [Size(4000)]
      [RuleRequiredField]
    public string UIRole
    {
      get
      {
        return _UIRole;
      }
      set
      {
        SetPropertyValue("UIRole", ref _UIRole, value);
      }
    }
    private string _Notes;
    [Size(4000)]
    public string Notes
    {
      get
      {
        return _Notes;
      }
      set
      {
        SetPropertyValue("Notes", ref _Notes, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("UserInterfaceRules-ProjectEntity")]
      [RuleRequiredField]
    public Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity ProjectEntity
    {
      get
      {
        return _projectEntity;
      }
      set
      {
        SetPropertyValue("ProjectEntity", ref _projectEntity, value);
      }
    }
      //ATA  8/5/216 moving uiroles under tracking [start]
    [DevExpress.Xpo.AssociationAttribute("UserInterfaceRules-TrackingEntry")]
      [RuleRequiredField]
    public Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry TrackingEntry
    {
        get
        {
            //ATA,2 7/4/2016 [start,Queue ]
            if (_trackingentry != null && _trackingentry.ProjectEntity != null)
            {
                _projectEntity = _trackingentry.ProjectEntity;
            }
            //ATA,2,[end]
            return _trackingentry;
        }
        set
        {
            SetPropertyValue("TrackingEntry", ref _trackingentry, value);
        }
    }
      //ATA,1,8/5/2016 [end ] move this tab to  tracking entry instead of project entity 
     
  }
}