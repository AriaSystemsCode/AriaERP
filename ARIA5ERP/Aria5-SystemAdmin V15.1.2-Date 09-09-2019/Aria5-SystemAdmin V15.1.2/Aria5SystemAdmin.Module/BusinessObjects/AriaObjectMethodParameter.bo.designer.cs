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
using DevExpress.ExpressApp.Actions;
using DevExpress.Persistent.Validation;
using System.Xml.Serialization;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
      [Serializable]
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-AriaObjectMethodParameter")]

  public partial class AriaObjectMethodParameter : DevExpress.Persistent.BaseImpl.BaseObject
  {
     
      public enum modificationType { Add, Modify, Delete };
      private modificationType _modificationType;
    private Aria5SystemAdmin.Module.BusinessObjects.PropertyType _parameterType;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethod _ariaObjectMethod;
    private System.Int32 _parameterNo;
    private System.String _parameterName;
    public AriaObjectMethodParameter(DevExpress.Xpo.Session session)
      : base(session)
    {
    }

    public AriaObjectMethodParameter()
    {
    }
          [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("AriaObjectMethodParameters-AriaObjectMethod")]
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethod AriaObjectMethod
    {
      get
      {
        return _ariaObjectMethod;
      }
      set
      {
        SetPropertyValue("AriaObjectMethod", ref _ariaObjectMethod, value);
      }
    }
    public System.String ParameterName
    {
      get
      {
        return _parameterName;
      }
      set
      {
        SetPropertyValue("ParameterName", ref _parameterName, value);
      }
    }
    public System.Int32 ParameterNo
    {
      get
      {
        return _parameterNo;
      }
      set
      {
        SetPropertyValue("ParameterNo", ref _parameterNo, value);
      }
    }
      [RuleRequiredField(DefaultContexts.Save)]
    public Aria5SystemAdmin.Module.BusinessObjects.PropertyType ParameterType
    {
      get
      {
        return _parameterType;
      }
      set
      {
        SetPropertyValue("ParameterType", ref _parameterType, value);
      }
    }

  [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("AriaObjectMethodParameterSettingses-AriaObjectMethodParameter")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethodParameterSetting> AriaObjectMethodParameterSettings
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethodParameterSetting>("AriaObjectMethodParameterSettings");
      }
    }
  private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethodParameterSetting[] _ariaObjectMethodParameterSettingsArray;
  public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethodParameterSetting[] AriaObjectMethodParameterSettingsArray
  {
      get
      {
          return _ariaObjectMethodParameterSettingsArray;
      }
      set
      {
          _ariaObjectMethodParameterSettingsArray = value;
      }
  }
    public override void AfterConstruction()
    {
      BaseObject.OidInitializationMode = DevExpress.Persistent.BaseImpl.OidInitializationMode.AfterConstruction;
      ParameterNo = 0;
      base.AfterConstruction();
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
  }
}
