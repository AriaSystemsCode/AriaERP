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
  [RelatedEntity("Aria5-SystemAdmin-AriaObjectEventParameter")]

  public partial class AriaObjectEventParameter : DevExpress.Persistent.BaseImpl.BaseObject
  {
    
    public enum modificationType
    {
      Add,
      Modify,
      Delete
    };
    private modificationType _modificationType;
    private Aria5SystemAdmin.Module.BusinessObjects.PropertyType _parameterType;
    private System.String _parameterName;
    private System.Int32 _parameterNo;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEvent _ariaObjectEvent;
    public AriaObjectEventParameter(DevExpress.Xpo.Session session)
      : base(session)
    {
    }

    public AriaObjectEventParameter()
    {
    }
      [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("AriaObjectEventParameters-AriaObjectEvent")]
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEvent AriaObjectEvent
    {
      get
      {
        return _ariaObjectEvent;
      }
      set
      {
        SetPropertyValue("AriaObjectEvent", ref _ariaObjectEvent, value);
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
    [DevExpress.Xpo.AssociationAttribute("AriaObjectEventParameterSettingses-AriaObjectEventParameter")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEventParameterSetting> AriaObjectEventParameterSettings
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEventParameterSetting>("AriaObjectEventParameterSettings");
      }
    }
      private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEventParameterSetting[] _ariaObjectEventParameterSettingsArray;

      public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEventParameterSetting[] AriaObjectEventParameterSettingsArray
      {
          get
          {
              return _ariaObjectEventParameterSettingsArray;
          }
          set
          {
              _ariaObjectEventParameterSettingsArray = value;
          }
      }
    public override void AfterConstruction()
    {
      BaseObject.OidInitializationMode = DevExpress.Persistent.BaseImpl.OidInitializationMode.AfterConstruction;
      ParameterNo = 0;
      base.AfterConstruction();
    }
    protected override void OnSaving()
    {
      base.OnSaving();
      if (Session.IsNewObject(this))
      {
        Aria5SystemAdmin.Module.Aria5Globals aria5Globals = new Aria5Globals();
        ParameterNo = aria5Globals.GetSequence("AriaObjectEventParameter", "ParameterNo", Session, " AriaObjectEvent ='" + AriaObjectEvent.Oid.ToString() + "'");
        for (int i = 0; i < ParameterType.SettingTypes.Count; i++)
        {
          AriaObjectEventParameterSetting newAriaObjectEventParameterSettings = new AriaObjectEventParameterSetting(Session);
          newAriaObjectEventParameterSettings.AriaObjectEventParameter = this;
          newAriaObjectEventParameterSettings.Value = "";
          newAriaObjectEventParameterSettings.DataType = ParameterType.SettingTypes[i].DataType.ToString();
          newAriaObjectEventParameterSettings.Width = ParameterType.SettingTypes[i].Width;
          newAriaObjectEventParameterSettings.DecimalPlaces = ParameterType.SettingTypes[i].Decimal;
          newAriaObjectEventParameterSettings.SettingType = ParameterType.SettingTypes[i];
          AriaObjectEventParameterSettings.Add(newAriaObjectEventParameterSettings);
        }
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
  }
}
