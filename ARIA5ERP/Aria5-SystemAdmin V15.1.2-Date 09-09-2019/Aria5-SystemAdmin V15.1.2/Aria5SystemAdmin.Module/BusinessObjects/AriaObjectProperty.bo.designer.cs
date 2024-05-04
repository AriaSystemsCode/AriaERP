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
  [RelatedEntity("Aria5-SystemAdmin-AriaObjectProperty")]
  public partial class AriaObjectProperty : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObject _ariaObject;
    public enum modificationType
    {
      Add,
      Modify,
      Delete
    };
    private System.String _ariaParentRevision;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObject _ariaParentObject;
    private Aria5SystemAdmin.Module.BusinessObjects.PropertyType _propertyType;
    private System.String _genSquFn;
    private System.Boolean _required;
    private System.String _propertyDescription;
    private modificationType _modificationType;
    private System.String _propertySettings;
    private System.String _propertyName;
    private System.String _objectRevision;
     
    public AriaObjectProperty(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
       

    public AriaObjectProperty()
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
    public System.String PropertyName
    {
      get
      {
        return _propertyName;
      }
      set
      {
        SetPropertyValue("PropertyName", ref _propertyName, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(-1)]
    public System.String PropertySettings
    {
      get
      {
        return _propertySettings;
      }
      set
      {
        SetPropertyValue("PropertySettings", ref _propertySettings, value);
      }
    }
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
    [DevExpress.Xpo.SizeAttribute(250)]
    public System.String PropertyDescription
    {
      get
      {
        return _propertyDescription;
      }
      set
      {
        SetPropertyValue("PropertyDescription", ref _propertyDescription, value);
      }
    }
    public System.Boolean Required
    {
      get
      {
        return _required;
      }
      set
      {
        SetPropertyValue("Required", ref _required, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(25)]
    public System.String GenSquFn
    {
      get
      {
        return _genSquFn;
      }
      set
      {
        SetPropertyValue("GenSquFn", ref _genSquFn, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("AriaObjectPropertieses-PropertyType")]
    public Aria5SystemAdmin.Module.BusinessObjects.PropertyType PropertyType
    {
      get
      {
        return _propertyType;
      }
      set
      {
        SetPropertyValue("PropertyType", ref _propertyType, value);
      }
    }

    //ATA add new field to save related aria object as a property 7/13/2017 [Start]
    private AriaObject _relatedAriaObject;

     [DataSourceCriteria("ObjectType.Name == '@This.PropertyType.Name'")]
    public AriaObject RelatedAriaObject
    {
        get { return _relatedAriaObject; }
        set
        {
            SetPropertyValue("RelatedAriaObject", ref _relatedAriaObject, value);
        }
    }
     //ATA add new field to save related aria object as a property 7/13/2017 [End]

    public override void AfterConstruction()
    {
      base.AfterConstruction();
    }

    [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("AriaObjectPropertiesSettingses-AriaObjectProperties")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectPropertySetting> AriaObjectPropertiesSettings
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectPropertySetting>("AriaObjectPropertiesSettings");
      }
    }

    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectPropertySetting[] _ariaObjectPropertiesSettingsArray;
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectPropertySetting[] AriaObjectPropertiesSettingsArray
    {
        get
        {
            return _ariaObjectPropertiesSettingsArray;
        }
        set
        {
            _ariaObjectPropertiesSettingsArray = value;
        }
    }
    
    
    protected override void OnSaving()
    {
      if (Session.IsNewObject(this) == true)
      {
          if ( AriaObjectPropertiesSettings != null && AriaObjectPropertiesSettings.Count == 0)
          {
              if (PropertyType!=null)
              {
                  for (int i = 0; i < PropertyType.SettingTypes.Count; i++)
                  {
                      if (PropertyType.SettingTypes[i] != null)
                      {
                          AriaObjectPropertySetting newAriaObjectPropertiesSettings = new AriaObjectPropertySetting(Session);
                          newAriaObjectPropertiesSettings.AriaObjectProperty = this;
                          newAriaObjectPropertiesSettings.Value = "";
                          newAriaObjectPropertiesSettings.DataType = PropertyType.SettingTypes[i].DataType.ToString();
                          newAriaObjectPropertiesSettings.Width = PropertyType.SettingTypes[i].Width;
                          newAriaObjectPropertiesSettings.DecimalPlaces = PropertyType.SettingTypes[i].Decimal;
                          newAriaObjectPropertiesSettings.SettingType = PropertyType.SettingTypes[i];
                          // New added line 

                          Session.Save(newAriaObjectPropertiesSettings);
                          AriaObjectPropertiesSettings.Add(newAriaObjectPropertiesSettings);
                      }
                  } 
              }
             
          }
       
      }
    }
    protected override void OnChanged(string propertyName, object oldValue, object newValue)
    {
      base.OnChanged(propertyName, oldValue, newValue);
    }
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObject AriaParentObject
    {
      get
      {
        return _ariaParentObject;
      }
      set
      {
        SetPropertyValue("AriaParentObject", ref _ariaParentObject, value);
      }
    }
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    public System.String AriaParentRevision
    {
      get
      {
        return _ariaParentRevision;
      }
      set
      {
        SetPropertyValue("AriaParentRevision", ref _ariaParentRevision, value);
      }
    }
    private Boolean _saveObject = true;
    [NonPersistent]
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    public Boolean SaveObject
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
    private Boolean _copySettings = true;
    [NonPersistent]
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    public Boolean CopySettings
    {
      get
      {
        return _copySettings;
      }
      set
      {
        SetPropertyValue("CopySettings", ref _copySettings, value);
      }
    }
    [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("AriaObjectProperties-AriaObject")]
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
  }
}
