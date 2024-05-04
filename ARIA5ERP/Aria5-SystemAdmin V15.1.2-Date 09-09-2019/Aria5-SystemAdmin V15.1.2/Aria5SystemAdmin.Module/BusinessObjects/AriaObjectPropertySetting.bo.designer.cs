using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Model;
using System.Xml.Serialization;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
    [Serializable]
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-AriaObjectPropertySetting")]
  public partial class AriaObjectPropertySetting : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectProperty _ariaObjectProperty;
    public AriaObjectPropertySetting(DevExpress.Xpo.Session session)
      : base(session)
    {
    }

    public AriaObjectPropertySetting()
    {
    }
        [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("AriaObjectPropertiesSettingses-AriaObjectProperties")]
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectProperty AriaObjectProperty
    {
      get
      {
        return _ariaObjectProperty;
      }
      set
      {
        SetPropertyValue("AriaObjectProperty", ref _ariaObjectProperty, value);
      }
    }
    private Aria5SystemAdmin.Module.BusinessObjects.SettingType _settingType;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectRevision _ariaObjectRevision;
    private System.String _value;
    private System.String _format;
    private System.Int32 _decimalPlaces;
    private System.Int32 _width;
    private System.String _validvalues;
    private System.String _dataType;
    [ModelDefault("PropertyEditorType", "Aria5SystemAdmin.Module.CustomEditorPropertySettings")]
    public System.String Value
    {
      get
      {
        return _value;
      }
      set
      {
        SetPropertyValue("Value", ref _value, value);
      }
    }
    public Aria5SystemAdmin.Module.BusinessObjects.SettingType SettingType
    {
      get
      {
        return _settingType;
      }
      set
      {
        SetPropertyValue("SettingType", ref _settingType, value);
      }
    }
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    public System.String DataType
    {
      get
      {
        return _dataType;
      }
      set
      {
        SetPropertyValue("DataType", ref _dataType, value);
      }
    }
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    public System.Int32 Width
    {
      get
      {
        return _width;
      }
      set
      {
        SetPropertyValue("Width", ref _width, value);
      }
    }
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    public System.Int32 DecimalPlaces
    {
      get
      {
        return _decimalPlaces;
      }
      set
      {
        SetPropertyValue("DecimalPlaces", ref _decimalPlaces, value);
      }
    }
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    public System.String Format
    {
      get
      {
        return _format;
      }
      set
      {
        SetPropertyValue("Format", ref _format, value);
      }
    }
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    public System.String Validvalues
    {
      get
      {
        return _validvalues;
      }
      set
      {
        SetPropertyValue("Validvalues", ref _validvalues, value);
      }
    }
    private Boolean _modified;
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    public Boolean Modified
    {
      get
      {
        return _modified;
      }
      set
      {
        SetPropertyValue("Modified", ref _modified, value);
      }
    }
    //ATA add new field  actual column name to settings to save related column if the property is a system file record  7/13/2017 [Start] 
    private string _actualColumnName;

    public string ActualColumnName
    {
        get { return _actualColumnName; }
        set { SetPropertyValue("ActualColumnName", ref _actualColumnName, value); }
    }
      //ATA add new field  actual column name to settings to save related column if the property is a system file record  7/13/2017 [End] 

  }
}
