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
using DevExpress.ExpressApp.Model;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-AriaObjectShelvePropertySetting")]
  public partial class AriaObjectShelvePropertySetting : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveProperty _ariaObjectShelveProperty;
    private System.Int32 _width;
    private System.Int32 _decimalPlaces;
    private Aria5SystemAdmin.Module.BusinessObjects.SettingType _settingType;
    private System.Boolean _modified;
    private System.String _dataType;
    private System.String _value;
    public AriaObjectShelvePropertySetting(DevExpress.Xpo.Session session)
      : base(session)
    {
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
       [ModelDefault("PropertyEditorType", "Aria5SystemAdmin.Module.CustomEditorSettings")]
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
    [DevExpress.Xpo.AssociationAttribute("AriaObjectShelvePropertySettings-AriaObjectShelveProperty")]
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveProperty AriaObjectShelveProperty
    {
      get
      {
        return _ariaObjectShelveProperty;
      }
      set
      {
        SetPropertyValue("AriaObjectShelveProperty", ref _ariaObjectShelveProperty, value);
      }
    }
    public System.Boolean Modified
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
    //ATA add new field  actual column name to settings to save related column if the Property  is a system file record  7/13/2017 [Start] 
    private string _actualColumnName;

    public string ActualColumnName
    {
        get { return _actualColumnName; }
        set { SetPropertyValue("ActualColumnName", ref _actualColumnName, value); }
    }
      //ATA add new field  actual column name to settings to save related column if the Property is a system file record  7/13/2017 [End] 

  }
}
