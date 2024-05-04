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
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  public partial class AriaObjectPropertiesSettings : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectProperties _ariaObjectProperties;
    public AriaObjectPropertiesSettings(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.AssociationAttribute("AriaObjectPropertiesSettingses-AriaObjectProperties")]
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectProperties AriaObjectProperties
    {
      get
      {
        return _ariaObjectProperties;
      }
      set
      {
        SetPropertyValue("AriaObjectProperties", ref _ariaObjectProperties, value);
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
        if (this.Session.IsNewObject(this) != true && this.FromInside == false && this.IsLoading == false)
        {
          this.Modified = true;
        }
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
    protected override void OnSaving()
    {
        return;
      //Save old record HIA 2013-Jan-13 [Strart]
      //  if (this.Session.IsNewObject(this) == false && this.AriaObjectProperties.ObjectRevision != "" && this.AriaObjectProperties.calledFromSettings == false)
      //{
      //    //this.AriaObjectProperties.AriaObject.IncrementRevision();
      //    this.AriaObjectProperties.calledFromSettings = true;
      //    this.AriaObjectProperties.ModificationType = BusinessObjects.AriaObjectProperties.modificationType.Modify;
      //    this.AriaObjectProperties.Before_Save();
      //}
      //  if (this.Session.IsNewObject(this) == false && this.AriaObjectProperties.ObjectRevision != "" && this.AriaObjectProperties.calledFromSettings == true)
      //  { // Restore DB values
      //      this.Value = this.oldValue;
      //      this.Modified = this.oldModified;
      //  }
      if (this.IsDeleted == false && this.Session.IsNewObject(this) == false && this.AriaObjectProperties.ObjectRevision != "")
      {
        this.AriaObjectProperties.Before_Save();
      }
      ;
      base.OnSaving();
    }
    public void CustomSave()
    {
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
    [NonPersistent]
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    public Boolean FromInside = false;
    [NonPersistent]
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    private System.String oldValue = "";
    //private System.Boolean oldModified ;
    [NonPersistent]
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    private Boolean _nowLocked;
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    public System.Boolean NowLocked
    {
      get
      {
        return _nowLocked;
      }
      set
      {
        SetPropertyValue("NowLocked", ref _nowLocked, value);
      }
    }
    protected override void OnLoaded()
    {
      base.OnLoaded();
      string strValue;
      if (this.AriaObjectProperties.AriaParentObject != null && this.Modified == false)
      {
        AriaObjectSettings newAriaObjectSettings;
        try
        {
          newAriaObjectSettings = this.Session.FindObject<AriaObjectSettings>(DevExpress.Data.Filtering.CriteriaOperator.Parse("AriaObjectRevision.ObjectRevision='" + this.AriaObjectProperties.AriaParentRevision + "' and AriaObjectRevision.AriaObject ='" + this.AriaObjectProperties.AriaParentObject.Oid + "' and SettingType='" + this.SettingType.Oid + "'"));
          strValue = newAriaObjectSettings.Value;
          FromInside = true;
          this.Value = strValue;
          FromInside = false;
        }
        catch (Exception ex)
        {
        }
        ;
      }
      // save value, to, use in restore after modified by user
      //this.oldValue = this.Value;
      //this.oldModified = this.Modified;
    }
  }
}
