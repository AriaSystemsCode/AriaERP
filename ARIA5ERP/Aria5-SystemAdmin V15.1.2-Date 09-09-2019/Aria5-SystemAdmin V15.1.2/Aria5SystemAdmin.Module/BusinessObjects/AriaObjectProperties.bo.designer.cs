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
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  public partial class AriaObjectProperties : DevExpress.Persistent.BaseImpl.BaseObject
  {
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
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObject _ariaObject;
    private modificationType _modificationType;
    private System.String _propertySettings;
    private System.String _propertyName;
    private System.String _objectRevision;
    public AriaObjectProperties(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.AssociationAttribute("AriaObjectPropertieses-AriaObject")]
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
    public override void AfterConstruction()
    {
      //this.ObjectRevision = this.AriaObject.ActiveRevision; 
      //BaseObject.OidInitializationMode = DevExpress.Persistent.BaseImpl.OidInitializationMode.AfterConstruction;
      base.AfterConstruction();
    }
    [DevExpress.Xpo.AssociationAttribute("AriaObjectPropertiesSettingses-AriaObjectProperties")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectPropertiesSettings> AriaObjectPropertiesSettingses
    {
      get
      {
        //if (this.AriaParentObject != null)
        //{
        //    XPCollection<AriaObjectPropertiesSettings> newAriaObjectPropertiesSettingses = new XPCollection<AriaObjectPropertiesSettings>(Session);
        //    for (int i = 0; i < this.AriaParentObject.AriaObjectSettings.Count; i++)
        //    {
        //        Boolean found = false;
        //        for (int j = 0; j < newAriaObjectPropertiesSettingses.Count; j++)
        //        {
        //            if (newAriaObjectPropertiesSettingses[j].SettingType == this.AriaParentObject.AriaObjectSettings[i].SettingType)
        //            {
        //                found = true;
        //                break;
        //            }
        //        }
        //        if (found == false)
        //        {
        //            AriaObjectPropertiesSettings newAriaObjectPropertiesSettings = new AriaObjectPropertiesSettings(Session);
        //            newAriaObjectPropertiesSettings.AriaObjectProperties = this;
        //            newAriaObjectPropertiesSettings.Value = this.AriaParentObject.AriaObjectSettings[i].Value;
        //            newAriaObjectPropertiesSettings.DataType = this.AriaParentObject.AriaObjectSettings[i].DataType;
        //            newAriaObjectPropertiesSettings.Width = this.AriaParentObject.AriaObjectSettings[i].Width;
        //            newAriaObjectPropertiesSettings.DecimalPlaces = this.AriaParentObject.AriaObjectSettings[i].DecimalPlaces;
        //            newAriaObjectPropertiesSettings.SettingType = this.AriaParentObject.AriaObjectSettings[i].SettingType;
        //            newAriaObjectPropertiesSettingses.Add(newAriaObjectPropertiesSettings);
        //        };
        //    }
        //    return newAriaObjectPropertiesSettingses;
        //}
        //else
        //{ return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectPropertiesSettings>("AriaObjectPropertiesSettingses"); }
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectPropertiesSettings>("AriaObjectPropertiesSettingses");
      }
    }
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [NonPersistent]
    public Boolean CallUpOnSave = true;
    protected override void OnDeleting()
    {
      this.AriaObject.IncrementRevision();
      base.OnDeleting();
    }
    protected override void OnSaving()
    {
      //if (this.calledFromSettings == false) // to no repeat the code between here calling and property settings calling.
      // { Before_Save(); }
      Before_Save();
      base.OnSaving();
    }
    [NonPersistent]
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    public bool calledFromSettings = false;
    public void Before_Save()
    {
        return;
      if (this.Session.IsNewObject(this) == true)
      {
        if (this.ObjectRevision != "")
        {
          this.AriaObject.IncrementRevision();
          this.ObjectRevision = "";
        }
        ;
        if (CopySettings == true)
        {
          if (this.AriaParentObject == null)
          {
            for (int i = 0; i < this.PropertyType.SettingTypes.Count; i++)
            {
              if (this.PropertyType.SettingTypes[i] != null)
              {
                AriaObjectPropertiesSettings newAriaObjectPropertiesSettings = new AriaObjectPropertiesSettings(Session);
                newAriaObjectPropertiesSettings.AriaObjectProperties = this;
                newAriaObjectPropertiesSettings.Value = "";
                newAriaObjectPropertiesSettings.DataType = this.PropertyType.SettingTypes[i].DataType.ToString();
                newAriaObjectPropertiesSettings.Width = this.PropertyType.SettingTypes[i].Width;
                newAriaObjectPropertiesSettings.DecimalPlaces = this.PropertyType.SettingTypes[i].Decimal;
                newAriaObjectPropertiesSettings.SettingType = this.PropertyType.SettingTypes[i];
                // New added line 
                this.Session.Save(newAriaObjectPropertiesSettings);
                this.AriaObjectPropertiesSettingses.Add(newAriaObjectPropertiesSettings);
              }
              ;
            }
          }
          else
          {
            AriaObjectRevision newAriaObjectRevision = this.Session.FindObject<AriaObjectRevision>(DevExpress.Data.Filtering.CriteriaOperator.Parse("ObjectRevision='" + this.AriaParentRevision + "' and AriaObject ='" + this.AriaParentObject.Oid.ToString() + "'"));
            for (int i = 0; i < newAriaObjectRevision.AriaObjectSettingses.Count; i++)
            {
              //if (this.PropertyType.SettingTypes[i] != null)
              //{
              AriaObjectPropertiesSettings newAriaObjectPropertiesSettings = new AriaObjectPropertiesSettings(Session);
              newAriaObjectPropertiesSettings.AriaObjectProperties = this;
              newAriaObjectPropertiesSettings.Value = newAriaObjectRevision.AriaObjectSettingses[i].Value;
              newAriaObjectPropertiesSettings.DataType = newAriaObjectRevision.AriaObjectSettingses[i].DataType;
              newAriaObjectPropertiesSettings.Width = newAriaObjectRevision.AriaObjectSettingses[i].Width;
              newAriaObjectPropertiesSettings.DecimalPlaces = newAriaObjectRevision.AriaObjectSettingses[i].DecimalPlaces;
              newAriaObjectPropertiesSettings.SettingType = newAriaObjectRevision.AriaObjectSettingses[i].SettingType;
              // New added line 
              //this.Session.Save(newAriaObjectPropertiesSettings);
              this.AriaObjectPropertiesSettingses.Add(newAriaObjectPropertiesSettings);
              //}
              ;
            }
          }
          ;
        }
      }
      ;
      if (this.Session.IsNewObject(this) == false && this.IsDeleted == false && this.ObjectRevision != "" && SaveObject == true)
      {
        //Save old record HIA 2013-Jan-13 [Strart]
        //    #region copy the modified property line, to a new property line
        //    AriaObjectProperties newAriaObjectProperties = new AriaObjectProperties(Session);
        //    newAriaObjectProperties.CopySettings = false;
        //    newAriaObjectProperties.AriaObject = this.AriaObject;
        //    newAriaObjectProperties.ObjectRevision = "";
        //    newAriaObjectProperties.ModificationType = this.ModificationType;
        //    newAriaObjectProperties.PropertyName = this.PropertyName;
        //    newAriaObjectProperties.PropertyDescription = this.PropertyDescription;
        //    newAriaObjectProperties.PropertySettings = this.PropertySettings;
        //    newAriaObjectProperties.Required = this.Required;
        //    newAriaObjectProperties.GenSquFn = this.GenSquFn;
        //    newAriaObjectProperties.PropertyType = this.PropertyType;
        //    newAriaObjectProperties.AriaParentObject = this.AriaParentObject;
        //    newAriaObjectProperties.AriaParentRevision = this.AriaParentRevision;
        //    #region Add new property line, settings
        //    for (int j = 0; j < this.AriaObjectPropertiesSettingses.Count; j++)
        //    {
        //        AriaObjectPropertiesSettings newAriaObjectPropertiesSettings = new AriaObjectPropertiesSettings(Session);
        //        newAriaObjectPropertiesSettings.AriaObjectProperties = newAriaObjectProperties;
        //        newAriaObjectPropertiesSettings.Value = this.AriaObjectPropertiesSettingses[j].Value;
        //        newAriaObjectPropertiesSettings.DataType = this.AriaObjectPropertiesSettingses[j].DataType.ToString();
        //        newAriaObjectPropertiesSettings.Width = this.AriaObjectPropertiesSettingses[j].Width;
        //        newAriaObjectPropertiesSettings.DecimalPlaces = this.AriaObjectPropertiesSettingses[j].DecimalPlaces;
        //        newAriaObjectPropertiesSettings.SettingType = this.AriaObjectPropertiesSettingses[j].SettingType;
        //        newAriaObjectPropertiesSettings.Modified = this.AriaObjectPropertiesSettingses[j].Modified;
        //        // New added line 
        //        // this.Session.Save(newAriaObjectPropertiesSettings);
        //        newAriaObjectProperties.AriaObjectPropertiesSettingses.Add(newAriaObjectPropertiesSettings);
        //    }
        //    #endregion
        //    #endregion
        //    #region restore the modified fields, values from DB
        //    string ss2 = "Select ModificationType, PropertyName, PropertyDescription, Required, GenSquFn, PropertyType  from AriaObjectProperties where Oid = '" + Oid.ToString() + "'";
        //    SelectedData CanselectedData = Session.ExecuteQuery(ss2);
        //    try
        //    {
        //        switch (CanselectedData.ResultSet[0].Rows[0].Values[0].ToString().Trim())
        //        {
        //            case "2":
        //                this.ModificationType = modificationType.Delete;
        //                break;
        //            case "1":
        //                this.ModificationType = modificationType.Modify;
        //                break;
        //            default:
        //                this.ModificationType = modificationType.Add;
        //                break;
        //        }
        //    }
        //    catch (Exception ex)
        //    {
        //    }
        //    ;
        //    try
        //    {
        //        this.PropertyName = CanselectedData.ResultSet[0].Rows[0].Values[1].ToString();
        //    }
        //    catch (Exception ex)
        //    {
        //    }
        //    ;
        //    try
        //    {
        //        this.PropertyDescription = CanselectedData.ResultSet[0].Rows[0].Values[2].ToString();
        //    }
        //    catch (Exception ex)
        //    {
        //    }
        //    ;
        //    try
        //    {
        //        this.Required = Boolean.Parse(CanselectedData.ResultSet[0].Rows[0].Values[3].ToString());
        //    }
        //    catch (Exception ex)
        //    {
        //    }
        //    ;
        //    try
        //    {
        //        this.GenSquFn = CanselectedData.ResultSet[0].Rows[0].Values[4].ToString();
        //    }
        //    catch (Exception ex)
        //    {
        //    }
        //    ;
        //    try
        //    {
        //        this.PropertyType = Session.FindObject<PropertyType>(DevExpress.Data.Filtering.CriteriaOperator.Parse("Oid='" + CanselectedData.ResultSet[0].Rows[0].Values[5].ToString() + "'"));
        //    }
        //    catch (Exception ex)
        //    {
        //    }
        //    ;
        //};
        //    #endregion
        // Copy current property, to create old property line and settings
        if (this.IsDeleted == false && SaveObject == true)
        {
          this.AriaObject.IncrementRevision();
          if (this.Session.IsNewObject(this) == false && this.ObjectRevision.ToString().TrimEnd() != "")
          {
            this.ModificationType = modificationType.Modify;
            // Get new ID
            SelectedData CanselectedData = Session.ExecuteQuery("SELECT top(1) NEWID() AS Oid FROM AriaObjectProperties");
            String Newid = CanselectedData.ResultSet[0].Rows[0].Values[0].ToString();
            // Duplicate the AriaObjectProperties, with modification Type = "Modified"
            this.Session.ExecuteNonQuery("Insert into AriaObjectProperties (Oid, AriaObject, ObjectRevision, PropertyName, PropertySettings, ModificationType, PropertyDescription, Required, GenSquFn, PropertyType, AriaParentObject, AriaParentRevision, OptimisticLockField, GCRecord, NowLocked)" + " SELECT '" + Newid + "' as Oid, AriaObject, ObjectRevision, PropertyName, PropertySettings, ModificationType, PropertyDescription, Required, GenSquFn, PropertyType, AriaParentObject, AriaParentRevision, OptimisticLockField, GCRecord, 1 as NowLocked FROM AriaObjectProperties WHERE     (CONVERT(nvarchar(36), Oid) = '" + this.Oid.ToString() + "') ");
            // Duplicate the AriaObjectProperties Settings
            this.Session.ExecuteNonQuery("insert into AriaObjectPropertiesSettings ( Oid, AriaObjectProperties, Value, SettingType, DataType, Width, DecimalPlaces, Format, Validvalues, Modified, OptimisticLockField, GCRecord, NowLocked) SELECT  newid() as OID,'" + Newid + "' as AriaObjectProperties, Value, SettingType, DataType, Width, DecimalPlaces, Format, Validvalues, Modified, OptimisticLockField, GCRecord, 1 as NowLocked FROM AriaObjectPropertiesSettings where GCRecord is null and (CONVERT(nvarchar(36), AriaObjectProperties) = '" + this.Oid.ToString() + "') ");
          }
          this.ObjectRevision = "";
        }
        ;
        SaveObject = false;
        //Save old record HIA 2013-Jan-13 [Strart]
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
    //public System.Boolean Deleted
    //{
    //  get
    //  {
    //    return _deleted;
    //  }
    //  set
    //  {
    //    SetPropertyValue("Deleted", ref _deleted, value);
    //  }
    //}
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
  }
}
