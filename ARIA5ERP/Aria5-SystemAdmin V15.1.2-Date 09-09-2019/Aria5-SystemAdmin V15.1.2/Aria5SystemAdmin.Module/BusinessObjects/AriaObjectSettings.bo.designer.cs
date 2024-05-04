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
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.Persistent.Base;
using System.IO;
using DevExpress.Data.Filtering;
using DevExpress.Xpo.DB;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  public partial class AriaObjectSettings : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.SettingType _settingType;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectRevision _ariaObjectRevision;
    private System.String _value;
    private System.String _format;
    private System.Int32 _decimalPlaces;
    private System.Int32 _width;
    private System.String _validvalues;
    private System.String _dataType;
    public AriaObjectSettings(DevExpress.Xpo.Session session)
      : base(session)
    {
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
    [DevExpress.Xpo.AssociationAttribute("AriaObjectSettingses-AriaObjectRevision")]
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectRevision AriaObjectRevision
    {
      get
      {
        return _ariaObjectRevision;
      }
      set
      {
        SetPropertyValue("AriaObjectRevision", ref _ariaObjectRevision, value);
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
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [NonPersistent]
    public Boolean CallIncrement = true;
    protected override void OnSaving()
    {
        return;

        #region Support new revision feature
        //Support new revision feature HIA, 2013-Jan-12 [Start]
        ////if (this.IsDeleted == false && CallIncrement == true)
      //if (this.IsDeleted == false && string.IsNullOrEmpty(this.AriaObjectRevision.ObjectRevision) == false)
      //{
      //  //if (this.AriaObjectRevision.AriaObject.hasEmptyRevision == true)
      //  //{
      //  //    for (int i = 0; i < this.AriaObjectRevision.AriaObject.EmptyAriaObjectRevision.AriaObjectSettingses.Count; i++) 
      //  //    {
      //  //        if (this.AriaObjectRevision.AriaObject.EmptyAriaObjectRevision.AriaObjectSettingses[i].SettingType == this.SettingType)
      //  //        { this.AriaObjectRevision.AriaObject.EmptyAriaObjectRevision.AriaObjectSettingses[i].Value = this.Value;
      //  //        break;
      //  //        }
      //  //    }
      //  //}
      //  //else{
      //  this.AriaObjectRevision.AriaObject.IncrementRevision();
      //  //};
      //  try
      //  {
      //    SelectedData CanselectedData = this.Session.ExecuteQuery("Select value from AriaObjectSettings where Oid='" + this.Oid.ToString() + "'");
      //    this.Value = CanselectedData.ResultSet[0].Rows[0].Values[0].ToString();
      //  }
      //  catch (Exception ex)
      //  {
      //  }
      //  ;
        //}


        if (this.Session.IsNewObject(this) == false && this.IsDeleted == false && string.IsNullOrEmpty(this.AriaObjectRevision.ObjectRevision) == false && this.CallIncrement== true)
        {
            //..Duplicate the current revision and its settings
            // Get new ID
            SelectedData CanselectedData = Session.ExecuteQuery("SELECT top(1) NEWID() AS Oid FROM AriaObjectRevision");
            String Newid = CanselectedData.ResultSet[0].Rows[0].Values[0].ToString();

            // Duplicate the Revision
            this.Session.ExecuteNonQuery("Insert into AriaObjectRevision (Oid,NowLocked, AriaObject, ObjectRevision, ObjectRevisionSettings, BuildNo, TrackingNo, ServicePack, ReleaseNo, AutoTaskTicket, ShortDescription, LongDescription, OptimisticLockField, GCRecord ) SELECT '" + Newid + "' as Oid,1 as NowLocked, AriaObject, ObjectRevision, ObjectRevisionSettings, BuildNo, TrackingNo, ServicePack, ReleaseNo, AutoTaskTicket, ShortDescription, LongDescription, OptimisticLockField, GCRecord FROM AriaObjectRevision where  (CONVERT(nvarchar(36), Oid) = '" + this.AriaObjectRevision.Oid.ToString() + "') ");
            // Duplicate the Revision settings
            this.Session.ExecuteNonQuery("Insert into AriaObjectSettings (Oid,NowLocked, Value, AriaObjectRevision, SettingType, DataType, Width, DecimalPlaces, Format, Validvalues, OptimisticLockField, GCRecord) SELECT    newid() as  Oid,1 as NowLocked, Value,'" + Newid + "' as  AriaObjectRevision, SettingType, DataType, Width, DecimalPlaces, Format, Validvalues, OptimisticLockField, GCRecord FROM AriaObjectSettings where  GCRecord is null and  (CONVERT(nvarchar(36), AriaObjectRevision) = '" + this.AriaObjectRevision.Oid.ToString() + "') ");

            //.. Rename current revision and its settings
            this.AriaObjectRevision.ObjectRevision = "";
            this.AriaObjectRevision.AriaObject.ActiveRevision = "";
        }

        // Support new revision feature HIA, 2013-Jan-12 [End]
        #endregion
        base.OnSaving();
    }

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
