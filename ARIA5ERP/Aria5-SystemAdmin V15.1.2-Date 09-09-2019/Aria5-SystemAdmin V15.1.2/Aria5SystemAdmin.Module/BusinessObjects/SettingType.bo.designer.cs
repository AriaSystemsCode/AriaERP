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
using System.Xml.Serialization;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [Serializable]
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-SettingType")]
  public partial class SettingType : DevExpress.Persistent.BaseImpl.BaseObject
  {
    public enum EnumDataType
    {
      Character = 0,
      Varchar = 1,
      Number = 2,
      Int = 3,
      Boolean = 4,
      Date = 5,
      Datetime = 6,
      Image = 7,
      Text = 8,
      XML = 9,
        ValidEntries =10,
        Memo = 11
    };
    public enum EnumControlType
    {
      ObjectSettings = 0,
      PropertySettings = 1
    };
    private Aria5SystemAdmin.Module.BusinessObjects.SettingTypeGroup _settingTypesGroupKey;
    private EnumControlType _controlType;
    private System.Int16 _decimal;
    private System.Int16 _width;
    private EnumDataType _dataType;
    private System.String _name;
    private System.String _settingTypeId;
    public SettingType(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public SettingType()
    {
    }
    // we stoped the validation, because we need to allow, same setting, for property and Object
    //[RuleUniqueValue("", DevExpress.Persistent.Validation.DefaultContexts.Save, CriteriaEvaluationBehavior = PersistentCriteriaEvaluationBehavior.InTransaction)]
    [DevExpress.Xpo.SizeAttribute(50)]
    [RuleRequiredField(DefaultContexts.Save)]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public System.String SettingTypeId
    {
      get
      {
        return _settingTypeId;
      }
      set
      {
        if (IsLoading == false && ((String.IsNullOrEmpty(this.SettingTypeId) == true) || (String.IsNullOrEmpty(this.SettingTypeId) == false && this.SettingTypeId.ToLower().TrimEnd() != value.ToLower().TrimEnd())))
        {
          value = System.Threading.Thread.CurrentThread.CurrentCulture.TextInfo.ToTitleCase(value.ToLower());
        }
        if (string.IsNullOrEmpty(this.Name) == true)
        {
          this._name = value;
          this.Name = value;
        }
        ;
        value = value.Replace(" ", "");
        SetPropertyValue("SettingTypeId", ref _settingTypeId, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(100)]
    [RuleRequiredField(DefaultContexts.Save)]
    public System.String Name
    {
      get
      {
        return _name;
      }
      set
      {
        SetPropertyValue("Name", ref _name, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("SettingType-SettingTypeGroup")]
    public Aria5SystemAdmin.Module.BusinessObjects.SettingTypeGroup SettingTypesGroupKey
    {
      get
      {
        return _settingTypesGroupKey;
      }
      set
      {
        SetPropertyValue("SettingTypeGroupKey", ref _settingTypesGroupKey, value);
      }
    }
    [RuleRequiredField(DefaultContexts.Save)]
    public EnumControlType ControlType
    {
      get
      {
        return _controlType;
      }
      set
      {
        SetPropertyValue("ControlType", ref _controlType, value);
      }
    }
    [RuleRequiredField(DefaultContexts.Save)]
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [ImmediatePostData]
    public EnumDataType DataType
    {
      get
      {
        return _dataType;
      }
      set
      {
        if (value == EnumDataType.Number)
        {
          this.Decimal = 0;
        }
        ;
        if (value == EnumDataType.Boolean || value == EnumDataType.Date || value == EnumDataType.Datetime || value == EnumDataType.Image || value == EnumDataType.Int || value == EnumDataType.Text)
        {
          this.Width = 0;
        }
        ;
        SetPropertyValue("DataType", ref _dataType, value);
      }
    }
    // Look
    // [Appearance("Single", Visibility = ViewItemVisibility.Hide, Criteria = "!DataType==0 And !DataType==1 And !DataType==2 And !DataType==9")]
    //[RuleValueComparison("", DefaultContexts.Save, ValueComparisonType.GreaterThan, 0)]
    [RuleRequiredField(DefaultContexts.Save)]
    public System.Int16 Width
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
    [Appearance("Single", Visibility = ViewItemVisibility.Hide, Criteria = "!DataType==2")]
    public System.Int16 Decimal
    {
      get
      {
        return _decimal;
      }
      set
      {
        SetPropertyValue("Decimal", ref _decimal, value);
      }
    }
      //ATA add new field to hold this seting actual column name in sysfile 7/13/2017 [start]
    private string _actualColumnName;

    public string ActualColumnName
    {
        get { return _actualColumnName; }
        set { SetPropertyValue("ActualColumnName", ref _actualColumnName, value); }
    }
    //ATA add new field to hold this seting actual column name in sysfile 7/13/2017 [End]

    public override void AfterConstruction()
    {
      base.AfterConstruction();
      this.Width = 50;
      this.DataType = EnumDataType.Character;
      this.ControlType = EnumControlType.ObjectSettings;
      this.SettingTypesGroupKey = Session.FindObject<SettingTypeGroup>(DevExpress.Data.Filtering.CriteriaOperator.Parse("Name like '%' "));
    }
    [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("SettingTypes-PropertyTypes")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.PropertyType> PropertyTypes
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.PropertyType>("PropertyTypes");
      }
    }
    [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("SettingTypes-ObjectTypes")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ObjectType> ObjectTypes
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ObjectType>("ObjectTypes");
      }
    }
    [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("ApplicationSettings-SettingType")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ApplicationSetting> ApplicationSettings
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ApplicationSetting>("ApplicationSettings");
      }
    }
  }
}
