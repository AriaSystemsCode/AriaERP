using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Persistent.Validation;
using System.Xml.Serialization;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [Serializable]
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-ObjectType")]
  public partial class ObjectType : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.ObjectType _parentObjectType;
    private System.String _settingsClass;
    private System.String _name;
    private System.String _objectTypeID;
    public ObjectType(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public ObjectType()
    {
    }
    [DevExpress.Xpo.SizeAttribute(50)]
    [RuleRequiredField(DefaultContexts.Save)]
    [RuleUniqueValue("", DefaultContexts.Save, CriteriaEvaluationBehavior = DevExpress.Persistent.Validation.CriteriaEvaluationBehavior.InTransaction)]
    [ImmediatePostData]
    public System.String ObjectTypeID
    {
      get
      {
        return _objectTypeID;
      }
      set
      {
        if (IsLoading == false && ((String.IsNullOrEmpty(this.ObjectTypeID) == true) || (String.IsNullOrEmpty(this.ObjectTypeID) == false && this.ObjectTypeID.ToLower().TrimEnd() != value.ToLower().TrimEnd())))
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
        SetPropertyValue("ObjectTypeID", ref _objectTypeID, value);
      }
    }
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
    [RuleRequiredField(DefaultContexts.Save)]
    public System.String SettingsClass
    {
      get
      {
        return _settingsClass;
      }
      set
      {
        SetPropertyValue("SettingsClass", ref _settingsClass, value);
      }
    }
    [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("SettingTypes-ObjectTypes")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.SettingType> SettingTypes
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.SettingType>("SettingTypes");
      }
    }
    [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("PropertyTypes-ObjectTypes")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.PropertyType> PropertyTypes
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.PropertyType>("PropertyTypes");
      }
    }
    public Aria5SystemAdmin.Module.BusinessObjects.ObjectType ParentObjectType
    {
      get
      {
        return _parentObjectType;
      }
      set
      {
        SetPropertyValue("ParentObjectType", ref _parentObjectType, value);
      }
    }
    [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("AriaObjects-ObjectType")]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObject> AriaObjects
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObject>("AriaObjects");
      }
    }
  }
}
