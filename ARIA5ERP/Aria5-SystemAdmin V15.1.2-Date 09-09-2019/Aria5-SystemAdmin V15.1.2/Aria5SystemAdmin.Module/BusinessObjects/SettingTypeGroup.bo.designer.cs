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
  [RelatedEntity("Aria5-SystemAdmin-SettingTypeGroup")]
  public partial class SettingTypeGroup : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.String _settingTypeGroupID;
    private System.String _name;
    public SettingTypeGroup(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public SettingTypeGroup()
    {
    }
    [RuleRequiredField(DefaultContexts.Save)]
    [RuleUniqueValue("", DevExpress.Persistent.Validation.DefaultContexts.Save, CriteriaEvaluationBehavior = DevExpress.Persistent.Validation.CriteriaEvaluationBehavior.InTransaction)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    [DevExpress.Xpo.SizeAttribute(50)]
    [ImmediatePostData]
    public System.String SettingTypeGroupID
    {
      get
      {
        return _settingTypeGroupID;
      }
      set
      {
          if (IsLoading == false && ((String.IsNullOrEmpty(this.SettingTypeGroupID) == true) || (String.IsNullOrEmpty(this.SettingTypeGroupID) == false && this.SettingTypeGroupID.ToLower().TrimEnd() != value.ToLower().TrimEnd())))
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
        SetPropertyValue("SettingTypeGroupID", ref _settingTypeGroupID, value);
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
    [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("SettingType-SettingTypeGroup")]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.SettingType> SettingTypes
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.SettingType>("SettingTypes");
      }
    }

   
  }
}
