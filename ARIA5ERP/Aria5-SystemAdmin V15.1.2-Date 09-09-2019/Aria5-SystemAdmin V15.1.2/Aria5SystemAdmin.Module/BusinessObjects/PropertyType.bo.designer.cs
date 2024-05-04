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
  [RelatedEntity("Aria5-SystemAdmin-PropertyType")]
  public partial class PropertyType : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.String _name;
    private System.String _propertyTypeId;
    public PropertyType(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public PropertyType()
    {
    }
    [DevExpress.Xpo.SizeAttribute(50)]
    [RuleUniqueValue("", DefaultContexts.Save, CriteriaEvaluationBehavior = DevExpress.Persistent.Validation.CriteriaEvaluationBehavior.InTransaction)]
    [RuleRequiredField(DefaultContexts.Save)]
    [RuleRegularExpression("", DefaultContexts.Save, @"\S")]
    [ImmediatePostData]
    public System.String PropertyTypeId
    {
      get
      {
        return _propertyTypeId;
      }
      set
      {
        if (IsLoading == false && ((String.IsNullOrEmpty(this.PropertyTypeId) == true) || (String.IsNullOrEmpty(this.PropertyTypeId) == false && this.PropertyTypeId.ToLower().TrimEnd() != value.ToLower().TrimEnd())))
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
        SetPropertyValue("PropertyTypeId", ref _propertyTypeId, value);
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
    [DevExpress.Xpo.AssociationAttribute("SettingTypes-PropertyTypes")]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.SettingType> SettingTypes
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.SettingType>("SettingTypes");
      }
    }
    [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("PropertyTypes-ObjectTypes")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ObjectType> ObjectTypes
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ObjectType>("ObjectTypes");
      }
    }
    [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("AriaObjectPropertieses-PropertyType")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectProperty> AriaObjectPropertieses
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectProperty>("AriaObjectPropertieses");
      }
    }
  }
}
