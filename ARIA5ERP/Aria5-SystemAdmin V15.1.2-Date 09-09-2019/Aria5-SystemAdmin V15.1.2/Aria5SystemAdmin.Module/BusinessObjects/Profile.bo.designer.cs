using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  public partial class Profile : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.Boolean _isDefault;
    private System.String _deviceType;
    private Aria5SystemAdmin.Module.BusinessObjects.Industry _industry;
    private System.String _name;
    private System.String _profileId;
    public Profile(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String ProfileId
    {
      get
      {
        return _profileId;
      }
      set
      {
        SetPropertyValue("ProfileId", ref _profileId, value);
      }
    }
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
    public Aria5SystemAdmin.Module.BusinessObjects.Industry Industry
    {
      get
      {
        return _industry;
      }
      set
      {
        SetPropertyValue("Industry", ref _industry, value);
      }
    }
    public System.String DeviceType
    {
      get
      {
        return _deviceType;
      }
      set
      {
        SetPropertyValue("DeviceType", ref _deviceType, value);
      }
    }
    public System.Boolean IsDefault
    {
      get
      {
        return _isDefault;
      }
      set
      {
        SetPropertyValue("IsDefault", ref _isDefault, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ProfileSections-Profile")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ProfileSection> ProfileSections
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ProfileSection>("ProfileSections");
      }
    }
    private System.String _Title;
    public System.String Title
    {
        get
        {
            return _Title;
        }
        set
        {
            SetPropertyValue("Title", ref _Title, value);
        }
    }
    //private GuideType _GuideType;
    //public GuideType GuideType
    //{
    //    get
    //    {
    //        return _GuideType;
    //    }
    //    set
    //    {
    //        SetPropertyValue("GuideType", ref _GuideType, value);
    //    }
    //}
  }
}
