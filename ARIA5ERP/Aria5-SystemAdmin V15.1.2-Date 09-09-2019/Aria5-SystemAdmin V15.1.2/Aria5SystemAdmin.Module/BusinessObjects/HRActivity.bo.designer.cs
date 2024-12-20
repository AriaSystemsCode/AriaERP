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
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.ExpressApp.Editors;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [XafDefaultProperty("Name")]
  [RelatedEntity("Aria5-SystemAdmin-HRActivity")]
  [Appearance("Frequency", AppearanceItemType = "ViewItem", TargetItems = "FrequencyType", Criteria = "Frequency !='Recurring'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
  [Appearance("Days1", AppearanceItemType = "ViewItem", TargetItems = "Sat,Sun,Mon,Tue,Wed,Thu,Fri", Criteria = "FrequencyType !='Weekly'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
  [Appearance("Actioncomplete", AppearanceItemType = "Action", TargetItems = "CompleteActivity", Criteria = "Status == 'History'", Context = "DetailView", Enabled = false)]
  public partial class HRActivity : Entity
  {
    public HRActivity(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private string _name;
    [RuleRequiredField]
    public string Name
    {
      get
      {
        return _name;
      }
      set
      {
        _name = value;
      }
    }
    private HR_ActivityCategory _category;
    //[DataSourceProperty("Department.Categories")]
    //[DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    [Association("HRActivity-HR_ActivityCategory")]
    [RuleRequiredField]
    [DataSourceProperty("HRRole.Categories")]
    public HR_ActivityCategory Category
    {
      get
      {
        return _category;
      }
      set
      {
        _category = value;
      }
    }
    // private HRJobPosition _jobPosition;
    [DataSourceCriteria("Master = true")]
    [Association("HRJobPosition-HRActivity")]
    public XPCollection<HRJobPosition> JobPositions
    {
      get
      {
        return GetCollection<HRJobPosition>("JobPositions");
      }
    }
    [Association("HRJobPositionRevision-HRActivity")]
    public XPCollection<HRJobPositionRevision> JobPositionsRevisions
    {
      get
      {
        return GetCollection<HRJobPositionRevision>("JobPositionsRevisions");
      }
    }
    [Association("HRActivity-HREmployee")]
    public XPCollection<HREmployee> ActingAsEmployees
    {
      get
      {
        return GetCollection<HREmployee>("ActingAsEmployees");
      }
    }
    private Frequency _frequency;
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    [RuleRequiredField]
    public Frequency Frequency
    {
      get
      {
        return _frequency;
      }
      set
      {
        SetPropertyValue("Frequency", ref _frequency, value);
      }
    }
    private RecurringPeriod _frequencyType;
    // [Appearance("Frequency", AppearanceItemType = "ViewItem", TargetItems = "FrequencyType", Criteria = "Frequency !='Recurring'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    // [Appearance("Frequency",TargetItems = "FrequencyType", Criteria = "Frequency !='Recurring'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    [ImmediatePostDataAttribute]
    public RecurringPeriod FrequencyType
    {
      get
      {
        return _frequencyType;
      }
      set
      {
        SetPropertyValue("FrequencyType", ref _frequencyType, value);
      }
    }
    private bool _sat;
    private bool _sun;
    private bool _mon;
    private bool _tue;
    private bool _wed;
    private bool _Thu;
    private bool _fri;
    public bool Sat
    {
      get
      {
        return _sat;
      }
      set
      {
        _sat = value;
      }
    }
    public bool Sun
    {
      get
      {
        return _sun;
      }
      set
      {
        _sun = value;
      }
    }
    public bool Mon
    {
      get
      {
        return _mon;
      }
      set
      {
        _mon = value;
      }
    }
    public bool Tue
    {
      get
      {
        return _tue;
      }
      set
      {
        _tue = value;
      }
    }
    public bool Wed
    {
      get
      {
        return _wed;
      }
      set
      {
        _wed = value;
      }
    }
    public bool Thu
    {
      get
      {
        return _Thu;
      }
      set
      {
        _Thu = value;
      }
    }
    public bool Fri
    {
      get
      {
        return _fri;
      }
      set
      {
        _fri = value;
      }
    }
    private double _duration;
    public double Duration
    {
      get
      {
        return _duration;
      }
      set
      {
        _duration = value;
      }
    }
    private HR_Role _role;
    [Association("HRRole-Activities")]
    [DisplayName("Role")]
      [ImmediatePostData]
    public HR_Role HRRole
    {
      get
      {
        return _role;
          
      }
      set
      {
          SetPropertyValue("HRRole", ref _role, value);
      }
    }
    private string _initiator;
    public string Initiator
    {
      get
      {
        return _initiator;
      }
      set
      {
        _initiator = value;
      }
    }
    private string _detaillink;
    [RuleRegularExpression("HRDetailLink.RuleRegularExpression", DefaultContexts.Save, @"(((http|https|ftp)\://)?[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}(:[a-zA-Z0-9]*)?/?([a-zA-Z0-9\-\._\?\,\'/\\\+&amp;%\$#\=~])*)|([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6})")]
    [EditorAlias("HyperLinkStringPropertyEditor")]
    [Size(1000)]
    public string DetailLink
    {
      get
      {
        return _detaillink;
      }
      set
      {
        _detaillink = value;
      }
    }
    [Association("HRActivity-HR-ActivityLog")]
    public XPCollection<HR_ActivityLog> UserActivityList
    {
      get
      {
        return GetCollection<HR_ActivityLog>("UserActivityList");
      }
    }
    private EntityType _type;
    public EntityType Type
    {
      get
      {
        return _type;
      }
      set
      {
        SetPropertyValue("Type", ref _type, value);
      }
    }
  }
  public enum Frequency
  {
    [DisplayName("Upon Request")]
    UponRequest,
    Recurring
  }
  public enum RecurringPeriod
  {
    Select,
    Daily,
    Weekly,
    Monthly,
    Quarterly,
    [DisplayName("Half Annualy")]
    HalfAnnualy,
    Annualy
  }
}
