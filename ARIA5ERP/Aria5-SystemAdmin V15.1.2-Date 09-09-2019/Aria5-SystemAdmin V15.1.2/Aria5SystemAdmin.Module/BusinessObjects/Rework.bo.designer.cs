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
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.ConditionalAppearance;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-Rework")]
 // [Appearance("QualityFields", AppearanceItemType = "ViewItem", TargetItems = "TaskLink,Reviewer,TestedBy,Programmer,Type,TaskNumber,Application,ProductManager,TeamLeader,Ticketorproject", Criteria = "!IsCurrentUserInRole('Quality')", Context = "DetailView", Enabled = false)]
  //[Appearance("DevlopmentFields", AppearanceItemType = "ViewItem", TargetItems = "PermanentSolution,TempSolution", Criteria = "!IsCurrentUserInRole('Team Leader')", Context = "DetailView", Enabled = false)]
 // [Appearance("CompleteAction", AppearanceItemType = "Action", TargetItems = "Complete", Criteria = "!IsCurrentUserInRole('Quality')", Context = "DetailView", Enabled = false)]
  //[Appearance("Reviewer", AppearanceItemType = "ViewItem", TargetItems = "Reviewer", Criteria = "!IsCurrentUserInRole('Quality')", Context = "DetailView", Enabled = false)]

  public partial class Rework : DevExpress.Persistent.BaseImpl.BaseObject
  {
    public Rework(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private string _seqnum;
    public string SeqNum
    {
      get
      {
        return _seqnum;
      }
      set
      {
        _seqnum = value;
      }
    }
    private Application_T _application;
    [ImmediatePostData]
    public Application_T Application
    {
      get
      {
        if (_application != null && this.ProductManager == null)
        {
          if (_application.Name.ToUpper().Contains("EDI"))
          {
            this.ProductManager = this.Session.FindObject<Resources>(CriteriaOperator.Parse("Name = 'HeshamElmasry'"));
          }
          else
            if (_application.Name.ToUpper().Contains("XP"))
            {
              this.ProductManager = this.Session.FindObject<Resources>(CriteriaOperator.Parse("Name = 'HassanIbrahim'"));
            }
        }
        return _application;
      }
      set
      {
        SetPropertyValue("Application", ref _application, value);
      }
    }
    private Resources _productManager;
    public Resources ProductManager
    {
      get
      {
        return _productManager;
      }
      set
      {
        _productManager = value;
      }
    }
    private Resources _teamLeader;
    public Resources TeamLeader
    {
      get
      {
        return _teamLeader;
      }
      set
      {
        _teamLeader = value;
      }
    }
    private string _ticketorproject;
    public string Ticketorproject
    {
      get
      {
        return _ticketorproject;
      }
      set
      {
        _ticketorproject = value;
      }
    }
    private string _taskNumber;
    public string TaskNumber
    {
        get
        {
            return _taskNumber;
        }
        set
        {
            _taskNumber = value;
        }
    }
    private string _tasklink;
    [RuleRegularExpression("Tasklink.RuleRegularExpression", DefaultContexts.Save, @"(((http|https|ftp)\://)?[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}(:[a-zA-Z0-9]*)?/?([a-zA-Z0-9\-\._\?\,\'/\\\+&amp;%\$#\=~])*)|([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6})")]
    [EditorAlias("HyperLinkStringPropertyEditor")]
    [Size(1000)]
    public string TaskLink
    {
        get
        {
            return _tasklink;
        }
        set
        {
            SetPropertyValue("TaskLink", ref _tasklink, value);
        }
    }
    private DateTime _testedDate;
    public DateTime TestedDate
    {
      get
      {
        return _testedDate;
      }
      set
      {
        _testedDate = value;
      }
    }
    private Resources _testedby;
    public Resources TestedBy
    {
      get
      {
        return _testedby;
      }
      set
      {
        _testedby = value;
      }
    }
    private string _reworkFeedback;
    [RuleRequiredField]
    [Size(4000)]
    public string ReworkFeedback
    {
      get
      {
        return _reworkFeedback;
      }
      set
      {
        _reworkFeedback = value;
      }
    }
    private Resources _programmer;
    public Resources Programmer
    {
      get
      {
        return _programmer;
      }
      set
      {
        _programmer = value;
      }
    }
    private Resources _reviewer;
    public Resources Reviewer
    {
        get
        {
            return _reviewer;
        }
        set
        {
            SetPropertyValue("Reviewer", ref _reviewer, value);
        }
    }
    //private string _programmerfeedback;
    //[Size(4000)]
    //public string ProgrammerFeedback
    //{
    //  get
    //  {
    //    return _programmerfeedback;
    //  }
    //  set
    //  {
    //    _programmerfeedback = value;
    //  }
    //}
      //ATA enhancment in the reowrk screen 3/21/2017 [start]
    private Reworktype? _type;
    public Reworktype? Type
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
    private ReworkConfirmation? _isRework;
    public ReworkConfirmation? IsRework
    {
        get
        {
            return _isRework;
        }
        set
        {
            SetPropertyValue("IsRework", ref _isRework, value);
        }
    }
    private RoutCauseCategory? _rootCauseCategory;
    public RoutCauseCategory? RootCauseCategory
    {
        get
        {
            return _rootCauseCategory;
        }
        set
        {
            SetPropertyValue("RootCauseCategory", ref _rootCauseCategory, value);
        }
    }
    private ReworkStatus _status;
    public ReworkStatus Status
  {
      get
      {
          return _status;
      }
      set
      {
          SetPropertyValue("Status", ref _status, value);

      }
  }
  private string _tempSolution;
      [Size(3000)]
  public string TempSolution
  {
      get
      {
          return _tempSolution;
      }
      set
      {
          SetPropertyValue("TempSolution", ref _tempSolution, value);
      }
  }
  private string _permSolution;
       [Size(3000)]
  public string PermanentSolution
  {
      get
      {
          return _tempSolution;
      }
      set
      {
          SetPropertyValue("PermanentSolution", ref _permSolution, value);
      }
  }
    [Association("Rework-Feedbacks")]
    public XPCollection<Reworkfeedback> DiscussionBoard
    {
      get
      {
        return GetCollection<Reworkfeedback>("DiscussionBoard");
      }
    }
    //ATA enhancment in the reowrk screen 3/21/2017 [start]

    private XPCollection<AuditDataItemPersistent> changeHistory;
    public XPCollection<AuditDataItemPersistent> ChangeHistory
    {
      get
      {
        if (changeHistory == null)
        {
          changeHistory = AuditedObjectWeakReference.GetAuditTrail(Session, this);
        }
        return changeHistory;
      }
    }
  }
  public enum Reworktype
  {
      Internal,
      External
  }
  public enum ReworkConfirmation
  {
      Yes,
      No
  }
  public enum ReworkStatus
  {
      New,
      InWork,
      Complete
  }
  public enum RoutCauseCategory
  {
      Development,
      Requirement,
      Review,
      System,
      Environment,
      testingCriteria,
      TestCases
  }
}
