using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.Editors;
using DevExpress.Data.Filtering;
using Aria5SystemAdmin.Module.BusinessObjects;
using System.ComponentModel;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  //  [Appearance("Application", AppearanceItemType = "ViewItem", TargetItems = "Application", Criteria = "ProjectTemplate!=null", Context = "DetailView", Enabled = false)]
  //ATA add new role 
  [Appearance("TrackingStatus", AppearanceItemType = "ViewItem", TargetItems = "Status", Criteria = "Status ='New'", Context = "DetailView", Enabled = false)]
  [Appearance("TrackingEntityvisability", AppearanceItemType = "ViewItem", TargetItems = "Entity", Criteria = "ModificationType == 'Add'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
  [Appearance("TrackingEntitynamevisability", AppearanceItemType = "ViewItem", TargetItems = "ObjectName", Criteria = "ModificationType != 'Add'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
  [Appearance("ChildTrackingEntriesappearance", AppearanceItemType = "ViewItem", TargetItems = "ChildTrackingEntries", Criteria = "HasChildren == false", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
  [DefaultClassOptions]
  [XafDefaultProperty("ID")]
  [RelatedEntity("Aria5-SystemAdmin-TrackingEntry")]
  [OptimisticLocking(OptimisticLockingBehavior.NoLocking)]
  public partial class TrackingEntry : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.DateTime _approveDate;
    private System.String _approvedBy;
    private Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity _projectEntity;
    private System.String _checkInComment;
    private System.String _ticketNumber;
    private System.Boolean _disable;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObject _entity;
    private Session sessionTrackingEntry;
    private System.String _objectName;
    private Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry.ModificationTypes _modificationType;
    private System.Boolean _isNew;
    private System.String _state;
    private System.Boolean _hasChildren;
    private Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry _parentTrackingEntry;
    private System.String _technicalInformation;
    private System.String _releaseNote;
    private System.String _description;
    private System.String _referenceNo;
    private System.String _referenceTitle;
    private System.Int32 _buildNo;
    private System.Int32 _servicePackNo;
    private System.Int32 _releaseNo;
    private System.DateTime _completeDate;
    private System.DateTime _requestedDate;
    private System.DateTime _enteredDate;
    private System.String _accountID;
    private Aria5SystemAdmin.Module.BusinessObjects.Account _account;
    private System.String _accountName;
    private System.Int32 _iD;
    private TrackingPriority _priority;
    private Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate _projectTemplate;
    private Aria5SystemAdmin.Module.BusinessObjects.Application_T _application;
    private System.String _subject;
    private TrackingStatus _status;
    public enum TrackingType
    {
      Bug = 0,
      New = 1,
      Custom = 2,
      Enhancement = 3,
      TBuild = 4
    };
    public enum TrackingStatus
    {
      New,
      InWork,
      OnHold,
      Cancelled,
      Staging,
      ReadyForInstallation,
      AccountTesting,
      Complete,
      ReleaseCandidate
    };
    public enum TrackingPriority
    {
      VeryUrgent,
      Urgent,
      Normal,
      Low
    }
    private TrackingType _type;
    public TrackingEntry(DevExpress.Xpo.Session session)
      : base(session)
    {
      sessionTrackingEntry = session;
    }
    public System.Int32 ID
    {
      get
      {
        return _iD;
      }
      set
      {
        SetPropertyValue("ID", ref _iD, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(1)]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public TrackingType Type
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
    public TrackingStatus Status
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
    [DevExpress.Xpo.SizeAttribute(150)]
    public System.String Subject
    {
      get
      {
        return _subject;
      }
      set
      {
        SetPropertyValue("Subject", ref _subject, value);
      }
    }
    private string _trackingFix;
    [EditorAlias("HyperLinkStringPropertyEditor")]
    public string TrackingFix
    {
      get
      {
        return _trackingFix;
      }
      set
      {
        SetPropertyValue("TrackingFix", ref _trackingFix, value);
      }
    }
    public System.Int32 ReleaseNo
    {
      get
      {
        return _releaseNo;
      }
      set
      {
        SetPropertyValue("ReleaseNo", ref _releaseNo, value);
      }
    }
    public System.Int32 ServicePackNo
    {
      get
      {
        return _servicePackNo;
      }
      set
      {
        SetPropertyValue("ServicePackNo", ref _servicePackNo, value);
      }
    }
    public System.Int32 BuildNo
    {
      get
      {
        return _buildNo;
      }
      set
      {
        SetPropertyValue("BuildNo", ref _buildNo, value);
      }
    }
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public Aria5SystemAdmin.Module.BusinessObjects.Application_T Application
    {
      get
      {
        return _application;
      }
      set
      {
        SetPropertyValue("Application", ref _application, value);
        try
        {
          this.ReleaseNo = value.ReleaseNo == null ? 0 : int.Parse(value.ReleaseNo);
          this.ServicePackNo = value.ServicePack == null ? 0 : int.Parse(value.ServicePack);
        }
        catch (Exception ex)
        {
        }
      }
    }
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    [Association("ProjectTemplate-TrackingEntries")]
    public Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate ProjectTemplate
    {
      get
      {
        return _projectTemplate;
      }
      set
      {
        SetPropertyValue("ProjectTemplate", ref _projectTemplate, value);
        if (!IsSaving && !IsLoading && value != null)
        {
          this.Account = value.Account;
        }
      }
    }
    public TrackingPriority Priority
    {
      get
      {
        return _priority;
      }
      set
      {
        SetPropertyValue("Priority", ref _priority, value);
      }
    }
    //  [Appearance("Single", Visibility = ViewItemVisibility.Hide, Criteria = "!(Type==2 or Type=0)")]
    public System.String AccountID
    {
      get
      {
        return _accountID;
      }
      set
      {
        SetPropertyValue("AccountID", ref _accountID, value);
      }
    }
    //[Appearance("Single", Visibility = ViewItemVisibility.Hide, Criteria = "!(Type==2 or Type=0)")]
    public System.String AccountName
    {
      get
      {
        return _accountName;
      }
      set
      {
        SetPropertyValue("AccountName", ref _accountName, value);
      }
    }
    //[Appearance("Single", Visibility = ViewItemVisibility.Hide, Criteria = "!(Type==2 or Type=0)")]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public Aria5SystemAdmin.Module.BusinessObjects.Account Account
    {
      get
      {
        return _account;
      }
      set
      {
        SetPropertyValue("Account", ref _account, value);
        if (this.Account != null)
          this.AccountID = this.Account.AccountCode;
        if (this.Account != null)
          this.AccountName = this.Account.Name;
      }
    }
    public System.DateTime EnteredDate
    {
      get
      {
        return _enteredDate;
      }
      set
      {
        SetPropertyValue("EnteredDate", ref _enteredDate, value);
      }
    }
    public System.DateTime RequestedDate
    {
      get
      {
        return _requestedDate;
      }
      set
      {
        SetPropertyValue("RequestedDate", ref _requestedDate, value);
      }
    }
    public System.DateTime CompleteDate
    {
      get
      {
        return _completeDate;
      }
      set
      {
        SetPropertyValue("CompleteDate", ref _completeDate, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(-1)]
    [RuleRequiredField]
    public System.String Description
    {
      get
      {
        return _description;
      }
      set
      {
        SetPropertyValue("Description", ref _description, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(-1)]
    public System.String ReleaseNote
    {
      get
      {
        return _releaseNote;
      }
      set
      {
        SetPropertyValue("ReleaseNote", ref _releaseNote, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(-1)]
    public System.String TechnicalInformation
    {
      get
      {
        return _technicalInformation;
      }
      set
      {
        SetPropertyValue("TechnicalInformation", ref _technicalInformation, value);
      }
    }
    public System.String ReferenceTitle
    {
      get
      {
        return _referenceTitle;
      }
      set
      {
        SetPropertyValue("ReferenceTitle", ref _referenceTitle, value);
      }
    }
    [RuleRequiredField(TargetCriteria = "ProjectTemplate is not Null and ProjectTemplate.Type = 'Standard'")]
    public System.String ReferenceNo
    {
      get
      {
        return _referenceNo;
      }
      set
      {
        SetPropertyValue("ReferenceNo", ref _referenceNo, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("TrackingEntryAttachments-TrackingEntry")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TrackingEntryAttachment> TrackingEntryAttachments
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TrackingEntryAttachment>("TrackingEntryAttachments");
      }
    }
    //ATA add a real relation between parent and childs trackings 10/12/2017[Start]
    [Association("ParentTrackingEntry-Children")]
    [DataSourceCriteria("HasChildren == false")]
    public XPCollection<TrackingEntry> ChildTrackingEntries
    {
      get
      {
        //ATA comment old relation and create new one 
        //this.HasChildren = this.Oid.ToString();
        //return new XPCollection<TrackingEntry>(sessionTrackingEntry, CriteriaOperator.Parse("ParentTrackingEntry='" + this.Oid + "'"));
        return GetCollection<TrackingEntry>("ChildTrackingEntries");
      }
    }
    //ATA add a real relation between parent and childs trackings [END]
    [Association("ParentTrackingEntry-Children")]
    [DataSourceCriteria("HasChildren == True")]
    public Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry ParentTrackingEntry
    {
      get
      {
        return _parentTrackingEntry;
      }
      set
      {
        SetPropertyValue("ParentTrackingEntry", ref _parentTrackingEntry, value);
      }
    }
    //[DevExpress.Xpo.NonPersistentAttribute]
    //[DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    //[DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    //[DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    public System.Boolean HasChildren
    {
      get
      {
        return _hasChildren;
      }
      set
      {
        SetPropertyValue("HasChildren", ref _hasChildren, value);
      }
    }
    [DevExpress.Xpo.NonPersistentAttribute]
    public System.String State
    {
      get
      {
        return _state;
      }
      set
      {
        SetPropertyValue("State", ref _state, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("AriaObjectShelves-TrackingEntry")]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelve> AriaObjectShelves
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelve>("AriaObjectShelves");
      }
    }
    [DevExpress.Xpo.NonPersistentAttribute]
    public System.Boolean IsNew
    {
      get
      {
        return _isNew;
      }
      set
      {
        SetPropertyValue("IsNew", ref _isNew, value);
      }
    }
    [ImmediatePostData]
    public Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry.ModificationTypes ModificationType
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
    public System.String ObjectName
    {
      get
      {
        return _objectName;
      }
      set
      {
        SetPropertyValue("ObjectName", ref _objectName, value);
      }
    }
    //ATA add this association to relate this tracking entry with the aria object 2/22/2017 [6368][Start]
    //ATA add this association to relate this tracking entry with the aria object 2/22/2017 [6368][End]
    //ATA add validation role for the not parent tracking 
    [DevExpress.Xpo.AssociationAttribute("AriaObject-TrackingEntries")]
    [RuleRequiredField(TargetCriteria = "HasChildren = false")]
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObject Entity
    {
      get
      {
        return _entity;
      }
      set
      {
        SetPropertyValue("Entity", ref _entity, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("TrackingTasks-TrackingEntry")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TrackingTask> TrackingTasks
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TrackingTask>("TrackingTasks");
      }
    }
    //ATA add test cases and testplans on tracking entry level  10/12/2017 [start]
    [Association("Trackingentry-TestCases")]
    public XPCollection<TestCase> TestCases
    {
      get
      {
        return GetCollection<TestCase>("TestCases");
      }
    }
    [Association("TrackingEntry-TestPlans")]
    public XPCollection<QATestPlan> TestPlans
    {
      get
      {
        return GetCollection<QATestPlan>("TestPlans");
      }
    }
    [Association("TrackingEntry-Defects")]
    public XPCollection<QADefect> Defects
    {
      get
      {
        return GetCollection<QADefect>("Defects");
      }
    }
    public System.Boolean Disable
    {
      get
      {
        return _disable;
      }
      set
      {
        SetPropertyValue("Disable", ref _disable, value);
      }
    }
    private Aria5SystemAdmin.Module.BusinessObjects.ApplicationBuild_T _ApplicationBuild;
    [DevExpress.Xpo.AssociationAttribute("ApplicationBuild_T-TrackingEntrys")]
    public ApplicationBuild_T ApplicationBuild
    {
      get
      {
        return _ApplicationBuild;
      }
      set
      {
        SetPropertyValue("ApplicationBuild", ref _ApplicationBuild, value);
      }
    }
    [Browsable(false)]
    public System.String TicketNumber
    {
      get
      {
        return _ticketNumber;
      }
      set
      {
        SetPropertyValue("TicketNumber", ref _ticketNumber, value);
      }
    }
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
    [DevExpress.Xpo.AssociationAttribute("TestRuns-TrackingEntry")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TestRun> TestRuns
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TestRun>("TestRuns");
      }
    }
    public System.String CheckInComment
    {
      get
      {
        return _checkInComment;
      }
      set
      {
        SetPropertyValue("CheckInComment", ref _checkInComment, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityFeatures-TrackingEntry")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAMainFeature> EntityFeatures
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAMainFeature>("EntityFeatures");
      }
    }
    //ATA,2[start] moving detail design under tracking entry instead of projectenitiy
    [DevExpress.Xpo.AssociationAttribute("EntitySystemDesign-TrackingEntry")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntitySystemDesign> EntitySystemDesign
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntitySystemDesign>("EntitySystemDesign");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("DetailDesignEstimates-TrackingEntry")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.DesignDetailEstimate> DetailDesignEstimates
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.DesignDetailEstimate>("DetailDesignEstimates");
      }
    }
    //  ATA , 1, 8/5/2016 enhance [start]
    [DevExpress.Xpo.AssociationAttribute("UserInterfaceRules-TrackingEntry")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUserInterfaceRules> UserInterfaceRules
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUserInterfaceRules>("UserInterfaceRules");
      }
    }
    // ATA , 1, 8/5/2016 enhance [end]
    //  ATA,2[END] moving detail design under tracking entry instead of projectenitiy
    // [RuleRequiredField]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    [Browsable(false)]
    public Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity ProjectEntity
    {
      get
      {
        if (_projectEntity != null)
        {
          if (_projectEntity.EntitySystemDesign != null)
          {
            foreach (EntitySystemDesign entitydesign in _projectEntity.EntitySystemDesign)
            {
              if (!EntitySystemDesign.Contains(entitydesign))
              {
                entitydesign.TrackingEntry = this;
                //ATA
                //entitydesign.Save();
                //entitydesign.Session.CommitTransaction();
                //ATA
                //EntitySystemDesign.Add(entitydesign);
              }
            }
          }
          //ATA userinterfacerules added in tracking [start ]
          if (_projectEntity.UserInterfaceRules != null)
          {
            foreach (QAUserInterfaceRules userinterface in _projectEntity.UserInterfaceRules)
            {
              if (!UserInterfaceRules.Contains(userinterface))
              {
                userinterface.TrackingEntry = this;
                //userinterface.Save();
                //userinterface.Session.CommitTransaction();
                //EntitySystemDesign.Add(entitydesign);
              }
            }
          }
          //ATA userinterfacerules added in tracking [end ]
          if (_projectEntity.DetailDesignEstimates != null)
          {
            foreach (DesignDetailEstimate designestimate in _projectEntity.DetailDesignEstimates)
            {
              designestimate.TrackingEntry = this;
              //  DetailDesignEstimates.Add(designestimate);
            }
          }
          if (_projectEntity.MainFeatures != null)
          {
            foreach (QAMainFeature featue in _projectEntity.MainFeatures)
            {
              featue.TrackingEntry = this;
            }
          }
        }
        return _projectEntity;
      }
      set
      {
        SetPropertyValue("ProjectEntity", ref _projectEntity, value);
        //if (this.ProjectEntity != null)
        //{
        //    foreach (QAMainFeature item in this.EntityFeatures)
        //    {
        //        this.EntityFeatures.Remove(item);
        //    }
        //    this.EntityFeatures.AddRange(this.ProjectEntity.MainFeatures);
        //}
      }
    }
    //ATA add attachment that called technical documentation 
    private QAAttachement _technicaldoc;
    public QAAttachement TechnicalDocumentation
    {
      get
      {
        return _technicaldoc;
      }
      set
      {
        SetPropertyValue("Attachemnt", ref _technicaldoc, value);
      }
    }
    public System.String ApprovedBy
    {
      get
      {
        return _approvedBy;
      }
      set
      {
        SetPropertyValue("ApprovedBy", ref _approvedBy, value);
      }
    }
    public System.DateTime ApproveDate
    {
      get
      {
        return _approveDate;
      }
      set
      {
        SetPropertyValue("ApproveDate", ref _approveDate, value);
      }
    }
  }
}
