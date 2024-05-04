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
using DevExpress.Persistent.Base.General;
using System.ComponentModel;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  //[Appearance("Name", AppearanceItemType = "ViewItem", TargetItems = "Name", Criteria = "Type !='Custom'", Context = "DetailView", Enabled = false)]
  [Appearance("EndDate", AppearanceItemType = "ViewItem", TargetItems = "EndDate", Criteria = "Type='Key'", Context = "DetailView", Enabled = false)]
    // [Appearance("ProjectRefrence", AppearanceItemType = "ViewItem", TargetItems = "ProjectReference", Criteria = "Type !='Key'", Context = "DetailView", Enabled = false)]
   

    [DefaultClassOptions]
  public partial class ProjectTemplate : BaseObject, ISupportNotifications, ICloneable
  {
    private System.String _prjctArchDision;
    private System.String _prjctArchUMLDigrm;
    private Aria5SystemAdmin.Module.BusinessObjects.QAUseCasePoints _usecasePoints;
    private System.DateTime _endDate;
    private System.Int16 _testCasePassRatio;
    private System.Double _completness;
    private Aria5SystemAdmin.Module.BusinessObjects.Application_T _application;
    private System.DateTime _startDate;
    private System.String _description;
    private System.String _name;
    //ATA
    private System.Double _entityspecificationestimation;
    private System.Double _entityspecificationactual;
    private System.Double _phaseM0;
    private System.Double _phaseM1;
    private System.Double _phaseM2;
    private System.Double _phaseM3;
    //ATA 19/6/2016
    //ATA 19/7/2016 [start]
    private status _phaseM0_statues;
    private status _phaseM1_statues;
    private status _phaseM2_statues;
    private status _phaseM3_statues;
    private string _phaseM0_SPI;
    private string _phaseM1_SPI;
    private string _phaseM2_SPI;
    private string _phaseM3_SPI;
    //ATA 19/7/2016 [End]
    private status? _status;
    private Resources _projectowner;
    //ATA 19/6/2016
    //ATA
    public ProjectTemplate(DevExpress.Xpo.Session session)
      : base(session)
    {
    }

    

    //[DevExpress.Xpo.KeyAttribute]
    //ATA add rule required field attribute to be mandatory 1/5/2017[start]
    //ATA add rule required field attribute to be mandatory 1/5/2017[End]
    // [RuleRequiredField]
    [RuleUniqueValue]
    [SizeAttribute(200)]
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
    [DevExpress.Xpo.SizeAttribute(4000)]
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
    //ATA add rule required field attribute to be mandatory 1/5/2017[start]
    //ATA add rule required field attribute to be mandatory 1/5/2017[End]
    [DevExpress.Xpo.AssociationAttribute("ProjectTemplates-Application_T")]
    [RuleRequiredField]
    [ImmediatePostData]
    public Aria5SystemAdmin.Module.BusinessObjects.Application_T Application
    {
      get
      {
        return _application;
      }
      set
      {
        SetPropertyValue("Application", ref _application, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Requirements-ProjectTemplate")]
    [DataSourceCriteria("RequirementType.Name == 'Feature'")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Requirement> Requirements
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Requirement>("Requirements");
      }
    }
    //ATA make start date required [start]
    //ATA make start date required [end]
    [RuleRequiredField]
    [ImmediatePostData(true)]
    public System.DateTime StartDate
    {
      get
      {
        return _startDate;
      }
      set
      {
        SetPropertyValue("StartDate", ref _startDate, value);
        EndDate = CalcEndate.CalcEnddate(value, 4 - ((int)StartFrom));
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ProjectTemplates-Activities")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Activity> Activities
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Activity>("Activities");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("QAProjectEntities-ProjectTemplate")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity> ProjectEntities
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity>("ProjectEntities");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("QAUseCases-ProjectTemplate")]
    [DevExpress.Xpo.AggregatedAttribute]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUseCase> UseCases
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUseCase>("UseCases");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Environments-ProjectTemplate")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Environment> Environments
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Environment>("Environments");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("QADefects-ProjectTemplates")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QADefect> Defects
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QADefect>("Defects");
      }
    }
    [DevExpress.Persistent.Base.EditorAlias("CircularGaugePropertyEditor")]
    public System.Double Completness
    {
      get
      {
        return _completness;
      }
      set
      {
        SetPropertyValue("Completness", ref _completness, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Commentses-ProjectTemplate")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Comments> Commentses
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Comments>("Commentses");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("TestRuns-ProjectTemplate")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TestRun> TestRuns
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TestRun>("TestRuns");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("QAAttachements-ProjectTemplate")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAAttachement> Attachements
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAAttachement>("Attachements");
      }
    }
    public System.Int16 TestCasePassRatio
    {
      get
      {
        return _testCasePassRatio;
      }
      set
      {
        SetPropertyValue("TestCasePassRatio", ref _testCasePassRatio, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("TestCases-ProjectTemplate")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TestCase> TestCases
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TestCase>("TestCases");
      }
    }
    [RuleRequiredField]
    public System.DateTime EndDate
    {
      get
      {
        return _endDate;
      }
      set
      {
        SetPropertyValue("EndDate", ref _endDate, value);
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
    [DevExpress.Xpo.AssociationAttribute("Risks-ProjectTemplate")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QARisk> Risks
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QARisk>("Risks");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("TestPlans-ProjectTemplate")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QATestPlan> TestPlans
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QATestPlan>("TestPlans");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ManagementIssues-ProjectTemplate")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAManagementIssues> ManagementIssues
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAManagementIssues>("ManagementIssues");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ResourceShares-ProjectTemplate")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAResourceShare> ResourceShares
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAResourceShare>("ResourceShares");
      }
    }
    //ATA add new validation to usecase points 
    [RuleRequiredField(TargetCriteria = "Type = 'Key' and (ProjectReference is null or Type = 'Key')")]
    public Aria5SystemAdmin.Module.BusinessObjects.QAUseCasePoints UseCasePoints
    {
      get
      {
        return _usecasePoints;
      }
      set
      {
        SetPropertyValue("UseCasePoints", ref _usecasePoints, value);
      }
    }
    //[DevExpress.Xpo.AssociationAttribute("QAWBSs-ProjectTemplate")]
    //public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAWBS> QAWBSs
    //{
    //  get
    //  {
    //    return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAWBS>("QAWBSs");
    //  }
    //}
    [DevExpress.Xpo.SizeAttribute(200)]
    public System.String PrjctArchUMLDigrm
    {
      get
      {
        return _prjctArchUMLDigrm;
      }
      set
      {
        SetPropertyValue("PrjctArchUMLDigrm", ref _prjctArchUMLDigrm, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(200)]
    public System.String PrjctArchDision
    {
      get
      {
        return _prjctArchDision;
      }
      set
      {
        SetPropertyValue("PrjctArchDision", ref _prjctArchDision, value);
      }
    }
    private System.Int64 _AutoTaskID;
    public System.Int64 AutoTaskID
    {
      get
      {
        return _AutoTaskID;
      }
      set
      {
        SetPropertyValue("AutoTaskID", ref _AutoTaskID, value);
      }
    }
    //ATA set the default of enum field to be null and required in all iteration screen 1/11/2017 [start]
    private ProjectType? _ProjectType;
    //ATA add rule required field attribute to be mandatory 1/5/2017[start]
    //ATA add rule required field attribute to be mandatory 1/5/2017[End]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    [RuleRequiredField]
    public ProjectType? Type
    {
      //ATA set the default of enum field to be null and required in all iteration screen 1/11/2017 [End]
      get
      {
        return _ProjectType;
      }
      set
      {
        SetPropertyValue("Type", ref _ProjectType, value);
      }
    }
    [DevExpress.Persistent.Base.EditorAlias("CircularGaugePropertyEditor")]
    public System.Double PhaseM0
    {
      get
      {
        return _phaseM0;
      }
      set
      {
        SetPropertyValue("PhaseM0", ref _phaseM0, value);
      }
    }
    [DevExpress.Persistent.Base.EditorAlias("CircularGaugePropertyEditor")]
    public System.Double PhaseM1
    {
      get
      {
        return _phaseM1;
      }
      set
      {
        SetPropertyValue("PhaseM0", ref _phaseM1, value);
      }
    }
    [DevExpress.Persistent.Base.EditorAlias("CircularGaugePropertyEditor")]
    public System.Double PhaseM2
    {
      get
      {
        return _phaseM2;
      }
      set
      {
        SetPropertyValue("PhaseM0", ref _phaseM2, value);
      }
    }
    [DevExpress.Persistent.Base.EditorAlias("CircularGaugePropertyEditor")]
    public System.Double PhaseM3
    {
      get
      {
        return _phaseM3;
      }
      set
      {
        SetPropertyValue("PhaseM0", ref _phaseM3, value);
      }
    }
    //ATA enhance gauges [start]
    public status PhaseM0_statues
    {
      get
      {
        return _phaseM0_statues;
      }
      set
      {
        SetPropertyValue("PhaseM0_statues", ref _phaseM0_statues, value);
      }
    }
    public status PhaseM1_statues
    {
      get
      {
        return _phaseM1_statues;
      }
      set
      {
        SetPropertyValue("PhaseM1_statues", ref _phaseM1_statues, value);
      }
    }
    public status PhaseM2_statues
    {
      get
      {
        return _phaseM2_statues;
      }
      set
      {
        SetPropertyValue("PhaseM2_statues", ref _phaseM2_statues, value);
      }
    }
    public status PhaseM3_statues
    {
      get
      {
        return _phaseM3_statues;
      }
      set
      {
        SetPropertyValue("PhaseM3_statues", ref _phaseM3_statues, value);
      }
    }
    public string PhaseM0_SPI
    {
      get
      {
        return _phaseM0_SPI;
      }
      set
      {
        SetPropertyValue("PhaseM0_SPI", ref _phaseM0_SPI, value);
      }
    }
    public string PhaseM1_SPI
    {
      get
      {
        return _phaseM1_SPI;
      }
      set
      {
        SetPropertyValue("PhaseM1_SPI", ref _phaseM1_SPI, value);
      }
    }
    public string PhaseM2_SPI
    {
      get
      {
        return _phaseM2_SPI;
      }
      set
      {
        SetPropertyValue("PhaseM2_SPI", ref _phaseM2_SPI, value);
      }
    }
    public string PhaseM3_SPI
    {
      get
      {
        return _phaseM3_SPI;
      }
      set
      {
        SetPropertyValue("PhaseM3_SPI", ref _phaseM3_SPI, value);
      }
    }
    //ATA enhance gauges [end]
    public System.Double EntitySpecificationEstimation
    {
      get
      {
        return _entityspecificationestimation;
      }
      set
      {
        SetPropertyValue("EntitySpecificationEstimation", ref _entityspecificationestimation, value);
      }
    }
    public System.Double EntitySpecificationActual
    {
      get
      {
        return _entityspecificationactual;
      }
      set
      {
        SetPropertyValue("EntitySpecificationActual", ref _entityspecificationactual, value);
      }
    }
    //ATA Add project status and owner  20/6/2016 [start ]
    //ATA set the default of enum field to be null and required in all iteration screen 1/11/2017 [start]
    [RuleRequiredField]
    public status? Status
    {
      //ATA set the default of enum field to be null and required in all iteration screen 1/11/2017 [End]
      get
      {
        return _status;
      }
      set
      {
        SetPropertyValue("Status", ref _status, value);
      }
    }
    //ATA add rule required field attribute to be mandatory 1/5/2017[start]
    //ATA add rule required field attribute to be mandatory 1/5/2017[End]
    [RuleRequiredField]
    public Resources ProjectOwner
    {
      get
      {
        return _projectowner;
      }
      set
      {
        SetPropertyValue("ProjectOwner", ref _projectowner, value);
      }
    }
    [Association("Projecttemplate-ProjectScopes")]
    public XPCollection<ProjectScope> Scope
    {
      get
      {
        return GetCollection<ProjectScope>("Scope");
      }
    }
    //ATA adding a direct relation between project and it's phases 9/21/2017 [start]
    [Association("Project-Phases")]
    public XPCollection<QAWBS> ProjectWBS
    {
      get
      {
        return GetCollection<QAWBS>("ProjectWBS");
      }
    }
        //ATA adding a direct relation between project and it's phases 9/21/2017 [End]
        //ATA add refrience project and start in fields to work with one phase key projects that depend on previous projects 9/21/2017[start]
        //Doaa
        private string _projectReference;
    // [ImmediatePostData]
   // [DataSourceProperty("Application.ProjectTemplates")]
    public string ProjectReference
    {
      get
      {
        return _projectReference;
      }
      set
      {
        SetPropertyValue("ProjectReference", ref _projectReference, value);
      }
    }
    private Phases _startFrom;
    public Phases StartFrom
    {
      get
      {
        return _startFrom;
      }
      set
      {
        _startFrom = value;
      }
    }
    public enum Phases
    {
      M0,
      M1,
      M2,
      M3
    }
    [Association("ProjectTemplate-ProofOfConcepts")]
    [DataSourceCriteria("Status = 'Approved'")]
    public XPCollection<ProofOfConcept> POCs
    {
      get
      {
        return GetCollection<ProofOfConcept>("POCs");
      }
    }
    [Association("Project-Tasks")]
    public XPCollection<TrackingTask> TrackignTasks
    {
      get
      {
        return GetCollection<TrackingTask>("TrackignTasks");
      }
    }
    //ATA Add project status  and owner 20/6/2016 [end]
    //ATA[end]
    //ATA add new relation between project and tracking entry direct instead of project entity  11/19/2017 [start]
    [Association("ProjectTemplate-TrackingEntries")]
    public XPCollection<TrackingEntry> TrackingEntries
    {
      get
      {
        return GetCollection<TrackingEntry>("TrackingEntries");
      }
    }
    //ATA add new relation between project and tracking entry direct instead of project entity  11/19/2017 [End]
    //ATA 14/7/2016 start notification customization
    private bool _emailsent;
    [Browsable(false)]
    public bool EmailSent
    {
      get
      {
        return _emailsent;
      }
      set
      {
        _emailsent = value;
      }
    }
    private bool _secondmail;
    [Browsable(false)]
    public bool SecondEmailSent
    {
      get
      {
        return _secondmail;
      }
      set
      {
        _secondmail = value;
      }
    }
    private DateTime? alarmTime;
    [Browsable(false)]
    public DateTime? AlarmTime
    {
      get
      {
        return alarmTime;
      }
      set
      {
        alarmTime = value;
        if (value == null)
        {
          RemindIn = null;
          IsPostponed = false;
        }
      }
    }
    [Browsable(false)]
    public bool IsPostponed { get; set; }
    private string _notificationmessage;
    [Browsable(false)]
    [DevExpress.Xpo.SizeAttribute(1000)]
    public string NotificationMessage
    {
      get
      {
        return _notificationmessage;
      }
      set
      {
        SetPropertyValue("NotificationMessage", ref _notificationmessage, value);
      }
    }
    [Browsable(false)]
    public TimeSpan? RemindIn { get; set; }
    [Browsable(false)]
    public object UniqueId
    {
      get
      {
        return Oid;
      }
    }
    //ATA 14/7/2016 end 
    //ATA 1/18/2017  add new field to control projects namin in system admin [start]
    private int? _iterationnumber;
    [RuleRequiredField]
    [RuleValueComparison(ValueComparisonType.GreaterThan, 0)]
    public int? IterationNumber
    {
      get
      {
        return _iterationnumber;
      }
      set
      {
        SetPropertyValue("IterationNumber", ref _iterationnumber, value);
      }
    }
    private Account _account;
    [RuleRequiredField]
    public Account Account
    {
      get
      {
        return _account;
      }
      set
      {
        SetPropertyValue("Account", ref _account, value);
      }
    }
    [Association("QANonComplains-ProjectTemplate")]
    public XPCollection<QANonComplains> NonComplians
    {
      get
      {
        return GetCollection<QANonComplains>("NonComplians");
      }
    }
    //ATA 1/18/2017  add new field to control projects namin in system admin [End]
    public enum ProjectType
    {
      Key,
      Custom,
            Standard, // maintanance
      CustomNonSoftware,
      
        }
    public enum status
    {
      New,
      InWork,
      Paused,
      Cancelled,
      Complete,
    }
    public object Clone()
    {

            if (this.Type != ProjectType.Key && this.IterationNumber == null)
            {
                this.IterationNumber = 0;
            }
            return this;
    }
  }
}
