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
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.Persistent.Validation;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-QATestPlan")]
  [XafDefaultProperty("Description")]
    //ATA add test plan to baseline check and NC's 3/23/2017 [Start]
  [Appearance("EndDate", AppearanceItemType = "ViewItem", TargetItems = "Enddate", Criteria = "!IsCurrentUserInRole('IterationAdmin')", Context = "DetailView", Enabled = false)]
    //ATA add test plan to baseline check and NC's 3/23/2017 [End]
    [Appearance("ProjectTemplate", AppearanceItemType = "ViewItem", TargetItems = "ProjectTemplate", Criteria = "ProjectTemplate!=null", Context = "DetailView", Enabled = false)]
  public partial class QATestPlan : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.String _description;
    private Aria5SystemAdmin.Module.BusinessObjects.TestRun _testRun;
    public QATestPlan(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate _projectTemplate;
      //ATA no need 
   // private Aria5SystemAdmin.Module.BusinessObjects.Resources _Testers;
    private System.String _TestDeliverables;
    private System.String _PassFailCriteria;
    private System.String _TrainingNeeds;
    private Testlevels _Testlevels;
      //ATA add new relation with tracking entry 
    private TrackingEntry _trackingEntry;
  
      //ATA add new end date to the test plan 
    private System.DateTime _enddate;
    public System.DateTime Enddate
    {
        get
        {
            return _enddate;
        }
        set
        {
            SetPropertyValue("Enddate", ref _enddate, value);
        }
    }
    [DevExpress.Xpo.SizeAttribute(4000)]
    [RuleRequiredField]
    public System.String TrainingNeeds
    {
      get
      {
        return _TrainingNeeds;
      }
      set
      {
        SetPropertyValue("TrainingNeeds", ref _TrainingNeeds, value);
      }
    }
      //ATA add new relation with tracking entry 
    [Association("TrackingEntry-TestPlans")]
    public TrackingEntry TrackingEntry
    {
        get { return _trackingEntry; }
        set
        {
            SetPropertyValue("TrackingEntry", ref _trackingEntry, value);
            if (!IsSaving && !IsLoading && value != null)
            {
                this.ProjectTemplate = value.ProjectTemplate;
            }
        }
    }
    //  [RuleRequiredField]
    //public Aria5SystemAdmin.Module.BusinessObjects.Resources Testers
    //{
    //  get
    //  {
    //    return _Testers;
    //  }
    //  set
    //  {
    //    SetPropertyValue("Testers", ref _Testers, value);
    //  }
    //}
    [DevExpress.Xpo.SizeAttribute(4000)]
      [RuleRequiredField]
    public System.String PassFailCriteria
    {
      get
      {
        return _PassFailCriteria;
      }
      set
      {
        SetPropertyValue("PassFailCriteria", ref _PassFailCriteria, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(4000)]
    [RuleRequiredField]
    public System.String TestDeliverables
    {
      get
      {
        return _TestDeliverables;
      }
      set
      {
        SetPropertyValue("TestDeliverables", ref _TestDeliverables, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("TestPlans-ProjectTemplate")]
    public Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate ProjectTemplate
    {
      get
      {
          //ATA add the end date value based on the project type 
          if (_projectTemplate != null)
          {
              if (this._enddate == DateTime.MinValue)
              {
                  if (_projectTemplate.Type.ToString() == "Key")
                  {
                      this._enddate = CalcEndate.CalcEnddate(_projectTemplate.StartDate, 1);
                  }
                  else
                  {
                      this.Enddate = _projectTemplate.EndDate;
                  }
              }
          }
        return _projectTemplate;

      }
      set
      {
        SetPropertyValue("ProjectTemplate", ref _projectTemplate, value);
      }
    }
    //ATA , modifiy test plane test level to be check boxex instead enum [start]
    private System.Boolean _componenttest;
    public System.Boolean ComponentTest
    {
      get
      {
        return _componenttest;
      }
      set
      {
        SetPropertyValue("ComponentTest", ref _componenttest, value);
      }
    }
    private System.Boolean _acceptanceTest;
    public System.Boolean AcceptanceTest
    {
      get
      {
        return _acceptanceTest;
      }
      set
      {
        SetPropertyValue("AcceptanceTest", ref _acceptanceTest, value);
      }
    }
    private System.Boolean _systemTest;
    public System.Boolean SystemTest
    {
      get
      {
        return _systemTest;
      }
      set
      {
        SetPropertyValue("SystemTest", ref _systemTest, value);
      }
    }
    private System.Boolean _integrationTest;
    public System.Boolean IntegrationTest
    {
      get
      {
        return _integrationTest;
      }
      set
      {
        SetPropertyValue("IntegrationTest", ref _integrationTest, value);
      }
    }
    //ATA , modifiy test plane test level to be check boxex instead enum [end]
    public enum Testlevels
    {
      ComponentTest,
      IntegrationTest,
      SystemTest,
      AcceptanceTest,
    }
    //ATA , modifiy test plane test type to be check boxex instead enum [start]
    private System.Boolean _functionTesting;
    public System.Boolean FunctionTesting
    {
      get
      {
        return _functionTesting;
      }
      set
      {
        SetPropertyValue("FunctionTesting", ref _functionTesting, value);
      }
    }
    private System.Boolean _usabilityTesting;
    public System.Boolean UsabilityTesting
    {
      get
      {
        return _usabilityTesting;
      }
      set
      {
        SetPropertyValue("UsabilityTesting", ref _usabilityTesting, value);
      }
    }
    private System.Boolean _performanceTesting;
    public System.Boolean PerformanceTesting
    {
      get
      {
        return _performanceTesting;
      }
      set
      {
        SetPropertyValue("PerformanceTesting", ref _performanceTesting, value);
      }
    }
    private System.Boolean _portabilityTesting;
    public System.Boolean PortabilityTesting
    {
      get
      {
        return _portabilityTesting;
      }
      set
      {
        SetPropertyValue("PortabilityTesting", ref _portabilityTesting, value);
      }
    }
    public Testlevels Testlevel
    {
      get
      {
        return _Testlevels;
      }
      set
      {
        SetPropertyValue("Testlevel", ref _Testlevels, value);
      }
    }

    //ATA , modifiy test plane test type to be check boxex instead enum [end]
    // Ras 8-11-2015 add new Property [begin]
    public enum Testtype
    {
      FunctionTesting,
      UsabilityTesting,
      PerformanceTesting,
      PortabilityTesting
    }
    private Testtype _TestType;
    public Testtype TestType
    {
      get
      {
        return _TestType;
      }
      set
      {
        SetPropertyValue("TestType", ref _TestType, value);
      }
    }
    // Ras 8-11-2015 add new Property [end]
    [DevExpress.Xpo.AssociationAttribute("Environments-TestPlan")]
    [DataSourceCriteria("ProjectTemplate.Environments")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Environment> Environments
    {
      get
      {
        //if (_projectTemplate != null)
        //{
        //   Environments.AddRange(_projectTemplate.Environments); 
        //}
        // return Environments;
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Environment>("Environments");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ProjectEntities-TestPlan")]
    [DataSourceCriteria("ProjectTemplate.ProjectEntities")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity> ProjectEntities
    {
      get
      {
        //if (_projectTemplate != null)
        //{
        //    ProjectEntities.AddRange(_projectTemplate.ProjectEntities);
        //}
        // return ProjectEntities;
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity>("ProjectEntities");
      }
    }
    public Aria5SystemAdmin.Module.BusinessObjects.TestRun TestRun
    {
      get
      {
        return _testRun;
      }
      set
      {
        SetPropertyValue("TestRun", ref _testRun, value);
      }
    }
    //ATA 19/5/2016 enhance relation between test plan and test run to be one to many [start ]
    [DevExpress.Xpo.AssociationAttribute("QATestPlan-TestRun")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TestRun> TestRuns
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TestRun>("TestRuns");
      }
    }
    //ATA 19/5/2016 enhance relation between test plan and test run to be one to many [End]
    //ATA change description caption to be Title instead of description 
    //ATA add rule required field attribute to be mandatory 1/5/2017[start]
    [RuleRequiredField]
    //ATA add rule required field attribute to be mandatory 1/5/2017[End]
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
  }
}
