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
  //[Appearance("QADefect", AppearanceItemType = "ViewItem", TargetItems = "QADefect", Criteria = "QADefect!=null", Context = "DetailView", Enabled = false)]
  //[Appearance("QAUseCase", AppearanceItemType = "ViewItem", TargetItems = "QAUseCase", Criteria = "QAUseCase!=null", Context = "DetailView", Enabled = false)]
  //[Appearance("QAUseCaseFlow", AppearanceItemType = "ViewItem", TargetItems = "QAUseCaseFlow", Criteria = "QAUseCaseFlow!=null", Context = "DetailView", Enabled = false)]
  //[Appearance("Requirement", AppearanceItemType = "ViewItem", TargetItems = "Requirement", Criteria = "Requirement!=null", Context = "DetailView", Enabled = false)]
  //[Appearance("TestCase", AppearanceItemType = "ViewItem", TargetItems = "TestCase", Criteria = "TestCase!=null", Context = "DetailView", Enabled = false)]
  //[Appearance("TestCaseSteps", AppearanceItemType = "ViewItem", TargetItems = "TestCaseSteps", Criteria = "TestCaseSteps!=null", Context = "DetailView", Enabled = false)]
  //[Appearance("TestRun", AppearanceItemType = "ViewItem", TargetItems = "TestRun", Criteria = "TestRun!=null", Context = "DetailView", Enabled = false)]
  [DefaultClassOptions]
  [XafDefaultProperty("CommentDetails")]
  [Appearance("ProjectTemplate", AppearanceItemType = "ViewItem", TargetItems = "ProjectTemplate", Criteria = "ProjectTemplate!=null", Context = "DetailView", Enabled = false)]
  public partial class Comments : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.TeamLeaderDailyFollowUp _teamLeaderDailyFollowUp;
    private Aria5SystemAdmin.Module.BusinessObjects.QAUseCaseSteps _qAUseCaseSteps;
    private Aria5SystemAdmin.Module.BusinessObjects.QAUseCase _qAUseCase;
    private Aria5SystemAdmin.Module.BusinessObjects.TestCaseSteps _testCaseSteps;
    private Aria5SystemAdmin.Module.BusinessObjects.TestCase _testCase;
    private Aria5SystemAdmin.Module.BusinessObjects.RequirementType _requirementType;
    private Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate _projectTemplate;
    private Aria5SystemAdmin.Module.BusinessObjects.QAUseCaseFlow _qAUseCaseFlow;
    private Aria5SystemAdmin.Module.BusinessObjects.QADefect _qADefect;
    private Aria5SystemAdmin.Module.BusinessObjects.TestRun _testRun;
    private Aria5SystemAdmin.Module.BusinessObjects.Requirement _requirement;
    private System.String _commentDetails;
    public Comments(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(4000)]
      //ATA add rule required field 2/2/2017
        [RuleRequiredField]
    public System.String CommentDetails
    {
      get
      {
        return _commentDetails;
      }
      set
      {
        SetPropertyValue("CommentDetails", ref _commentDetails, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Commentses-Requirement")]
    public Aria5SystemAdmin.Module.BusinessObjects.Requirement Requirement
    {
      get
      {
        return _requirement;
      }
      set
      {
        SetPropertyValue("Requirement", ref _requirement, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Comments-TestRun")]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
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
    [DevExpress.Xpo.AssociationAttribute("Comments-QADefect")]
    public Aria5SystemAdmin.Module.BusinessObjects.QADefect QADefect
    {
      get
      {
        return _qADefect;
      }
      set
      {
        SetPropertyValue("QADefect", ref _qADefect, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Comments-QAUseCaseFlow")]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public Aria5SystemAdmin.Module.BusinessObjects.QAUseCaseFlow QAUseCaseFlow
    {
      get
      {
        return _qAUseCaseFlow;
      }
      set
      {
        SetPropertyValue("QAUseCaseFlow", ref _qAUseCaseFlow, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Commentses-ProjectTemplate")]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
      //ATA Add required field 2/2/2017
        [RuleRequiredField]
    public Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate ProjectTemplate
    {
      get
      {
        return _projectTemplate;
      }
      set
      {
        SetPropertyValue("ProjectTemplate", ref _projectTemplate, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Comments-RequirementType")]
    public Aria5SystemAdmin.Module.BusinessObjects.RequirementType RequirementType
    {
      get
      {
        return _requirementType;
      }
      set
      {
        SetPropertyValue("RequirementType", ref _requirementType, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Comments-TestCase")]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public Aria5SystemAdmin.Module.BusinessObjects.TestCase TestCase
    {
      get
      {
        return _testCase;
      }
      set
      {
        SetPropertyValue("TestCase", ref _testCase, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Comments-TestCaseSteps")]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public Aria5SystemAdmin.Module.BusinessObjects.TestCaseSteps TestCaseSteps
    {
      get
      {
        return _testCaseSteps;
      }
      set
      {
        SetPropertyValue("TestCaseSteps", ref _testCaseSteps, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Comments-QAUseCase")]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public Aria5SystemAdmin.Module.BusinessObjects.QAUseCase QAUseCase
    {
      get
      {
        return _qAUseCase;
      }
      set
      {
        SetPropertyValue("QAUseCase", ref _qAUseCase, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Comments-QAUseCaseSteps")]
    public Aria5SystemAdmin.Module.BusinessObjects.QAUseCaseSteps QAUseCaseSteps
    {
      get
      {
        return _qAUseCaseSteps;
      }
      set
      {
        SetPropertyValue("QAUseCaseSteps", ref _qAUseCaseSteps, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Comments-TeamLeaderDailyFollowUp")]
    public Aria5SystemAdmin.Module.BusinessObjects.TeamLeaderDailyFollowUp TeamLeaderDailyFollowUp
    {
      get
      {
        return _teamLeaderDailyFollowUp;
      }
      set
      {
        SetPropertyValue("TeamLeaderDailyFollowUp", ref _teamLeaderDailyFollowUp, value);
      }
    }
  }
}