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
  [RelatedEntity("Aria5-SystemAdmin-Application_T")]
  public partial class Application_T : Aria5SystemAdmin.Module.BusinessObjects.Entity
  {
    private System.String _servicePack;
    private System.String _buildNo;
    private System.String _releaseNo;
    private System.String _name;
    private Aria5SystemAdmin.Module.BusinessObjects.ApplicationCategory _applicationCategory;
    public Application_T(DevExpress.Xpo.Session session)
      : base(session)
    {
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
    [DevExpress.Xpo.SizeAttribute(50)]
    public System.String ReleaseNo
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
    public System.String BuildNo
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
    [DevExpress.Xpo.SizeAttribute(50)]
    public System.String ServicePack
    {
      get
      {
        return _servicePack;
      }
      set
      {
        SetPropertyValue("ServicePack", ref _servicePack, value);
      }
    }
    public Aria5SystemAdmin.Module.BusinessObjects.ApplicationCategory ApplicationCategory
    {
      get
      {
        return _applicationCategory;
      }
      set
      {
        SetPropertyValue("ApplicationCategory", ref _applicationCategory, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("AriaObjects-Applications")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObject> AriaObjects
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObject>("AriaObjects");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ApplicationBuild_Ts-Application_T")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ApplicationBuild_T> ApplicationBuild_Ts
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ApplicationBuild_T>("ApplicationBuild_Ts");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ApplicationSettings-Application_T")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ApplicationSetting> ApplicationSettings
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ApplicationSetting>("ApplicationSettings");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("PreRequiste_Applications-Application_Ts")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Application_T> Application_Ts
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Application_T>("Application_Ts");
      }
    }
    //Sara.N  , 26/04/2015 [Tracking# + Aria5-DevExpress-Build]_Programming.[Start]
    [DevExpress.Xpo.AssociationAttribute("PreRequiste_Applications-Application_Ts")]
    [DataSourceCriteria("Oid!='@Oid'")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Application_T> PreRequiste_Applications
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Application_T>("PreRequiste_Applications");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ProjectTemplates-Application_T")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate> ProjectTemplates
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate>("ProjectTemplates");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Requirements-Application_T")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Requirement> Requirements
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Requirement>("Requirements");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("TestRuns-Application")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TestRun> TestRuns
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TestRun>("TestRuns");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("TestCases-Application")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TestCase> TestCases
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TestCase>("TestCases");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("QAUseCases-Application")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUseCase> QAUseCases
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUseCase>("QAUseCases");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("QAProjectEntities-Application")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity> QAProjectEntities
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity>("QAProjectEntities");
      }
    }
    //ATA develop defect density 28/9/2016[start]
    [Association("Application_T-ApplicationKloc")]
    public XPCollection<ApplicationKloc> KlocVersions
    {
      get
      {
        return GetCollection<ApplicationKloc>("KlocVersions");
      }
    }
    //ATA [End]
  }
}
