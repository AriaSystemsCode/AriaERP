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
using DevExpress.ExpressApp.Model;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-ApplicationBuild_T")]
  public partial class ApplicationBuild_T : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.String _tFSLabel;
    private Aria5SystemAdmin.Module.BusinessObjects.Application_T _application_T;
    private System.String _technicalConsiderations;
    private System.String _releaseNotes;
    private System.String _completeBy;
    private System.DateTime _completeDate;
    private System.String _approvedBy;
    private System.DateTime _approveDate;
    private System.DateTime _issueDate;
    private ApplicationBuildStatus _status;
    private System.String _applicationName;
    private System.String _applicationId;
    private System.String _servicePackNo;
    private System.String _releaseNo;
    private System.String _description;
    private System.String _applicationBuildId;
    private System.Boolean _isNew;
    public ApplicationBuild_T(DevExpress.Xpo.Session session)
      : base(session)
    {
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
    //   [RuleRequiredField]
        [DevExpress.Xpo.SizeAttribute(30)]
        [ModelDefault("AllowEdit", "False")]
        public System.String ApplicationBuildId
    {
      get
      {
        //ATA return build number from the application 
        //if (Application_T != null)
        //  return _applicationBuildId = Application_T.BuildNo;
        //else
          return _applicationBuildId;
      }
      set
      {
        SetPropertyValue("ApplicationBuildId", ref _applicationBuildId, value);
      }
    }
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
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String ReleaseNo
    {
      get
      {
        if (Application_T != null)
          return _releaseNo = Application_T.ReleaseNo;
        else
          return _releaseNo;
      }
      set
      {
        SetPropertyValue("ReleaseNo", ref _releaseNo, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String ServicePackNo
    {
      get
      {
        if (Application_T != null)
          return _servicePackNo = Application_T.ServicePack;
        else
          return _servicePackNo;
      }
      set
      {
        SetPropertyValue("ServicePackNo", ref _servicePackNo, value);
      }
    }
    //[RuleRequiredField]
    public System.String ApplicationId
    {
      get
      {
        if (Application_T != null)
          return _applicationId = Application_T.Id;
        else
          return _applicationId;
      }
      set
      {
        SetPropertyValue("ApplicationId", ref _applicationId, value);
      }
    }
    //[RuleRequiredField]
    public System.String ApplicationName
    {
      get
      {
        if (Application_T != null)
          return _applicationName = Application_T.Name;
        else
          return _applicationName;
      }
      set
      {
        SetPropertyValue("ApplicationName", ref _applicationName, value);
      }
    }
    [RuleRequiredField]
    [DevExpress.Xpo.SizeAttribute(50)]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public ApplicationBuildStatus Status
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
    [RuleRequiredField]
    public System.DateTime IssueDate
    {
      get
      {
        return _issueDate;
      }
      set
      {
        SetPropertyValue("IssueDate", ref _issueDate, value);
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
    [DevExpress.Xpo.SizeAttribute(30)]
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
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String CompleteBy
    {
      get
      {
        return _completeBy;
      }
      set
      {
        SetPropertyValue("CompleteBy", ref _completeBy, value);
      }
    }
    [DbType("nvarchar(max)")]
    [DevExpress.Xpo.SizeAttribute(5000)]
    public System.String ReleaseNotes
    {
      get
      {
        return _releaseNotes;
      }
      set
      {
        SetPropertyValue("ReleaseNotes", ref _releaseNotes, value);
      }
    }
    [DbType("nvarchar(max)")]
    [DevExpress.Xpo.SizeAttribute(5000)]
    public System.String TechnicalConsiderations
    {
      get
      {
        return _technicalConsiderations;
      }
      set
      {
        SetPropertyValue("TechnicalConsiderations", ref _technicalConsiderations, value);
      }
    }

        private string _BuildURL;
        [EditorAlias("HyperLinkStringPropertyEditor")]
        public string BuildURL
        {
            get
            {
                return _BuildURL;
            }
            set
            {
                SetPropertyValue("BuildURL", ref _BuildURL, value);
            }
        }
        [DevExpress.Xpo.AssociationAttribute("ApplicationBuild_Ts-Application_T")]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public Aria5SystemAdmin.Module.BusinessObjects.Application_T Application_T
    {
      get
      {
        return _application_T;
      }
      set
      {
        SetPropertyValue("Application_T", ref _application_T, value);
      }
    }
        #region Commented Code
        //[DevExpress.Xpo.AssociationAttribute("ApplicationTrackingEntries-ApplicationBuild_T")]
        //public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ApplicationBuildEntries_T> ApplicationTrackingEntries
        //{
        //  get
        //  {
        //    return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ApplicationBuildEntries_T>("ApplicationTrackingEntries");
        //  }
        //}
        #endregion

            
        [DevExpress.Xpo.AssociationAttribute("ApplicationBuild_T-TrackingEntrys")]
       // [DataSourceProperty("Application.ApplicationTrackingEntries")]
        [DataSourceCriteria("[Status] ='ReleaseCandidate' AND [Application]='@This.Application_T'")]
        //And[ApplicationBuild] = null
        public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry> ApplicationTrackingEntries
        {
            get
            {
               
                return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry>("ApplicationTrackingEntries");
            }
        }
        public System.String TFSLabel
    {
      get
      {
        return _tFSLabel;
      }
      set
      {
        SetPropertyValue("TFSLabel", ref _tFSLabel, value);
      }
    }
        //MMT
        [DevExpress.Xpo.AssociationAttribute("ApplicationBuild_T-TestRuns")]
        // [DataSourceProperty("Application.ApplicationTrackingEntries")]
        //[DataSourceCriteria("[Status] ='ReleaseCandidate' AND [Application]='@This.Application_T'")]
        //And[ApplicationBuild] = null
        public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TestRun> ApplicationBuildTestRuns
        {
            get
            {

                return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TestRun>("ApplicationBuildTestRuns");
            }
        }
   
        [DevExpress.Xpo.AssociationAttribute("ApplicationBuild_T-Defects")]
        public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QADefect> ApplicationBuildDefects
        {
            get
            {

                return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QADefect>("ApplicationBuildDefects");
            }
        }
        //MMT
    }
    public enum ApplicationBuildStatus
  {
    Open,
    Staging,
    ReleaseCandidate,
    Complete,
    Approved
  };
}