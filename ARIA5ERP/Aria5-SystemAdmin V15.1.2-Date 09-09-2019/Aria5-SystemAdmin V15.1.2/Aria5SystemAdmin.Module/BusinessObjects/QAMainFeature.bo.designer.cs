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
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.ConditionalAppearance;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [Appearance("EndDate", AppearanceItemType = "ViewItem", TargetItems = "Enddate", Criteria = "!IsCurrentUserInRole('IterationAdmin')", Context = "DetailView", Enabled = false)]
  [RelatedEntity("Aria5-SystemAdmin-QAMainFeature")]
  public partial class QAMainFeature : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry _trackingEntry;
    private Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity _projectEntity;
    private System.String _description;
    private System.String _name;
    /*ATA ,1,bgn,15/3/2016, Baselineflag , add 2 new attribute to calcuate end date,
     and disable or enable edit or delet for this enddate property according to loginuser  
     */private System.DateTime _enddate;
    [NonPersistent]
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    public string LoginUser { get; set; }
    //ATA , end 
    public QAMainFeature(DevExpress.Xpo.Session session)
      : base(session)
    {
      //ATA get login user ,14/3/2016
      //LoginUser = SecuritySystem.CurrentUserName.Trim().ToUpper();
      //ATA , end 
    }
    [RuleUniqueValue]
    [Indexed(Unique = true)]
    [Size(200)]
    //ATA add rule required field attribute to be mandatory 1/5/2017[start]
    [RuleRequiredField]
    //ATA add rule required field attribute to be mandatory 1/5/2017[End]
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
    [DevExpress.Xpo.AssociationAttribute("MainFeatures-ProjectEntity")]
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity ProjectEntity
    {
      get
      {
        //ATA, 1,bgn,15/3/2016,check if _enddate not calculated and it is assign to a project then calculate it according to project type 
        if (_projectEntity != null && _enddate == DateTime.MinValue && _projectEntity.Enddate != DateTime.MinValue)
        {
          this._enddate = _projectEntity.Enddate;
          //if (_projectEntity.ProjectTemplate.Type.ToString() == "Key")
          //{
          //  this._enddate = CalcEndate.CalcEnddate(_projectEntity.ProjectTemplate.StartDate, 2);
          //  //this._enddate = CalcEnddate(_projectEntity.ProjectTemplate.StartDate, 2);
          //}
          //else
          //{
          //  this._enddate = _projectEntity.ProjectTemplate.EndDate;
          //}
        }
        //ATA, end 
        return _projectEntity;
      }
      set
      {
        SetPropertyValue("ProjectEntity", ref _projectEntity, value);
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
    [DevExpress.Xpo.AssociationAttribute("EntityFeatures-TrackingEntry")]
    public Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry TrackingEntry
    {
      get
      {
        ////ATA, 1,bgn,14/3/2016,check if _enddate not calculated and it is assign to a project then calculate it according to project type 
        //if (_trackingEntry != null && _enddate == DateTime.MinValue && _trackingEntry.ProjectTemplate!=null)
        //{
        //    if (_trackingEntry.ProjectTemplate.Type.ToString() == "Key")
        //    {
        //        this._enddate = CalcEndate.CalcEnddate(_trackingEntry.ProjectTemplate.StartDate, 2);
        //        //this._enddate = CalcEnddate(_project.StartDate, 1);
        //    }
        //    else
        //    {
        //        this._enddate = _trackingEntry.ProjectTemplate.EndDate;
        //    }
        //}
        ////ATA , end
        //ATA ,2, 7/4/2016, [start,queue]
        if (_trackingEntry != null && _trackingEntry.ProjectEntity != null)
        {
          ProjectEntity = _trackingEntry.ProjectEntity;
        }
        //ATA[end]
        return _trackingEntry;
      }
      set
      {
        SetPropertyValue("TrackingEntry", ref _trackingEntry, value);
      }
    }
    //ATA ,1,15/3/2016 add new property end date to calculate the edit end date for this usecase [start]
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
    //ATA ,1,15/3/2016 add new property end date to calculate the edit end date for this usecase [End]
      //ATA add relation with testing cases 3/23/2017 [start]
      [Association("EntityFeature-testcases")]
    public XPCollection<TestCase> TestCases
    {
        get
        {
            return GetCollection<TestCase>("TestCases");
        }
    }
      //ATA add relation with testing cases 3/23/2017 [start]

  }
}