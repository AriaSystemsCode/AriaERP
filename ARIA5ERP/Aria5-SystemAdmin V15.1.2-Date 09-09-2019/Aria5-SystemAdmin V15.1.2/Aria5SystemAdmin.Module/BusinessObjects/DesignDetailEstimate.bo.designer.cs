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
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.Persistent.Validation;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [Appearance("EndDate", AppearanceItemType = "ViewItem", TargetItems = "Enddate", Criteria = "!IsCurrentUserInRole('IterationAdmin')", Context = "DetailView", Enabled = false)]
  [DefaultClassOptions]
    //ATA to fix error that the object is modified by another user 
    [OptimisticLocking(OptimisticLockingBehavior.NoLocking)]
  public partial class DesignDetailEstimate : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity _qAProjectEntity;
    //ATA
    private Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry _trackingentry;
    //ATA
    private System.Int64 _estimate;
    private System.String _activity;
    /*ATA ,1,bgn,15/3/2016, Baselineflag , add 2 new attribute to calcuate end date,
  and disable or enable edit or delet for this enddate property according to loginuser  
  */private System.DateTime _enddate;
    //[NonPersistent]
    //[DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    //[DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    //[DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    //public string LoginUser { get; set; }
    ////ATA, 1 , end 
    public DesignDetailEstimate(DevExpress.Xpo.Session session)
      : base(session)
    {
      //ATA,1, get login user ,14/3/2016
     // LoginUser = SecuritySystem.CurrentUserName.Trim().ToUpper();
      //ATA ,1, end 
    }
    [Size(4000)]
    public System.String Activity
    {
      get
      {
        return _activity;
      }
      set
      {
        SetPropertyValue("Activity", ref _activity, value);
      }
    }
    //ATA bgn 
    public System.DateTime Enddate
    {
      get
      {
        return _enddate;
      }
      set
      {
        //if (QAProjectEntity != null && QAProjectEntity.ProjectTemplate != null)
        //{
        //    SetPropertyValue("Enddate", ref _enddate, CalcEnddate(_qAProjectEntity.ProjectTemplate.StartDate, 3));
        //}
        //else
        //{
        SetPropertyValue("Enddate", ref _enddate, value);
        //}
      }
    }
    //ATA end 
    //ATA add rule required field attribute to be mandatory 1/5/2017[start]
    [RuleRequiredField]
    //ATA add rule required field attribute to be mandatory 1/5/2017[End]
    public System.Int64 Estimate
    {
      get
      {
        return _estimate;
      }
      set
      {
        SetPropertyValue("Estimate", ref _estimate, value);
      }
    }
    //ATA,2[start ] move detail design tabs under tracking entry instead of project entity 
    [DevExpress.Xpo.AssociationAttribute("DetailDesignEstimates-TrackingEntry")]
    public Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry TrackingEntry
    {
      get
      {
        //ATA,2 7/4/2016 [start,Queue ]
        if (_trackingentry != null && _trackingentry.ProjectEntity != null)
        {
          _qAProjectEntity = _trackingentry.ProjectEntity;
        }
        //ATA,2,[end]
        return _trackingentry;
      }
      set
      {
        SetPropertyValue("TrackingEntry", ref _trackingentry, value);
      }
    }
    //ATA,2,[END]
    [DevExpress.Xpo.AssociationAttribute("DetailDesignEstimates-QAProjectEntity")]
    public Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity QAProjectEntity
    {
      get
      {
        //ATA, 1,bgn,15/3/2016,check if _enddate not calculated and it is assign to a project then calculate it according to project type 
        if (_qAProjectEntity != null && _enddate == DateTime.MinValue)
        {
          if (_qAProjectEntity.ProjectTemplate.Type == ProjectTemplate.ProjectType.Key)
          {
            _enddate = CalcEndate.CalcEnddate(_qAProjectEntity.ProjectTemplate.StartDate, 3);
            //_enddate = CalcEnddate(_qAProjectEntity.ProjectTemplate.StartDate, 3);
          }
          else
          {
            _enddate = _qAProjectEntity.ProjectTemplate.EndDate;
          }
        }
        //ATA , end 
        return _qAProjectEntity;
      }
      set
      {
        SetPropertyValue("QAProjectEntity", ref _qAProjectEntity, value);
      }
    }
  }
}