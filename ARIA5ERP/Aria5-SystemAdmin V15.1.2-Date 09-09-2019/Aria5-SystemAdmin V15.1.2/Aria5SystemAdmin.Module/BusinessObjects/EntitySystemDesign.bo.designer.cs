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
using DevExpress.ExpressApp;
using DevExpress.Persistent.Validation;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [Appearance("EndDate", AppearanceItemType = "ViewItem", TargetItems = "Enddate", Criteria = "!IsCurrentUserInRole('IterationAdmin')", Context = "DetailView", Enabled = false)]
  public partial class EntitySystemDesign : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity _qAProjectEntity;
    //ATA
    private Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry _trackingentry;
   //ATA ,1, enhanc entity system design with estimate and activity [Start]
    private System.Int64 _estimate;
    private System.String _activity;
      //ATA,1,enhance system design with estimate and activity [End]
    //ATA
    /*ATA ,1,bgn,15/3/2016, Baselineflag , add 2 new attribute to calcuate end date,
and disable or enable edit or delet for this enddate property according to loginuser  
*/private System.DateTime _enddate;
    //[NonPersistent]
    //[DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    //[DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    //[DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    //public string LoginUser { get; set; }
    //ATA, 1 , end 
    public EntitySystemDesign(DevExpress.Xpo.Session session)
      : base(session)
    {
      //ATA,1, get login user ,14/3/2016
     // LoginUser = SecuritySystem.CurrentUserName.Trim().ToUpper();
      //ATA ,1, end 
    }
    private string _MemberFullName;
    [Size(4000)]
      [RuleRequiredField]
    public string MemberFullName
    {
      get
      {
        return _MemberFullName;
      }
      set
      {
        SetPropertyValue("MemberFullName", ref _MemberFullName, value);
      }
    }
    private string _MemberType;
    [Size(4000)]
      [RuleRequiredField]
    public string MemberType
    {
      get
      {
        return _MemberType;
      }
      set
      {
        SetPropertyValue("MemberType", ref _MemberType, value);
      }
    }
    public enum ModType
    {
      Add,
      Modify,
      Remove
    }
    private ModType _ModificationType;
    public ModType ModificationType
    {
      get
      {
        return _ModificationType;
      }
      set
      {
        SetPropertyValue("ModificationType", ref _ModificationType, value);
      }
    }
    private string _Project;
    [Size(SizeAttribute.Unlimited)]
      [RuleRequiredField]
    public string Project
    {
      get
      {
        return _Project;
      }
      set
      {
        SetPropertyValue("Project", ref _Project, value);
      }
    }
    private string _Design;
    [Size(4000)]
      [RuleRequiredField]
    public string Design
    {
      get
      {
        return _Design;
      }
      set
      {
        SetPropertyValue("Design", ref _Design, value);
      }
    }
    //ATA,2[start ] move detail design tabs under tracking entry instead of project entity 
    [DevExpress.Xpo.AssociationAttribute("EntitySystemDesign-TrackingEntry")]
    public Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry TrackingEntry
    {
      get
      {
        //ATA,2,7/4/2016 [start,queue]
        if (_trackingentry != null && _trackingentry.ProjectEntity != null)
        {
          _qAProjectEntity = _trackingentry.ProjectEntity;
        }
        //ATA,2,7/4/2016 [END]
        return _trackingentry;
      }
      set
      {
        SetPropertyValue("TrackingEntry", ref _trackingentry, value);
      }
    }
    //ATA,2,[END]
    [DevExpress.Xpo.AssociationAttribute("EntitySystemDesign-QAProjectEntity")]
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
      //ATA,1,enhance entity system design with estimate and activity [Start]
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
      //ATA,1,enhance entity system design with estimate and activity [End]
      //ATA add status to the detail design to know if it approved or not 1/9/2017 [Start]
    public enum StatusItems
    {
        New,
        Approved
    }
    private StatusItems _status;
      [RuleRequiredField]
    public StatusItems Status
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
      //ATA add status to the detail design to know if it approved or not 1/9/2017 [ENd]

  }
}
