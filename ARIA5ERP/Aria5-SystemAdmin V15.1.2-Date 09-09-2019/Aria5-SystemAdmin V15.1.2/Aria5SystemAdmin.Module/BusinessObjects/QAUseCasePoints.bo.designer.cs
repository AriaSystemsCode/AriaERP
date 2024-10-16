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
  [Appearance("EndDate", AppearanceItemType = "ViewItem", TargetItems = "Enddate", Criteria = "!IsCurrentUserInRole('IterationAdmin')", Context = "DetailView", Enabled = false)]
  [DefaultClassOptions]
  [XafDefaultProperty("Title")]
  [RelatedEntity("Aria5-SystemAdmin-QAUseCasePoints")]
  [Appearance("ProjectTemplate", AppearanceItemType = "ViewItem", TargetItems = "ProjectTemplate", Criteria = "ProjectTemplate!=null", Context = "DetailView", Enabled = false)]
  public partial class QAUseCasePoints : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate _project;
    /*ATA ,1,bgn,14/3/2016, Baselineflag , add 2 new attribute to calcuate end date,
      and disable or enable edit or delet for this enddate property according to loginuser  
      */private System.DateTime _enddate;
    //[NonPersistent]
    //[DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    //[DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    //[DevExpress.Persistent.Base.VisibleInLookupListViewAttribute(false)]
    //public string LoginUser { get; set; }
    //ATA ,end 
    public QAUseCasePoints(DevExpress.Xpo.Session session)
      : base(session)
    {
      // LoginUser = SecuritySystem.CurrentUserName.Trim().ToUpper();
    }
    // Sara.N, Customize Clonning of usecase Points [Start]
    public QAUseCasePoints(DevExpress.Xpo.Session session, QAUseCasePoints UseCasePoints)
      : base(session)
    {
      this.Title = UseCasePoints.Title.ToString() + "" + DateTime.Now;
      this.EffortBasedCosts = UseCasePoints.EffortBasedCosts;
      this.FixedCost = UseCasePoints.FixedCost;
      this.Notes = UseCasePoints.Notes;
      this.ProductivityFactor = UseCasePoints.ProductivityFactor;
      this.ProjectEfforts = UseCasePoints.ProjectEfforts;
      this.ProjectSize = UseCasePoints.ProjectSize;
      this.StandardCostRate = UseCasePoints.StandardCostRate;
      foreach (var item in UseCasePoints.ResourceShares.ToList())
      {
        this.ResourceShares.Add(new QAResourceShare(session, item));
      }
      foreach (var item in UseCasePoints.UseCaseEnvironmentalFactors.ToList())
      {
        this.UseCaseTechnicalFactors.Add(new QAUseCaseTechnicalFactors(session, item));
      }
      foreach (var item in UseCasePoints.WBS.ToList())
      {
        this.WBS.Add(new QAWBS(session, item));
      }
      foreach (var item in UseCasePoints.UseCaseEnvironmentalFactors.ToList())
      {
        this.UseCaseEnvironmentalFactors.Add(new QAUseCaseEnvironmentalFactors(session, item));
      }
      foreach (var item in UseCasePoints.UseCAsePointsCategories.ToList())
      {
        this.UseCAsePointsCategories.Add(new QAUseCAsePointsCategory(session, item));
      }
      foreach (var item in UseCasePoints.UsecaseActors.ToList())
      {
        this.UsecaseActors.Add(new QAUsecaseActors(session, item));
      }
      this.TotalProjectCosts = UseCasePoints.TotalProjectCosts;
      this.TotalUsecaseActorsPoints = UseCasePoints.TotalUsecaseActorsPoints;
      this.TotalUsecaseCategoryPoints = UseCasePoints.TotalUsecaseCategoryPoints;
      this.TotalUsecaseEnvironmentalFactorsPoints = UseCasePoints.TotalUsecaseEnvironmentalFactorsPoints;
      this.TotalUsecaseTechnicalFactorsPoints = UseCasePoints.TotalUsecaseTechnicalFactorsPoints;
      this.Totalweight = UseCasePoints.Totalweight;
    }
    // Sara.N, Customize Clonning of usecase Points [End]
    private string _title;
    //ATA add rule required field attribute to be mandatory 1/5/2017[start]
    //ATA add rule required field attribute to be mandatory 1/5/2017[End]
    [RuleRequiredField]
    public System.String Title
    {
      get
      {
        return _title;
      }
      set
      {
        SetPropertyValue("Title", ref _title, value);
      }
    }
    private short _UseCaseTotalweight;
    public short Totalweight
    {
      get
      {
        // _UseCaseTotalweight=TotalUsecaseActorsPoints+
          //ATA comment that because it is make and error while loading 
        //_UseCaseTotalweight = (short)((TotalUsecaseActorsPoints + TotalUsecaseCategoryPoints) * TotalUsecaseEnvironmentalFactorsPoints * TotalUsecaseTechnicalFactorsPoints);
        return _UseCaseTotalweight;
      }
      set
      {
        SetPropertyValue("Totalweight", ref _UseCaseTotalweight, value);
      }
    }
    private string _notes;
    public System.String Notes
    {
      get
      {
        return _notes;
      }
      set
      {
        SetPropertyValue("Notes", ref _notes, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("UseCAsePointsCategories-UseCasePoints")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUseCAsePointsCategory> UseCAsePointsCategories
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUseCAsePointsCategory>("UseCAsePointsCategories");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("UsecaseActors-UseCasePoints")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUsecaseActors> UsecaseActors
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUsecaseActors>("UsecaseActors");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("UseCaseTechnicalFactors-UseCasePoints")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUseCaseTechnicalFactors> UseCaseTechnicalFactors
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUseCaseTechnicalFactors>("UseCaseTechnicalFactors");
      }
    }
    private short _ProductivityFactor;
    public short ProductivityFactor
    {
      get
      {
        return _ProductivityFactor;
      }
      set
      {
        SetPropertyValue("ProductivityFactor", ref _ProductivityFactor, value);
      }
    }
    private short _StandardCostRate;
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public short StandardCostRate
    {
      get
      {
        return _StandardCostRate;
      }
      set
      {
        SetPropertyValue("StandardCostRate", ref _StandardCostRate, value);
      }
    }
    private short _fixedcost;
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public short FixedCost
    {
      get
      {
        return _fixedcost;
      }
      set
      {
        SetPropertyValue("FixedCost", ref _fixedcost, value);
      }
    }
    private short _ProjectSize;
    public short ProjectSize
    {
      get
      {
        //_ProjectSize = (short)(_ProductSize * TotalUsecaseEnvironmentalFactorsPoints * TotalUsecaseTechnicalFactorsPoints);
         //ATA change the method of calculation because it caused an error 
          // _ProjectSize = (short)(TotalUsecaseActorsPoints + TotalUsecaseCategoryPoints);
        
          return _ProjectSize;
      }
      set
      {
        SetPropertyValue("ProjectSize", ref _ProjectSize, value);
       
      }
    }
    private short _ProjectEfforts;
    public short ProjectEfforts
    {
      get
      {
        //_ProjectEfforts = (short)(_ProjectSize * ProductivityFactor);
        _ProjectEfforts = (short)(ProjectSize * ProductivityFactor);
        return _ProjectEfforts;
      }
      set
      {
        SetPropertyValue("ProjectEfforts", ref _ProjectEfforts, value);
      }
    }
    private short _EffortBasedCosts;
    public short EffortBasedCosts
    {
      get
      {
        _EffortBasedCosts = (short)(StandardCostRate * ProjectEfforts);
        return _EffortBasedCosts;
      }
      set
      {
        SetPropertyValue("EffortBasedCosts", ref _EffortBasedCosts, value);
      }
    }
    private Int16 _TotalProjectCosts;
    public short TotalProjectCosts
    {
      get
      {
        _TotalProjectCosts = (short)(EffortBasedCosts + FixedCost);
        return _TotalProjectCosts;
      }
      set
      {
        SetPropertyValue("TotalProjectCosts", ref _TotalProjectCosts, value);
      }
    }
    private short _TotalUsecaseCategoryPoints;
    public short TotalUsecaseCategoryPoints
    {
      get
      {
          //ATA 10/1/2017 comment it because it make an error while loading
        //if (this.UseCAsePointsCategories.Count > 0)
        //{
        //  _TotalUsecaseCategoryPoints = 0;
        //  foreach (QAUseCAsePointsCategory item in this.UseCAsePointsCategories)
        //  {
        //    _TotalUsecaseCategoryPoints += item.Totalweight;
        //  }
        //}
        return _TotalUsecaseCategoryPoints;
      }
      set
      {
        SetPropertyValue("TotalUsecaseCategoryPoints", ref _TotalUsecaseCategoryPoints, value);
        if (!IsLoading && !IsSaving && value != null)
        {
            _UseCaseTotalweight = (short)((TotalUsecaseActorsPoints + value) * TotalUsecaseEnvironmentalFactorsPoints * TotalUsecaseTechnicalFactorsPoints);
            _ProjectSize = (short)(TotalUsecaseActorsPoints + value);

        }
      }
    }
    private Int16 _TotalUsecaseActorsPoints;
    public Int16 TotalUsecaseActorsPoints
    {
      get
      {
        //ATA//if (this.UseCAsePointsCategories.Count > 0)
        //if (this.UsecaseActors.Count > 0)
        //{
        //  _TotalUsecaseActorsPoints = 0;
        //  foreach (QAUsecaseActors item in this.UsecaseActors)
        //  {
        //    _TotalUsecaseActorsPoints += item.UAW;
        //  }
        //}
        return _TotalUsecaseActorsPoints;
      }
      set
      {
        SetPropertyValue("TotalUsecaseActorsPoints", ref _TotalUsecaseActorsPoints, value);
        if (!IsLoading && !IsSaving && value != null)
        {
            _UseCaseTotalweight = (short)((TotalUsecaseActorsPoints + TotalUsecaseCategoryPoints) * TotalUsecaseEnvironmentalFactorsPoints * TotalUsecaseTechnicalFactorsPoints);
            _ProjectSize = (short)(TotalUsecaseActorsPoints + TotalUsecaseCategoryPoints);

        }
      }
    }
    private Int16 _TotalUsecaseTechnicalFactorsPoints;
    public Int16 TotalUsecaseTechnicalFactorsPoints
    {
      get
      {
          //ATA comment it because it is make an error while loading 
        //if (this.UseCaseTechnicalFactors.Count > 0)
        //{
        //  _TotalUsecaseTechnicalFactorsPoints = 0;
        //  foreach (QAUseCaseTechnicalFactors item in this.UseCaseTechnicalFactors)
        //  {
        //    _TotalUsecaseTechnicalFactorsPoints += item.Score;
        //  }
        //}
        return _TotalUsecaseTechnicalFactorsPoints;
      }
      set
      {
        SetPropertyValue("TotalUsecaseTechnicalFactorsPoints", ref _TotalUsecaseTechnicalFactorsPoints, value);
        if (!IsLoading && !IsSaving && value != null)
        {
            _UseCaseTotalweight = (short)((TotalUsecaseActorsPoints + TotalUsecaseCategoryPoints) * TotalUsecaseEnvironmentalFactorsPoints * value);
        }
      }
    }
    private Int16 _TotalUsecaseEnvironmentalFactorsPoints;
    public Int16 TotalUsecaseEnvironmentalFactorsPoints
    {
      get
      {
          //ATA comment it because it is make an error while loading 
        //if (this.UseCaseEnvironmentalFactors.Count > 0)
        //{
        //  _TotalUsecaseEnvironmentalFactorsPoints = 0;
        //  foreach (QAUseCaseEnvironmentalFactors item in this.UseCaseEnvironmentalFactors)
        //  {
        //    _TotalUsecaseEnvironmentalFactorsPoints += item.Score;
        //  }
        //}
        return _TotalUsecaseEnvironmentalFactorsPoints;
       
      }
      set
      {
        SetPropertyValue("TotalUsecaseEnvironmentalFactorsPoints", ref _TotalUsecaseEnvironmentalFactorsPoints, value);
        if (!IsLoading && !IsSaving && value != null)
        {
            _UseCaseTotalweight = (short)((TotalUsecaseActorsPoints + TotalUsecaseCategoryPoints) * value * TotalUsecaseTechnicalFactorsPoints);
        }
      }
    }
    [DevExpress.Xpo.AssociationAttribute("UseCaseEnvironmentalFactors-UseCasePoint")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUseCaseEnvironmentalFactors> UseCaseEnvironmentalFactors
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAUseCaseEnvironmentalFactors>("UseCaseEnvironmentalFactors");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("WBS-UseCasePoints")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAWBS> WBS
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAWBS>("WBS");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ResourceShares-UseCasePoints")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.QAResourceShare> ResourceShares
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.QAResourceShare>("ResourceShares");
      }
    }
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate Project
    {
      get
      {
        //ATA, 1,bgn,14/3/2016,check if _enddate not calculated and it is assign to a project then calculate it according to project type 
        if (_project != null && _enddate == DateTime.MinValue)
        {
          if (_project.Type.ToString() == "Key")
          {
            this._enddate = CalcEndate.CalcEnddate(_project.StartDate, 1);
            //this._enddate = CalcEnddate(_project.StartDate, 1);
          }
          else
          {
            this._enddate = _project.EndDate;
          }
        }
        //ATA , end
        return _project;
      }
      set
      {
        SetPropertyValue("Project", ref _project, value);
      }
    }
    //ATA , 1, bgn , define the end date property
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
    //ATA, end 
    //ATA automate NC's check for all phases 1/24/2017[Start]
    private ApproveStatu _approvestatus;
    public ApproveStatu ApproveStatus
    {
      get
      {
        return _approvestatus;
      }
      set
      {
        SetPropertyValue("ApproveStatus", ref _approvestatus, value);
      }
    }
    public enum ApproveStatu
    {
      New,
      Ready
    }
    //ATA automate NC's check for all phases 1/24/2017[End]
  }
}
