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
  [MapInheritance(MapInheritanceType.ParentTable)]
  [System.ComponentModel.DefaultProperty("Description")]
  [RelatedEntity("Aria5-SystemAdmin-Department")]
  public partial class Department : Entity
  {
    private Aria5SystemAdmin.Module.BusinessObjects.QAResourceShare _qAResourceShare;
    public Department(DevExpress.Xpo.Session session)
      : base(session)
    {
    }


   // private HR_Company _company;
    private HREmployee _headofdepartment;


    public HREmployee HeadOfDepartment
    {
        get { return _headofdepartment; }
        set { _headofdepartment = value; }
    }

    //[Association("HR_Company-Department")]
    //public HR_Company Company
    //{
    //    get { return _company; }
    //    set { _company = value; }
    //}
    public Aria5SystemAdmin.Module.BusinessObjects.QAResourceShare QAResourceShare
    {
      get
      {
        return _qAResourceShare;
      }
      set
      {

          SetPropertyValue("QAResourceShare", ref _qAResourceShare, value);
        //if (_qAResourceShare == value)
        //  return;
        //Aria5SystemAdmin.Module.BusinessObjects.QAResourceShare prevQAResourceShare = _qAResourceShare;
        //_qAResourceShare = value;
        //if (IsLoading)
        //  return;
        //Be sure no need to it
        //if (prevQAResourceShare != null && prevQAResourceShare.Department)
        //  prevQAResourceShare.Department Name = null;
        //if (_qAResourceShare != null)
        //  _qAResourceShare.Department Name = this;
      //  OnChanged("QAResourceShare");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Resourceses-Department")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Resources> Resourceses
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Resources>("Resourceses");
      }
    }
     [DevExpress.Xpo.AssociationAttribute("Resource_DOT-Department")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Department_DOT> DepartmentDOT
    {
        get
        {
            return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Department_DOT>("DepartmentDOT");
        }
    }


     //[Association("HREmployee-Department")]
     //public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.HREmployee> Employees
     //{
     //    get
     //    {
     //        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.HREmployee>("Employees");
     //    }
     //}
     [Association("HRJobPosition-Department")]
     public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.HRJobPosition> JobPositions
     {
         get
         {
             return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.HRJobPosition>("JobPositions");
         }
     }
     [Association("HR_ActivityCategory-Department")]
     public XPCollection<HR_ActivityCategory> Categories
     {
         get
         {
             return GetCollection<HR_ActivityCategory>("Categories");
         }
     }
     //[Association("HR_Activity-Department")]
     //public XPCollection<HR_Activity> Activities
     //{
     //    get
     //    {
     //        return GetCollection<HR_Activity>("Activities");
     //    }
     //}
  }
}
