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
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-Projects Delivery on Time")]
  public partial class Projects_Delivery_on_Time : DevExpress.Persistent.BaseImpl.BaseObject
  {
    public Projects_Delivery_on_Time(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private string _projectname;
    public string ProjectName
    {
      get
      {
        return _projectname;
      }
      set
      {
        SetPropertyValue("ProjectName", ref _projectname, value);
      }
    }
    private int _numberoftasks;
    public int Numberoftasks
    {
      get
      {
        return _numberoftasks;
      }
      set
      {
        SetPropertyValue("Numberoftasks", ref _numberoftasks, value);
      }
    }
    private int _numberofotdtasks;
    public int NumberofOTDTasks
    {
      get
      {
        return _numberofotdtasks;
      }
      set
      {
        SetPropertyValue("NumberofOTDTasks", ref _numberofotdtasks, value);
      }
    }
    private long _id;
    public long Id
    {
      get
      {
        return _id;
      }
      set
      {
        SetPropertyValue("Id", ref _id, value);
      }
    }
    private long _deliveryontime;
    public long Deliveryontime
    {
      get
      {
        return _deliveryontime;
      }
      set
      {
        SetPropertyValue("Deliveryontime", ref _deliveryontime, value);
      }
    }
    private double _otd;
    public double OTD
    {
      get
      {
        return _otd;
      }
      set
      {
        SetPropertyValue("OTD", ref _otd, value);
      }
    }
    private long _total;
    public long Total
    {
      get
      {
        return _total;
      }
      set
      {
        SetPropertyValue("Total", ref _total, value);
      }
    }
    private DeliveryOnTime _dot;
    [DevExpress.Xpo.AssociationAttribute("Projects_Delivery_on_Time-DeliveryOnTime")]
    public DeliveryOnTime Dot
    {
      get
      {
        return _dot;
      }
      set
      {
        SetPropertyValue("Dot", ref _dot, value);
      }
    }
    private bool _ismaster;
    public bool IsMaster
    {
      get
      {
        return _ismaster;
      }
      set
      {
        SetPropertyValue("IsMaster", ref _ismaster, value);
      }
    }
    private DateTime _createdate;
    public DateTime CreateDate
    {
      get
      {
        return _createdate;
      }
      set
      {
        SetPropertyValue("CreateDate", ref _createdate, value);
      }
    }
  }
}