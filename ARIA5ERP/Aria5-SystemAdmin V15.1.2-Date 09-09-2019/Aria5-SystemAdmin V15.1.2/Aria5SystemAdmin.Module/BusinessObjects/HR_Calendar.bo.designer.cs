﻿//------------------------------------------------------------------------------
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
using DevExpress.ExpressApp.Editors;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
    [NonPersistent]
    [XafDefaultProperty("Name")]
  [Appearance("Days", AppearanceItemType = "ViewItem", TargetItems = "Sat,Sun,Mon,Tue,Wed,Thu,Fri", Criteria = "RecurringType !='Weekly'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
  public partial class HR_Calendar : DevExpress.Persistent.BaseImpl.BaseObject
  {
    public HR_Calendar(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private string _name;
    private RecurringPeriod _recurringType;
   
    public string Name
    {
      get
      {
        return _name;
      }
      set
      {
        _name = value;
      }
    }
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public RecurringPeriod RecurringType
    {
      get
      {
        return _recurringType;
      }
      set
      {
        _recurringType = value;
      }
    }

    private bool _sat;
    private bool _sun;
    private bool _mon;
    private bool _tue;
    private bool _wed;
    private bool _Thu;
    private bool _fri;
    private int _repeatedEvery;
    private string _summry;
    public bool Sat
    {
      get
      {
        return _sat;
      }
      set
      {
        _sat = value;
      }
    }
    public bool Sun
    {
      get
      {
        return _sun;
      }
      set
      {
        _sun = value;
      }
    }
    public bool Mon
    {
      get
      {
        return _mon;
      }
      set
      {
        _mon = value;
      }
    }
    public bool Tue
    {
      get
      {
        return _tue;
      }
      set
      {
        _tue = value;
      }
    }
    public bool Wed
    {
      get
      {
        return _wed;
      }
      set
      {
        _wed = value;
      }
    }
    public bool Thu
    {
      get
      {
        return _Thu;
      }
      set
      {
        _Thu = value;
      }
    }
    public bool Fri
    {
      get
      {
        return _fri;
      }
      set
      {
        _fri = value;
      }
    }
    public int RepeatedEvery
    {
      get
      {
        return _repeatedEvery;
      }
      set
      {
        _repeatedEvery = value;
      }
    }
    public string Summry
    {
      get
      {
        return _summry;
      }
      set
      {
        _summry = value;
      }
    }
    //[Association("HR_Activity-HR_Calendar")]
    //public XPCollection<HR_Activity> Activities
    //{
    //  get
    //  {
    //    return GetCollection<HR_Activity>("Activities");
    //  }
    //}
  }

}