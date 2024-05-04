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
  public partial class DataFilterColumn : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.Boolean _Is;
    private Aria5SystemAdmin.Module.BusinessObjects.DataFilter _filter;
    private System.String _valueType;
    private System.String _value;
    private System.String _name;
    private System.String _operator;
    public DataFilterColumn(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(50)]
    public System.String Operator
    {
      get
      {
        return _operator;
      }
      set
      {
        SetPropertyValue("Operator", ref _operator, value);
      }
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
    public System.String Value
    {
      get
      {
        return _value;
      }
      set
      {
        SetPropertyValue("Value", ref _value, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(20)]
    public System.String ValueType
    {
      get
      {
        return _valueType;
      }
      set
      {
        SetPropertyValue("ValueType", ref _valueType, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("DataFilterColumns-Filter")]
    public Aria5SystemAdmin.Module.BusinessObjects.DataFilter Filter
    {
      get
      {
        return _filter;
      }
      set
      {
        SetPropertyValue("Filter", ref _filter, value);
      }
    }
    private System.Int16 _Order;
    public System.Int16 Order
    {
      get
      {
        return _Order;
      }
      set
      {
        SetPropertyValue("Order", ref _Order, value);
      }
    }
    private System.String _Conjunction;
    public System.String Conjunction
    {
      get
      {
        return _Conjunction;
      }
      set
      {
        SetPropertyValue("Conjunction", ref _Conjunction, value);
      }
    }
    public System.Boolean Is
    {
      get
      {
        return _Is;
      }
      set
      {
        SetPropertyValue("Is", ref _Is, value);
      }
    }
    private System.String _Value1;
    public System.String Value1
    {
      get
      {
        return _Value1;
      }
      set
      {
        SetPropertyValue("Value1", ref _Value1, value);
      }
    }
    private System.String _Value2;
    public System.String Value2
    {
      get
      {
        return _Value2;
      }
      set
      {
        SetPropertyValue("Value2", ref _Value2, value);
      }
    }
  }
}
