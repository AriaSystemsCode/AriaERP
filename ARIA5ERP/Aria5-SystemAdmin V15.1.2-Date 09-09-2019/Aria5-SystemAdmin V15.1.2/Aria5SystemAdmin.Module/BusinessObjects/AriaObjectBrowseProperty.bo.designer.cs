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
  public partial class AriaObjectBrowseProperty : XPCustomObject
  {
    private System.String _keyProperty;
    private System.String _propertyOid;
    private Aria5SystemAdmin.Module.BusinessObjects.PropertyType _propertyType;
    private System.String _propertyDescription;
    private System.String _propertyName;
    public AriaObjectBrowseProperty(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public System.String PropertyName
    {
      get
      {
        return _propertyName;
      }
      set
      {
        SetPropertyValue("PropertyName", ref _propertyName, value);
      }
    }
    public Aria5SystemAdmin.Module.BusinessObjects.PropertyType PropertyType
    {
      get
      {
        return _propertyType;
      }
      set
      {
        SetPropertyValue("PropertyType", ref _propertyType, value);
      }
    }
    public System.String PropertyDescription
    {
      get
      {
        return _propertyDescription;
      }
      set
      {
        SetPropertyValue("PropertyDescription", ref _propertyDescription, value);
      }
    }
    public System.String PropertyOid
    {
      get
      {
        return _propertyOid;
      }
      set
      {
        SetPropertyValue("PropertyOid", ref _propertyOid, value);
      }
    }
    [DevExpress.Xpo.KeyAttribute]
    public System.String KeyProperty
    {
      get
      {
        return _keyProperty;
      }
      set
      {
        SetPropertyValue("KeyProperty", ref _keyProperty, value);
      }
    }
  }
}