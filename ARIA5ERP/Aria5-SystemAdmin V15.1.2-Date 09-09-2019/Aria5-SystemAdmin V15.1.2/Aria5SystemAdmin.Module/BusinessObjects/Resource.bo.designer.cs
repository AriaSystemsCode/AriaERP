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
  [RelatedEntity("Aria5-Windows8Xaml-Resource")]
  [DivisionAttribute(true)]
    [MapInheritance(MapInheritanceType.ParentTable)]
  public partial class Resource : Aria5SystemAdmin.Module.BusinessObjects.Entity
  {
    private System.Boolean _active;
    private System.Int32 _capacity;
    private System.String _idResource;
    private System.String _resourceStatus;
    public Resource(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.NonPersistentAttribute]
    public System.String ResourceStatus
    {
      get
      {
        return _resourceStatus;
      }
      set
      {
        SetPropertyValue("ResourceStatus", ref _resourceStatus, value);
      }
    }
    [DevExpress.Xpo.NonPersistentAttribute]
    public System.String IdResource
    {
      get
      {
          return _idResource;
      }
      set
      {
          SetPropertyValue("IdResource", ref _idResource, value);
      }
    }
    [DevExpress.Xpo.NonPersistentAttribute]
    public System.Int32 Capacity
    {
      get
      {
        return _capacity;
      }
      set
      {
        SetPropertyValue("Capacity", ref _capacity, value);
      }
    }
    [DevExpress.Xpo.NonPersistentAttribute]
    public System.Boolean Active
    {
      get
      {
        return _active;
      }
      set
      {
        SetPropertyValue("Active", ref _active, value);
      }
    }
  }
}