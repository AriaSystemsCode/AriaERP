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
using Aria5SystemAdmin.Module;
namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
  [DefaultClassOptions]
  [IsClient(true, false)]
  [RelatedEntity("Aria5-Windows8Xaml-DataFilter")]
  public partial class DataFilter : ClientBaseObject
  {
    private Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityType _entityType;
    private System.String _name;
    private System.String _filterId;
    public DataFilter(Session session)
      : base(session)
    {
    }
    [SizeAttribute(30)]
    public System.String FilterId
    {
      get
      {
        return _filterId;
      }
      set
      {
        SetPropertyValue("FilterId", ref _filterId, value);
      }
    }
    [SizeAttribute(100)]
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
    public Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityType EntityType
    {
      get
      {
        return _entityType;
      }
      set
      {
        SetPropertyValue("EntityType", ref _entityType, value);
      }
    }
  }
}