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
namespace Module1.BusinessObjects
{
  [DefaultClassOptions]
  public partial class BusinessObject2 : DevExpress.Persistent.BaseImpl.BaseObject
  {
    public BusinessObject2(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.AssociationAttribute(typeof(BusinessObject1))]
    public XPCollection<Module1.BusinessObjects.BusinessObject1> BusinessObject1s
    {
      get
      {
        return GetCollection<Module1.BusinessObjects.BusinessObject1>("BusinessObject1s");
      }
    }
  }
}
