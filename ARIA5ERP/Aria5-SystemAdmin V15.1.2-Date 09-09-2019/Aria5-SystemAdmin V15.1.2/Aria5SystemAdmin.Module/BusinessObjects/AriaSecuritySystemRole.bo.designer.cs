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
using DevExpress.ExpressApp.Security.Strategy;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [RelatedEntity("Aria5-SystemAdmin-AriaSecuritySystemRole")]
  public partial class AriaSecuritySystemRole : SecuritySystemRole
  {
    private Aria5SystemAdmin.Module.BusinessObjects.Account _account;
    public AriaSecuritySystemRole(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public Aria5SystemAdmin.Module.BusinessObjects.Account Account
    {
      get
      {
        return _account;
      }
      set
      {
        SetPropertyValue("Account", ref _account, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityOperationPermissions-AriaSecuritySystemRole")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityOperationPermission> EntityOperationPermissions
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityOperationPermission>("EntityOperationPermissions");
      }
    }
  }
}