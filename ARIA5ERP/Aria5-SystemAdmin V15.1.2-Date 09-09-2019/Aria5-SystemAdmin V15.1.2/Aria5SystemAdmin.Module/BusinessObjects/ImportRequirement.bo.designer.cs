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
using DevExpress.Persistent.Validation;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [NonPersistent]
  public partial class ImportRequirement : FileAttachmentBase
  {
    public ImportRequirement(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private Application_T _application;
    [RuleRequiredField]
    public Application_T Application
    {
      get
      {
        return _application;
      }
      set
      {
        SetPropertyValue<Application_T>("Application", ref _application, value);
      }
    }
  }
}