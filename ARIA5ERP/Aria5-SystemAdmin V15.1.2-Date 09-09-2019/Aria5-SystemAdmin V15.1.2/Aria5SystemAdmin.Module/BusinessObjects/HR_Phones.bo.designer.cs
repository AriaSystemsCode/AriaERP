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
  public partial class HR_Phones : DevExpress.Persistent.BaseImpl.BaseObject
  {
    public HR_Phones(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private System.String _extension;
    private System.String _phoneNumber;
    private HR_PhoneType _phonetype;
    private string _phoneTypeDescription;
    [Size(500)]
    public string PhoneTypeDescription
    {
      get
      {
        return _phoneTypeDescription;
      }
      set
      {
        _phoneTypeDescription = value;
      }
    }
    public HR_PhoneType PhoneType
    {
      get
      {
        return _phonetype;
      }
      set
      {
        _phonetype = value;
      }
    }
    [RuleRequiredField]
    public System.String PhoneNumber
    {
      get
      {
        return _phoneNumber;
      }
      set
      {
        SetPropertyValue("PhoneNumber", ref _phoneNumber, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(10)]
    public System.String Extension
    {
      get
      {
        return _extension;
      }
      set
      {
        SetPropertyValue("Extension", ref _extension, value);
      }
    }
    //[DevExpress.Xpo.AssociationAttribute("HR_Entities-HR_Phones")]
    //public XPCollection<HR_Entity> HREntities
    //{
    //  get
    //  {
    //    return GetCollection<HR_Entity>("HREntities");
    //  }
    //}
  }
}
