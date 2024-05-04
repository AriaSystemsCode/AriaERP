using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Persistent.Validation;
using System.ComponentModel;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
    //ATA make the number is the display property 7/9/2017[start]
  [XafDefaultProperty("PhoneNumber")]
    //ATA make the number is the display property 7/9/2017[End]
  public partial class ContactPhone : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.String _phoneTypeDescription;
    private System.String _extension;
    private System.String _phoneNumber;
    private Contact _contact;
    public ContactPhone(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    private PhoneType phonetype;
    public PhoneType PhoneType
    {
      get
      {
        return phonetype;
      }
      set
      {
        SetPropertyValue("PhoneType", ref phonetype, value);
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
    [DevExpress.Xpo.AssociationAttribute("ContactPhones-Contact")]
    [RuleRequiredField]
      //AtA 
        [Browsable(false)]
    public Contact Contact
    {
      get
      {
        return _contact;
      }
      set
      {
        SetPropertyValue("Contact", ref _contact, value);
      }
    }
      //ATA to enhance the phone screen to be used in HRIS
   // [RuleRequiredField]
      //ATA 
    public System.String PhoneTypeDescription
    {
      get
      {
        return _phoneTypeDescription;
      }
      set
      {
        SetPropertyValue("PhoneTypeDescription", ref _phoneTypeDescription, value);
      }
    }
      private HR_Branches _branch;
      [Association("ContactPhone-HR_Branches")]
      public HR_Branches Branch
      {
          get
          {
              return _branch;
          }
          set
          {
              _branch = value;
          }
      }
  }
}
