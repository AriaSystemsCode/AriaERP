using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.Security;
using DevExpress.ExpressApp;
using DevExpress.Data.Filtering;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [MapInheritance(MapInheritanceType.ParentTable)]
  [RelatedEntity("Aria5-Windows8Xaml-Person")]
  [DivisionAttribute(true)]
  public partial class Person : Contact
  {
    private System.String _positionTitle;
    private System.String _departmentName;
    private Position _position;
    private Department _department;
    private Business _business;
    private System.String _salutation;
    private System.String _sSN;
    private TitleOfCourtesy _titleOfCourtesy;
    private System.String _spouseName;
    private System.DateTime _birthDate;
    private System.String _nickName;
    private System.String _lastName; 
    private System.String _middleName;
    private System.String _firstName;
    XPCollection<Division> ListofDevisions = null;
    public Person(DevExpress.Xpo.Session session)
      : base(session)
    {
        
    }
  [DevExpress.Xpo.NonPersistentAttribute]
    [RuleRequiredField]
    public System.String FirstName
    {
      get
      {
        return _firstName;
      }
      set
      {
        SetPropertyValue("FirstName", ref _firstName, value);
      }
    }
  [DevExpress.Xpo.NonPersistentAttribute]
    public System.String MiddleName
    {
      get
      {
        return _middleName;
      }
      set
      {
        SetPropertyValue("MiddleName", ref _middleName, value);
      }
    }
 [DevExpress.Xpo.NonPersistentAttribute]
    [RuleRequiredField]
    public System.String LastName
    {
      get
      {
        return _lastName;
      }
      set
      {
        SetPropertyValue("LastName", ref _lastName, value);
      }
    }
  [DevExpress.Xpo.NonPersistentAttribute]
    public System.String NickName
    {
      get
      {
        return _nickName;
      }
      set
      {
        SetPropertyValue("NickName", ref _nickName, value);
      }
    }
  [DevExpress.Xpo.NonPersistentAttribute]
    public System.DateTime BirthDate
    {
      get
      {
        return _birthDate;
      }
      set
      {
        SetPropertyValue("BirthDate", ref _birthDate, value);
      }
    }
   [DevExpress.Xpo.NonPersistentAttribute]
    public System.String SpouseName
    {
      get
      {
        return _spouseName;
      }
      set
      {
        SetPropertyValue("SpouseName", ref _spouseName, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(20)]
    [DevExpress.Xpo.NonPersistentAttribute]
    public TitleOfCourtesy TitleOfCourtesy
    {
      get
      {
        return _titleOfCourtesy;
      }
      set
      {
        SetPropertyValue("TitleOfCourtesy", ref _titleOfCourtesy, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(20)]
   [DevExpress.Xpo.NonPersistentAttribute]
    public System.String SSN
    {
      get
      {
        return _sSN;
      }
      set
      {
        SetPropertyValue("SSN", ref _sSN, value);
      }
    }
   [DevExpress.Xpo.NonPersistentAttribute]
    public System.String Salutation
    {
      get
      {
        return _salutation;
      }
      set
      {
        SetPropertyValue("Salutation", ref _salutation, value);
      }
    }


      //ATA add this datasourceproperty to detrmin whic divisio to choose for this contact 
      [DataSourceProperty("ListofDevisions")]
   //ATA add this datasourceproperty to detrmin whic divisio to choose for this contact 
    [DevExpress.Xpo.AssociationAttribute("Persons-Business")]
    public Business Business
    {
      get
      {
        return _business;
      }
      set
      {
        SetPropertyValue("Business", ref _business, value);
      }
    }
      //ATA make it persistanat to be able to use it in filtercriteria [Start]
  //[DevExpress.Xpo.NonPersistentAttribute]
      //ATA make it persistanat to be able to use it in filtercriteria [End]

      //ATA to fill the department name field automatic 12/25/2016 [start]
      [ImmediatePostData]
    public Department Department
    {
      get
      {
          if (_department != null)
          {
              _departmentName = _department.Description;
          }
          //ATA to fill the department name field automatic 12/25/2016 [End]

        return _department;
      }
      set
      {
        SetPropertyValue("Department", ref _department, value);
      }
    }
   [DevExpress.Xpo.NonPersistentAttribute]
    public Position Position
    {
      get
      {
        return _position;
      }
      set
      {
        SetPropertyValue("Position", ref _position, value);
      }
    }
   [DevExpress.Xpo.NonPersistentAttribute]
    public System.String DepartmentName
    {
      get
      {
        return _departmentName;
      }
      set
      {
        SetPropertyValue("DepartmentName", ref _departmentName, value);
      }
    }
 [DevExpress.Xpo.NonPersistentAttribute]
    public System.String PositionTitle
    {
      get
      {
        return _positionTitle;
      }
      set
      {
        SetPropertyValue("PositionTitle", ref _positionTitle, value);
      }
    }
  //  [DevExpress.Xpo.AssociationAttribute("ContactAddresses-Person")]
  //  public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ContactAddress> ContactAddresses
  //  {
  //    get
  //    {
  ////      return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ContactAddress>("ContactAddresses");
  //    }
  //  }
  }
  public enum TitleOfCourtesy
  {
    Dr,
    Miss,
    Mr,
    Mrs,
    Ms
  };
}
