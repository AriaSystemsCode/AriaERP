using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Persistent.Validation;
using Aria5SystemAdmin.Module;
using Aria5SystemAdmin.Module.BusinessObjects;
namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
  [DefaultClassOptions]
  [MapInheritance(MapInheritanceType.ParentTable)]
  [RelatedEntity("Aria5-Windows8Xaml-Person")]
  [DivisionAttribute(true)]
  [IsClient(true, false)]
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
    public Person(Session session)
      : base(session)
    {
    }
    [NonPersistentAttribute]
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
    [NonPersistentAttribute]
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
    [NonPersistentAttribute]
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
    [NonPersistentAttribute]
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
    [NonPersistentAttribute]
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
    [NonPersistentAttribute]
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
    [SizeAttribute(20)]
    [NonPersistentAttribute]
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
    [SizeAttribute(20)]
    [NonPersistentAttribute]
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
    [NonPersistentAttribute]
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
    [AssociationAttribute("Persons-Business")]
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
    [NonPersistentAttribute]
    public Department Department
    {
      get
      {
        return _department;
      }
      set
      {
        SetPropertyValue("Department", ref _department, value);
      }
    }
    [NonPersistentAttribute]
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
    [NonPersistentAttribute]
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
    [NonPersistentAttribute]
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
  //      return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ContactAddress>("ContactAddresses");
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
