using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using Aria5SystemAdmin.Module;
namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
  [DefaultClassOptions]
  [MapInheritance(MapInheritanceType.ParentTable)]
  [RelatedEntity("Aria5-Windows8Xaml-Business")]
  [IsClient(true, true)]

  public partial class Business : Contact
  {
    private System.Int16 _guide;
    private System.String _internalContactNo;
    private System.String _tradeName;
    public Business(Session session)
      : base(session)
    {
    }
    [NonPersistentAttribute]
    public System.String TradeName
    {
      get
      {
        return _tradeName;
      }
      set
      {
        SetPropertyValue("TradeName", ref _tradeName, value);
      }
    }
    [NonPersistentAttribute]
    public System.String InternalContactNo
    {
      get
      {
        return _internalContactNo;
      }
      set
      {
        SetPropertyValue("InternalContactNo", ref _internalContactNo, value);
      }
    }
    [AssociationAttribute("Persons-Business")]
    public XPCollection<Person> Persons
    {
      get
      {
        return GetCollection<Person>("Persons");
      }
    }
    //[AssociationAttribute("Divisions-Business")]
    //public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Division> Divisions
    //{
    //  get
    //  {
    //    return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Division>("Divisions");
    //  }
    //}
    //[AssociationAttribute("Locations-Business")]
    //public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Location> Locations
    //{
    //  get
    //  {
    //    return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Location>("Locations");
    //  }
    //}
    public System.Int16 Guide
    {
      get
      {
        return _guide;
      }
      set
      {
        SetPropertyValue("Guide", ref _guide, value);
      }
    }
  }
}
