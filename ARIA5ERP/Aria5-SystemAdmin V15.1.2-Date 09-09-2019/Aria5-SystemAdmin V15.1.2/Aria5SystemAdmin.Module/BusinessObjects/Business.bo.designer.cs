using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [MapInheritance(MapInheritanceType.ParentTable)]
  [RelatedEntity("Aria5-Windows8Xaml-Business")]
  public partial class Business : Contact
  {
    private System.Int16 _guide;
    private Aria5SystemAdmin.Module.BusinessObjects.ADS _aDS;
    private System.String _internalContactNo;
    private System.String _tradeName;
    public Business(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.NonPersistentAttribute]
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
    [DevExpress.Xpo.NonPersistentAttribute]
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
    [DevExpress.Xpo.AssociationAttribute("Persons-Business")]
    public XPCollection<Person> Persons
    {
      get
      {
        return GetCollection<Person>("Persons");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Divisions-Business")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Division> Divisions
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Division>("Divisions");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Locations-Business")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Location> Locations
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Location>("Locations");
      }
    }
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
