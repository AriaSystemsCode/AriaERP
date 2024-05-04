using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [MapInheritance(MapInheritanceType.ParentTable)]
  public partial class Division : Business
  {
    private System.String _dBSchema;
    private System.String _schemaName;
    private Aria5SystemAdmin.Module.BusinessObjects.Business _business;
    public Division(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.AssociationAttribute("Divisions-Business")]
    public Aria5SystemAdmin.Module.BusinessObjects.Business Business
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
    [DevExpress.Xpo.SizeAttribute(128)]
    public System.String SchemaName
    {
      get
      {
        return _schemaName;
      }
      set
      {
        SetPropertyValue("SchemaName", ref _schemaName, value);
      }
    }
    public System.String DBSchema
    {
      get
      {
        return _dBSchema;
      }
      set
      {
        SetPropertyValue("DBSchema", ref _dBSchema, value);
      }
    }

    //  //add Employee table to division screen 12/8/2016 [start]
    //  [Association("Division-Employees")]
    //public XPCollection<HREmployee> Employees
    //{
    //    get
    //    {
    //        return GetCollection<HREmployee>("Employees");
    //    }
    //}
    //  //add Employee table to division screen 12/8/2016 [End]
      
  }
}
