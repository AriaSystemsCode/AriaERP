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
  [System.ComponentModel.DefaultProperty("Description")]
  public partial class Language : Entity
  {
    public Language(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.AssociationAttribute("Countries-Language")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.CountryTS> Countries
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.CountryTS>("Countries");
      }
    }
    private System.String _Name;
    public System.String Name
    {
        get
        {
            return _Name;
        }
        set
        {
            SetPropertyValue("Name", ref _Name, value);
        }
    }

    private System.String _Code;
    public System.String Code
    {
        get
        {
            return _Code;
        }
        set
        {
            SetPropertyValue("Code", ref _Code, value);
        }
    }
  }
}
