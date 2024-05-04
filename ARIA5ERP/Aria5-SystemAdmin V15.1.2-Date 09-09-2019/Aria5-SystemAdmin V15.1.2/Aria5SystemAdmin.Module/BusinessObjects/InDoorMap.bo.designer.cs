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
  public partial class InDoorMap : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.Account _account;
    private Aria5SystemAdmin.Module.BusinessObjects.IndoorMapProvider _provider;
    private System.String _providerMapId;
    private System.String _name;
    private System.String _indoorMapId;
    public InDoorMap(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String IndoorMapId
    {
      get
      {
        return _indoorMapId;
      }
      set
      {
        SetPropertyValue("IndoorMapId", ref _indoorMapId, value);
      }
    }
    public System.String Name
    {
      get
      {
        return _name;
      }
      set
      {
        SetPropertyValue("Name", ref _name, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(50)]
    public System.String ProviderMapId
    {
      get
      {
        return _providerMapId;
      }
      set
      {
        SetPropertyValue("ProviderMapId", ref _providerMapId, value);
      }
    }
    public Aria5SystemAdmin.Module.BusinessObjects.IndoorMapProvider Provider
    {
      get
      {
        return _provider;
      }
      set
      {
        SetPropertyValue("Provider", ref _provider, value);
      }
    }
    public Aria5SystemAdmin.Module.BusinessObjects.Account Account
    {
      get
      {
        return _account;
      }
      set
      {
        SetPropertyValue("account", ref _account, value);
      }
    }
    private System.Guid _Client;
    public System.Guid Client
    {
        get
        {
            return _Client;
        }
        set
        {
            SetPropertyValue("Client", ref _Client, value);
        }
    }
  }
}
