using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using Aria5SystemAdmin.Module;

namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
    [DefaultClassOptions]
    [RelatedEntity("Aria5-Windows8Xaml-InDoorMap")]
    [DivisionAttribute(true)]
    [IsClient(true, false)]

    public partial class InDoorMap : ClientBaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.Account _account;
   private  IndoorMapProvider _provider;
    private System.String _providerMapId;
    private System.String _name;
    private System.String _indoorMapId;
    public InDoorMap(Session session)
      : base(session)
    {
    }
    [SizeAttribute(30)]
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
    [SizeAttribute(50)]
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
    public IndoorMapProvider Provider
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
