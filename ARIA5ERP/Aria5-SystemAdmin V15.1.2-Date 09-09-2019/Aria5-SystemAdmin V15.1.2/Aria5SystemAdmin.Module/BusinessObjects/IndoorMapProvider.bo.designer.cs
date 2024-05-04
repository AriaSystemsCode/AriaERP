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
  public partial class IndoorMapProvider : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.String _extraData;
    private System.String _name;
    private System.String _internalKey;
    private System.String _providerID;
    public IndoorMapProvider(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String ProviderID
    {
      get
      {
        return _providerID;
      }
      set
      {
        SetPropertyValue("ProviderID", ref _providerID, value);
      }
    }
    public System.String InternalKey
    {
      get
      {
        return _internalKey;
      }
      set
      {
        SetPropertyValue("InternalKey", ref _internalKey, value);
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
    [DevExpress.Xpo.SizeAttribute(-1)]
    public System.String ExtraData
    {
      get
      {
        return _extraData;
      }
      set
      {
        SetPropertyValue("ExtraData", ref _extraData, value);
      }
    }
  }
}
