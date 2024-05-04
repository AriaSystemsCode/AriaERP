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
    
    [RelatedEntity("Aria5-Windows8Xaml-IndoorMapProvider")]
    [DivisionAttribute(true)]
    [IsClient(true, false)]
    public partial class IndoorMapProvider : ClientBaseObject
  {
    private System.String _extraData;
    private System.String _name;
    private System.String _internalKey;
    private System.String _providerID;
    public IndoorMapProvider(Session session)
      : base(session)
    {
    }
    [SizeAttribute(30)]
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
    [SizeAttribute(-1)]
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
