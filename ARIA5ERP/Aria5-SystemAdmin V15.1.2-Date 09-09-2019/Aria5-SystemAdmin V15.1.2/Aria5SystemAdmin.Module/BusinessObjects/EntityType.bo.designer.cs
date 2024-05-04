using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using Aria5.DevExpress.MainSystem.Module.BusinessObjects;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
    public partial class EntityType :DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.EntityType _parentType;
    private System.String _businessObject;
    private System.String _name;
    private System.String _typeId;
    public EntityType(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String TypeId
    {
      get
      {
        return _typeId;
      }
      set
      {
        SetPropertyValue("TypeId", ref _typeId, value);
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
    [DevExpress.Xpo.SizeAttribute(200)]
    public System.String BusinessObject
    {
      get
      {
        return _businessObject;
      }
      set
      {
        SetPropertyValue("BusinessObject", ref _businessObject, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityTypes-ParentType")]
    public Aria5SystemAdmin.Module.BusinessObjects.EntityType ParentType
    {
      get
      {
        return _parentType;
      }
      set
      {
        SetPropertyValue("ParentType", ref _parentType, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityTypes-ParentType")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityType> EntityTypes
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityType>("EntityTypes");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityCategories-EntityType")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityCategory> EntityCategories
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityCategory>("EntityCategories");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityClassifications-EntityType")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityClassification> EntityClassifications
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityClassification>("EntityClassifications");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("Entities-EntityType")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Entity> Entities
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Entity>("Entities");
      }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityStatuses-EntityType")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityStatus> EntityStatuses
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityStatus>("EntityStatuses");
      }
    }
      //ATA add identifierstructure to Entity Type 12/8/2016 [strat ]
    //private IdentifierStructure _identifierstruct;

    //public IdentifierStructure IdentifierStructture
    //{
    //    get { return _identifierstruct; }
    //    set { _identifierstruct = value; }
    //}
    //ATA add identifierstructure to Entity Type 12/8/2016 [End ]
    #region Commented
    //[DevExpress.Xpo.AssociationAttribute("DataFilters-EntityType")]
    //public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.DataFilter> DataFilters
    //{
    //  get
    //  {
    //    return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.DataFilter>("DataFilters");
    //  }
    //}
    //[DevExpress.Xpo.AssociationAttribute("DataSorts-EntityType")]
    //public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.DataSort> DataSorts
    //{
    //  get
    //  {
    //    return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.DataSort>("DataSorts");
    //  }
    //}
    //private DataSort _DefaultDataSort;
    //public DataSort DefaultDataSort
    //{
    //    get
    //    {
    //        return _DefaultDataSort;
    //    }
    //    set
    //    {
    //        SetPropertyValue("DefaultDataSort", ref _DefaultDataSort, value);
    //    }
    //}
    #endregion
    private System.Drawing.Image _Image;
    public System.Drawing.Image Image
    {
        get
        {
            return _Image;
        }
        set
        {
            SetPropertyValue("Image", ref _Image, value);
        }
    }

    private System.Boolean _ShowInFlyout;
    public System.Boolean ShowInFlyout
    {
        get
        {
            return _ShowInFlyout;
        }
        set
        {
            SetPropertyValue("ShowInFlyout", ref _ShowInFlyout, value);
        }
    }

    private System.Boolean _HasSyncDataToBeSendInLog;
    public System.Boolean HasSyncDataToBeSendInLog
    {
        get
        {
            return _HasSyncDataToBeSendInLog;
        }
        set
        {
            SetPropertyValue("HasSyncDataToBeSendInLog", ref _HasSyncDataToBeSendInLog, value);
        }
    }
  }
}
