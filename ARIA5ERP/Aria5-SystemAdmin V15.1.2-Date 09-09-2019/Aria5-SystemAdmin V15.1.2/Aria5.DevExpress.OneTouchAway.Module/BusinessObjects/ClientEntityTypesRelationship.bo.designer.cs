//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------
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
  [IsClient(true, true)]
  [RelatedEntity("Aria5-Windows8Xaml-ClientEntityTypesRelationship")]

  public partial class ClientEntityTypesRelationship : ClientBaseObject
  {
    private System.String _relatedType;
    private Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityType _relatedEntityType;
    private Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityType _entityType;
    public ClientEntityTypesRelationship(Session session)
      : base(session)
    {
    }
    public Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityType EntityType
    {
      get
      {
        return _entityType;
      }
      set
      {
        SetPropertyValue("EntityType", ref _entityType, value);
      }
    }
    public Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityType RelatedEntityType
    {
      get
      {
        return _relatedEntityType;
      }
      set
      {
        SetPropertyValue("RelatedEntityType", ref _relatedEntityType, value);
      }
    }
    [SizeAttribute(10)]
    public System.String RelatedType
    {
      get
      {
        return _relatedType;
      }
      set
      {
        SetPropertyValue("RelatedType", ref _relatedType, value);
      }
    }
    private System.Boolean _AllowAdd;
    public System.Boolean AllowAdd
    {
        get
        {
            return _AllowAdd;
        }
        set
        {
            SetPropertyValue("AllowAdd", ref _AllowAdd, value);
        }
    }

    private System.Boolean _AllowLink;
    public System.Boolean AllowLink
    {
        get
        {
            return _AllowLink;
        }
        set
        {
            SetPropertyValue("AllowLink", ref _AllowLink, value);
        }
    }
    private System.String _RelatedEntityTypeName;
    public System.String RelatedEntityTypeName
    {
        get
        {
            return _RelatedEntityTypeName;
        }
        set
        {
            SetPropertyValue("RelatedEntityTypeName", ref _RelatedEntityTypeName, value);
        }
    }

    private System.Int32 _RelatedEntityTypeOrder;
    public System.Int32 RelatedEntityTypeOrder
    {
        get
        {
            return _RelatedEntityTypeOrder;
        }
        set
        {
            SetPropertyValue("RelatedEntityTypeOrder", ref _RelatedEntityTypeOrder, value);
        }
    }
    private System.String _RelatedEntityTypeId;
    public System.String RelatedEntityTypeId
    {
        get
        {
            return _RelatedEntityTypeId;
        }
        set
        {
            SetPropertyValue("RelatedEntityTypeId", ref _RelatedEntityTypeId, value);
        }
    }
    private System.String _EntityTypeId;
    public System.String EntityTypeId
    {
        get
        {
            return _EntityTypeId;
        }
        set
        {
            SetPropertyValue("EntityTypeId", ref _EntityTypeId, value);
        }
    }


    // POC Testing 19-4 [START]
    private System.Boolean _HasOneParent;
    [SizeAttribute(10)]

    public System.Boolean HasOneParent
    {
        get
        {
            return _HasOneParent;
        }
        set
        {
            SetPropertyValue("HasOneParent", ref _HasOneParent, value);
        }
    }

      //    POC Testing 19-4 [END]

  }
}