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
    [DivisionAttribute(true)]
    [RelatedEntity("Aria5-Windows8Xaml-ClientEntity")]

    public partial class ClientEntity : ClientBaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.Division _division;
    private Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityType _type;
    private Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityClassification _classification;
    private Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityCategory _category;
    private Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityStatus _status;
    private System.DateTime _addDate;
    private System.String _extraData;
    private System.String _classificationId;
    private System.String _categoryId;
    private System.String _typeId;
    private System.String _statusId;
    private System.String _notes;
    private System.String _description;
    private System.String _entityId;
    public ClientEntity(Session session)
      : base(session)
    {
    }

    public Aria5SystemAdmin.Module.BusinessObjects.Division Division
    {
        get
        {
            return _division;
        }
        set
        {
            SetPropertyValue("Division", ref _division, value);
        }
    }


    [SizeAttribute(30)]
    public System.String EntityId
    {
      get
      {
        return _entityId;
      }
      set
      {
        SetPropertyValue("EntityId", ref _entityId, value);
      }
    }
    [SizeAttribute(1024)]
    public System.String Description
    {
      get
      {
        return _description;
      }
      set
      {
        SetPropertyValue("Description", ref _description, value);
      }
    }
    [SizeAttribute(SizeAttribute.Unlimited)]
    public System.String Notes
    {
      get
      {
        return _notes;
      }
      set
      {
        SetPropertyValue("Notes", ref _notes, value);
      }
    }
    public Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityType Type
    {
      get
      {
        return _type;
      }
      set
      {
        SetPropertyValue("Type", ref _type, value);
      }
    }
    [SizeAttribute(30)]
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
    public Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityStatus Status
    {
      get
      {
        return _status;
      }
      set
      {
        SetPropertyValue("Status", ref _status, value);
      }
    }
    [SizeAttribute(30)]
    public System.String StatusId
    {
      get
      {
        return _statusId;
      }
      set
      {
        SetPropertyValue("StatusId", ref _statusId, value);
      }
    }
    public Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityCategory Category
    {
      get
      {
        return _category;
      }
      set
      {
        SetPropertyValue("Category", ref _category, value);
      }
    }
    [SizeAttribute(30)]
    public System.String CategoryId
    {
      get
      {
        return _categoryId;
      }
      set
      {
        SetPropertyValue("CategoryId", ref _categoryId, value);
      }
    }
    public Aria5.DevExpress.OneTouchAway.Module.BusinessObjects.ClientEntityClassification Classification
    {
      get
      {
        return _classification;
      }
      set
      {
        SetPropertyValue("Classification", ref _classification, value);
      }
    }
    [SizeAttribute(30)]
    public System.String ClassificationId
    {
      get
      {
        return _classificationId;
      }
      set
      {
        SetPropertyValue("ClassificationId", ref _classificationId, value);
      }
    }
    [SizeAttribute(SizeAttribute.Unlimited)]
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
    public System.DateTime AddDate
    {
      get
      {
        return _addDate;
      }
      set
      {
        SetPropertyValue("AddDate", ref _addDate, value);
      }
    }
    private System.String _GUID;
    [SizeAttribute(40)]
    public System.String GUID
    {
        get
        {
            return _GUID;
        }
        set
        {
            SetPropertyValue("GUID", ref _GUID, value);
        }
    }
    private Guid _Account;
    public Guid Account
    {
        get
        {
            return _Account;
        }
        set
        {
            SetPropertyValue("Account", ref _Account, value);
        }
    }
    //18-05-2016 Threading Problem due to static property [Start]
    public static Boolean CallFromSync { get; set; }


    //18-05-2016 Threading Problem due to static property [End]
  }
}