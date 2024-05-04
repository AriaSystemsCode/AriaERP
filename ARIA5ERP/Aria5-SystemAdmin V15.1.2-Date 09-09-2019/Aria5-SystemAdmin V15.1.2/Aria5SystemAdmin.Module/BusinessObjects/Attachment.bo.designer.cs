using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.ExpressApp.Web;
using DevExpress.Data.Filtering;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.ExpressApp.Editors;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [DevExpress.ExpressApp.DC.XafDefaultPropertyAttribute("Name")]
   //ATA 
    [Appearance("linkcontext", AppearanceItemType = "ViewItem", TargetItems = "link", Criteria = "AttachmentType !='Link'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    [Appearance("Filemenu", AppearanceItemType = "ViewItem", TargetItems = "File", Criteria = "AttachmentType !='File'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    [FileAttachmentAttribute("File")]
    //ATA 
    public partial class Attachment : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private System.Drawing.Image _attachmentFile;
    private System.String _attachmentID;
    private System.String _view;
    private System.String _resolution;
    private System.String _theme;
    private System.String _name;
    private System.String _location;
    public Attachment(DevExpress.Xpo.Session session)
      : base(session)
    {
    }

    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String AttachmentID
    {
      get
      {
        return _attachmentID;
      }
      set
      {
        SetPropertyValue("AttachmentID", ref _attachmentID, value);
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
    public System.String Location
    {
      get
      {
        return _location;
      }
      set
      {
        SetPropertyValue("Location", ref _location, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(50)]
    public System.String View
    {
      get
      {
        return _view;
      }
      set
      {
        SetPropertyValue("View", ref _view, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(50)]
    public System.String Resolution
    {
      get
      {
        return _resolution;
      }
      set
      {
        SetPropertyValue("Resolution", ref _resolution, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(50)]
    public System.String Theme
    {
      get
      {
        return _theme;
      }
      set
      {
        SetPropertyValue("Theme", ref _theme, value);
      }
    }
    [DevExpress.Xpo.ValueConverterAttribute(typeof(DevExpress.Xpo.Metadata.ImageValueConverter))]
    public System.Drawing.Image AttachmentFile
    {
      get
      {
        return _attachmentFile;
      }
      set
      {
        SetPropertyValue("AttachmentFile", ref _attachmentFile, value);
      }
    }

    private AttachmentType _attachmentType;
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public AttachmentType AttachmentType
    {
        get
        {
            return _attachmentType;
        }
        set
        {
            SetPropertyValue("AttachmentType", ref _attachmentType, value);
        }
    }
    private string _attachmentlink;
    //private string _attachmentlink;
    [RuleRegularExpression("attachmentasalink.RuleRegularExpression", DefaultContexts.Save, @"(((http|https|ftp)\://)?[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}(:[a-zA-Z0-9]*)?/?([a-zA-Z0-9\-\._\?\,\'/\\\+&amp;%\$#\=~])*)|([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6})")]
    [EditorAlias("HyperLinkStringPropertyEditor")]
    public string link
    {
        get
        {
            return _attachmentlink;
        }
        set
        {
            _attachmentlink = value;
        }
    }
    private FileData _file;
       [ExpandObjectMembers(ExpandObjectMembers.Never)]
    public FileData File
    {
        get
        {
            return _file;
        }
        set
        {
            SetPropertyValue("File", ref _file, value);
        }
    }
    [DevExpress.Xpo.AssociationAttribute("EntityAttachments-Attachment")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityAttachment> EntityAttachments
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.EntityAttachment>("EntityAttachments");
      }
    }
    private System.Int32 _DPI;
    public System.Int32 DPI
    {
        get
        {
            return _DPI;
        }
        set
        {
            SetPropertyValue("DPI", ref _DPI, value);
        }
    }



    private System.Int32 _Width;
    public System.Int32 Width
    {
        get
        {
            return _Width;
        }
        set
        {
            SetPropertyValue("Width", ref _Width, value);
        }
    }

    private System.Int32 _Height;
    public System.Int32 Height
    {
        get
        {
            return _Height;
        }
        set
        {
            SetPropertyValue("Height", ref _Height, value);
        }
    }

    private System.String _Machine;
    public System.String Machine
    {
        get
        {
            return _Machine;
        }
        set
        {
            SetPropertyValue("Machine", ref _Machine, value);
        }
    }

    private System.String _Path;
    public System.String Path
    {
        get
        {
            return _Path;
        }
        set
        {
            SetPropertyValue("Path", ref _Path, value);
        }
    }

    private DateTime _LastModified;
    public DateTime LastModified
    {
        get
        {
            return _LastModified;
        }
        set
        {
            SetPropertyValue("LastModified", ref _LastModified, value);
        }
    }

    private Boolean _IsCached;
    public Boolean IsCached
    {
        get
        {
            return _IsCached;
        }
        set
        {
            SetPropertyValue("IsCached", ref _IsCached , value);
        }
    }
  }
  public enum AttachmentType
  {
      File = 0,
      Link = 1
  }
}
