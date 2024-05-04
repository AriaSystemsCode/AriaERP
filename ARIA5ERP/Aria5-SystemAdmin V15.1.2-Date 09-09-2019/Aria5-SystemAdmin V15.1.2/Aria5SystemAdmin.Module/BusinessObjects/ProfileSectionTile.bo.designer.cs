using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  public partial class ProfileSectionTile : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.ProfileSection _section;
    private Aria5SystemAdmin.Module.BusinessObjects.Entity _entity;
    private Aria5SystemAdmin.Module.BusinessObjects.EntityCategory _entityCategory;
    private System.String _filter;
    private System.String _tileID;
    public ProfileSectionTile(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    [DevExpress.Xpo.SizeAttribute(30)]
    public System.String TileID
    {
      get
      {
        return _tileID;
      }
      set
      {
        SetPropertyValue("TileID", ref _tileID, value);
      }
    }

    [DataSourceProperty("Categories")]
    [ImmediatePostData]
    public Aria5SystemAdmin.Module.BusinessObjects.EntityCategory EntityCategory
    {
      get
      {
        return _entityCategory;
      }
      set
      {
        SetPropertyValue("EntityCategory", ref _entityCategory, value);
      }
    }

    [DataSourceProperty("Entities")]
    public Aria5SystemAdmin.Module.BusinessObjects.Entity Entity
    {
      get
      {
        return _entity;
      }
      set
      {
        SetPropertyValue("Entity", ref _entity, value);
      }
    }
    public System.String Filter
    {
      get
      {
        return _filter;
      }
      set
      {
        SetPropertyValue("Filter", ref _filter, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ProfileSectionTiles-ProfileSection")]
    public Aria5SystemAdmin.Module.BusinessObjects.ProfileSection Section
    {
      get
      {
        return _section;
      }
      set
      {
        SetPropertyValue("Section", ref _section, value);
        if (this.Section != null && this.Oid.ToString().IndexOf("000")>-1 )
        {
            this.EntityCategory = this.Section.EntityCategory;
        }

      }
        
    }

   
    public XPCollection<Entity> Entities
    {
        get {
            XPCollection<Entity> _entities = new XPCollection<Entity>(Session);
            try
            {
                _entities = this.EntityCategory.Entities;
            }
            catch (Exception ex)
            { }
 
            return _entities;}

    }

    public XPCollection<EntityCategory> Categories
    {
        get
        {

            XPCollection<EntityCategory> _categories = new XPCollection<EntityCategory>(Session) ;

            if (this.Section != null)
            {
                if (this.Section.EntityType != null)
                {
                    _categories = this.Section.EntityType.EntityCategories;
                }
            }
            return _categories;
        }

    }
    private System.String _LinkedTo;
    public System.String LinkedTo
    {
        get
        {
            return _LinkedTo;
        }
        set
        {
            SetPropertyValue("LinkedTo", ref _LinkedTo, value);
        }
    }
    private EntityType _EntityType;
    public EntityType EntityType
    {
        get
        {
            return _EntityType;
        }
        set
        {
            SetPropertyValue("EntityType", ref _EntityType, value);
        }
    }

    //private DataSort _DefaultDataSort;
    //public DataSort DefaultDataSort
    //{
    //    get
    //    {
    //        return _DefaultDataSort;
    //    }
    //    set
    //    {
    //        SetPropertyValue("DefaultDataSort", ref _DefaultDataSort , value);
    //    }
    //}


    private System.Guid _Gadget;
    public System.Guid Gadget
    {
        get
        {
            return _Gadget;
        }
        set
        {
            SetPropertyValue("Gadget", ref _Gadget, value);
        }
    }

    private System.Int16 _Row;
    public System.Int16 Row
    {
        get
        {
            return _Row;
        }
        set
        {
            SetPropertyValue("Row", ref _Row, value);
        }
    }

    private System.Int16 _Column;
    public System.Int16 Column
    {
        get
        {
            return _Column;
        }
        set
        {
            SetPropertyValue("Column", ref _Column, value);
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
    private System.String _TileTemplateId;
    public System.String TileTemplateId
    {
        get
        {
            return _TileTemplateId;
        }
        set
        {
            SetPropertyValue("TileTemplateId", ref _TileTemplateId, value);
        }
    }

    private System.String _TileTemplateSizeId;
    public System.String TileTemplateSizeId
    {
        get
        {
            return _TileTemplateSizeId;
        }
        set
        {
            SetPropertyValue("TileTemplateSizeId", ref _TileTemplateSizeId, value);
        }
    }

    private System.String _SectionPageId;
    public System.String SectionPageId
    {
        get
        {
            return _SectionPageId;
        }
        set
        {
            SetPropertyValue("SectionPageId", ref _SectionPageId, value);
        }
    }


    private System.String _DetailPageId;
    public System.String DetailPageId
    {
        get
        {
            return _DetailPageId;
        }
        set
        {
            SetPropertyValue("DetailPageId", ref _DetailPageId, value);
        }
    }

    private System.String _Tilestretch;
    public System.String Tilestretch
    {
        get
        {
            return _Tilestretch;
        }
        set
        {
            SetPropertyValue("Tilestretch", ref _Tilestretch, value);
        }
    }
    public override void AfterConstruction()
    {
        
        base.AfterConstruction();
        

    }

      
      
  }
}
