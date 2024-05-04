using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using System.IO;
using DevExpress.Data.Filtering;
using DevExpress.Xpo.DB;
using DevExpress.ExpressApp.Web;
using DevExpress.ExpressApp.Editors;
using DevExpress.ExpressApp.Web.Editors.ASPx;
using DevExpress.Web;
using System.Xml.Serialization;
namespace Aria5SystemAdmin.Module.BusinessObjects
{

    [Serializable]
  [DefaultClassOptions]
    //ATA to improve the display of revision object 
  [XafDefaultProperty("ObjectRevision")]
    //ATA 
  public partial class AriaObjectRevision : DevExpress.Persistent.BaseImpl.BaseObject
  {
    private Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry _trackingNo;
    private System.String _longDescription;
    private System.String _shortDescription;
    private System.String _autoTaskTicket;
    private System.String _releaseNo;
    private System.String _servicePack;
    private System.String _buildNo;
    private System.String _objectRevisionSettings;
    private System.String _objectRevision;
    private Aria5SystemAdmin.Module.BusinessObjects.AriaObject _ariaObject;
    public AriaObjectRevision(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    public AriaObjectRevision()
    {
    }
       
    [DevExpress.Xpo.AssociationAttribute("AriaObjectRevisions-AriaObject")]
    public Aria5SystemAdmin.Module.BusinessObjects.AriaObject AriaObject
    {
      get
      {
        return _ariaObject;
      }
      set
      {
        SetPropertyValue("AriaObject", ref _ariaObject, value);
      }
    }
    public System.String ObjectRevision
    {
      get
      {
        return _objectRevision;
      }
      set
      {
        SetPropertyValue("ObjectRevision", ref _objectRevision, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(-1)]
    public System.String ObjectRevisionSettings
    {
      get
      {
        return _objectRevisionSettings;
      }
      set
      {
        SetPropertyValue("ObjectRevisionSettings", ref _objectRevisionSettings, value);
      }
    }
         [XmlIgnore]
    [DevExpress.Xpo.AssociationAttribute("AriaObjectSettingses-AriaObjectRevision")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectSetting> AriaObjectSettings
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectSetting>("AriaObjectSettings");
      }
    }
         [XmlIgnore]
    public XPCollection<AriaObjectEvent> AriaObjectEvents
    {
      get
      {
        XPCollection<AriaObjectEvent> newAriaObjectEvents = new XPCollection<AriaObjectEvent>(Session);
        if (ObjectRevision == null)
        {
            return newAriaObjectEvents;
        }
        else
        {

            // newAriaObjectEvents.Criteria = DevExpress.Data.Filtering.ContainsOperator.Parse("AriaObject='" + AriaObject.Oid.ToString() + "'");
            //newAriaObjectEvents.Load();
            string ss2 = "SELECT Oid FROM AriaObjectEvent WHERE (AriaObject = '" + AriaObject.Oid.ToString() + "') AND (GCRecord IS NULL) AND ((EventName + ObjectRevision) IN " + " (SELECT MAX(EventName + ObjectRevision) AS Expr1 FROM AriaObjectEvent AS AriaObjectEvent_1 " + "  WHERE  (AriaObject = '" + AriaObject.Oid.ToString() + "') AND (ObjectRevision <= '" + ObjectRevision.ToString() + "') AND (GCRecord IS NULL) GROUP BY EventName))";
            for (int i = (newAriaObjectEvents.Count - 1); i > -1; i--)
            {
                newAriaObjectEvents.Remove(newAriaObjectEvents[i]);
            }
            SelectedData CanselectedData = Session.ExecuteQuery(ss2);
            for (int i = 0; i < CanselectedData.ResultSet[0].Rows.Length; i++)
            {
                try
                {
                    if (Session.FindObject<AriaObjectEvent>(DevExpress.Data.Filtering.ContainsOperator.Parse("Oid='" + CanselectedData.ResultSet[0].Rows[i].Values[0].ToString() + "'")).ModificationType != AriaObjectEvent.modificationType.Delete)
                    {
                        newAriaObjectEvents.Add(Session.FindObject<AriaObjectEvent>(DevExpress.Data.Filtering.ContainsOperator.Parse("Oid='" + CanselectedData.ResultSet[0].Rows[i].Values[0].ToString() + "'")));
                    }
                }
                catch (Exception ex)
                {
                }
                ;
            }
            return newAriaObjectEvents;
        }
      }
    }
         [XmlIgnore]
    public XPCollection<AriaObjectMethod> AriaObjectMethods
    {
      get
      {
        XPCollection<AriaObjectMethod> newAriaObjectMethods = new XPCollection<AriaObjectMethod>(Session);
        //newAriaObjectMethods.Criteria = DevExpress.Data.Filtering.ContainsOperator.Parse("AriaObject='" + AriaObject.Oid.ToString() + "'");
        //newAriaObjectMethods.Load();
        if (ObjectRevision == null)
        {
            return newAriaObjectMethods;
        }
        else
        {

            string ss2 = "SELECT Oid FROM AriaObjectMethod WHERE (AriaObject = '" + AriaObject.Oid.ToString() + "') AND (GCRecord IS NULL) AND ((MethodName + ObjectRevision) IN " + " (SELECT MAX(MethodName + ObjectRevision) AS Expr1 FROM AriaObjectMethod AS AriaObjectMethod_1 " + "  WHERE  (AriaObject = '" + AriaObject.Oid.ToString() + "') AND (ObjectRevision <= '" + ObjectRevision.ToString() + "') AND (GCRecord IS NULL) GROUP BY MethodName))";
            for (int i = (newAriaObjectMethods.Count - 1); i > -1; i--)
            {
                newAriaObjectMethods.Remove(newAriaObjectMethods[i]);
            }
            SelectedData CanselectedData = Session.ExecuteQuery(ss2);
            for (int i = 0; i < CanselectedData.ResultSet[0].Rows.Length; i++)
            {
                try
                {
                    if (Session.FindObject<AriaObjectMethod>(DevExpress.Data.Filtering.ContainsOperator.Parse("Oid='" + CanselectedData.ResultSet[0].Rows[i].Values[0].ToString() + "'")).ModificationType != AriaObjectMethod.modificationType.Delete)
                    {
                        newAriaObjectMethods.Add(Session.FindObject<AriaObjectMethod>(DevExpress.Data.Filtering.ContainsOperator.Parse("Oid='" + CanselectedData.ResultSet[0].Rows[i].Values[0].ToString() + "'")));
                    }
                }
                catch (Exception ex)
                {
                }
                ;
            }
            return newAriaObjectMethods;
        }
      }
    }
         [XmlIgnore]
    public XPCollection<AriaObjectProperty> AriaObjectProperties
    {
      get
      {
        XPCollection<AriaObjectProperty> newAriaObjectProperties = new XPCollection<AriaObjectProperty>(Session);

        if (ObjectRevision == null)
        {
            return newAriaObjectProperties;
        }
        else
        {

            //string ss2 = "SELECT Oid, AriaObject, ObjectRevision, PropertyName, PropertySettings, ModificationType, PropertyDescription, Required, GenSquFn, PropertyType FROM AriaObjectProperties WHERE (AriaObject = '" + AriaObject.Oid.ToString() + "') AND (GCRecord IS NULL) AND ((PropertyName + ObjectRevision) IN " + " (SELECT MAX(PropertyName + ObjectRevision) AS Expr1 FROM AriaObjectProperties AS AriaObjectProperties_1 " + "  WHERE  (AriaObject = '" + AriaObject.Oid.ToString() + "') AND (ObjectRevision <= '" + ObjectRevision.ToString() + "') AND (GCRecord IS NULL) GROUP BY PropertyName))";
            string ss2 = "SELECT Oid FROM AriaObjectProperty WHERE (AriaObject = '" + AriaObject.Oid.ToString() + "') AND (GCRecord IS NULL) AND ((PropertyName + ObjectRevision) IN " + " (SELECT MAX(PropertyName + ObjectRevision) AS Expr1 FROM AriaObjectProperty AS AriaObjectProperties_1 " + "  WHERE  (AriaObject = '" + AriaObject.Oid.ToString() + "') AND (ObjectRevision <= '" + ObjectRevision.ToString() + "') AND (GCRecord IS NULL) GROUP BY PropertyName))";
            if (newAriaObjectProperties != null)
            {
                for (int i = (newAriaObjectProperties.Count - 1); i > -1; i--)
                {
                    newAriaObjectProperties.Remove(newAriaObjectProperties[i]);
                }
            }

            SelectedData CanselectedData = Session.ExecuteQuery(ss2);
            for (int i = 0; i < CanselectedData.ResultSet[0].Rows.Length; i++)
            {
                try
                {
                    if (Session.FindObject<AriaObjectProperty>(DevExpress.Data.Filtering.ContainsOperator.Parse("Oid='" + CanselectedData.ResultSet[0].Rows[i].Values[0].ToString() + "'")).ModificationType != AriaObjectProperty.modificationType.Delete)
                    {
                        newAriaObjectProperties.Add(Session.FindObject<AriaObjectProperty>(DevExpress.Data.Filtering.ContainsOperator.Parse("Oid='" + CanselectedData.ResultSet[0].Rows[i].Values[0].ToString() + "'")));
                    }
                }
                catch (Exception ex)
                {
                }
                ;
            }
            return newAriaObjectProperties;
        }
      }
    }
    public System.String BuildNo
    {
      get
      {
        return _buildNo;
      }
      set
      {
        SetPropertyValue("BuildNo", ref _buildNo, value);
      }
    }
    public Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry TrackingNo
    {
      get
      {
        return _trackingNo;
      }
      set
      {
        SetPropertyValue("TrackingNo", ref _trackingNo, value);
      }
    }
    public System.String ServicePack
    {
      get
      {
        return _servicePack;
      }
      set
      {
        SetPropertyValue("ServicePack", ref _servicePack, value);
      }
    }
    public System.String ReleaseNo
    {
      get
      {
        return _releaseNo;
      }
      set
      {
        SetPropertyValue("ReleaseNo", ref _releaseNo, value);
      }
    }
    public System.String AutoTaskTicket
    {
      get
      {
        return _autoTaskTicket;
      }
      set
      {
        SetPropertyValue("AutoTaskTicket", ref _autoTaskTicket, value);
      }
    }
    public System.String ShortDescription
    {
      get
      {
        return _shortDescription;
      }
      set
      {
        SetPropertyValue("ShortDescription", ref _shortDescription, value);
      }
    }
    [DevExpress.Xpo.SizeAttribute(-1)]
    public System.String LongDescription
    {
      get
      {
        return _longDescription;
      }
      set
      {
        SetPropertyValue("LongDescription", ref _longDescription, value);
      }
    }

    private Boolean _nowLocked;
    [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
    [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
    public System.Boolean NowLocked
    {
        get
        {
            return _nowLocked;
        }
        set
        {
            SetPropertyValue("NowLocked", ref _nowLocked, value);
        }
    }
  }
}
