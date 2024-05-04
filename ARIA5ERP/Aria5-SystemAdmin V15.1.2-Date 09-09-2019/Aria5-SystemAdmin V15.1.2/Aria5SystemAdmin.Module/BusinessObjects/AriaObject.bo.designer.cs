using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using System.IO;
using DevExpress.Data.Filtering;
using DevExpress.Xpo.DB;
using System.Xml.Serialization;
using System.Runtime.Serialization;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.ExpressApp.Editors;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
    [Serializable]
    [DefaultClassOptions]
    [RelatedEntity("Aria5-SystemAdmin-AriaObject")]
    [XafDefaultProperty("ObjectName")]
    [Appearance("TestCasesAppearance", TargetItems = "TestCases", AppearanceItemType = "ViewItem", Criteria = "ObjectType.Name != 'Entity'", Context = "DetailView", Visibility = ViewItemVisibility.Hide)]
    public partial class AriaObject : DevExpress.Persistent.BaseImpl.BaseObject
    {
        private Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity _qAProjectEntity;
        private Aria5SystemAdmin.Module.BusinessObjects.QADefect _qADefect;
        //private System.Int32 _objectID;
        private System.Double _objectID;

        private System.Boolean _propertyLevel;
        private Aria5SystemAdmin.Module.BusinessObjects.AriaObject _ariaObject;
        private System.String _activeRevision;
        private Aria5SystemAdmin.Module.BusinessObjects.ObjectType _parentObjectType;
        private Aria5SystemAdmin.Module.BusinessObjects.AriaObject _parentObjectID;
        private Aria5SystemAdmin.Module.BusinessObjects.ObjectType _objectType;
        private System.String _objectDescription;
        private System.String _objectName;
        //private XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObject> xx;
        public AriaObject(DevExpress.Xpo.Session session)
          : base(session)
        {
            //function();
        }
        public AriaObject()
        {
        }
        [DevExpress.Xpo.SizeAttribute(50)]
        [RuleRequiredField(DefaultContexts.Save)]
        [RuleUniqueValue("", DefaultContexts.Save, CriteriaEvaluationBehavior = DevExpress.Persistent.Validation.CriteriaEvaluationBehavior.InTransaction)]
        [Calculated("concat('D', ToStr(SequentialNumber))")]
        public System.Double ObjectID
        {
            get
            {
                return _objectID;
            }
            set
            {
                SetPropertyValue("ObjectID", ref _objectID, value);
            }
        }
        //public System.Int32 ObjectID
        //{
        //  get
        //  {
        //    return _objectID;
        //  }
        //  set
        //  {
        //    SetPropertyValue("ObjectID", ref _objectID, value);
        //  }
        //}
        [DevExpress.Xpo.NonPersistentAttribute]
        public Aria5SystemAdmin.Module.BusinessObjects.ObjectType ParentObjectType
        {
            get
            {
                //return _parentObjectType;
                try
                {
                    return ParentObjectID.ObjectType;
                }
                catch (Exception ex)
                {
                    return _parentObjectType;
                }
            }
            set
            {
                SetPropertyValue("ParentObjectType", ref _parentObjectType, value);
            }
        }
        [XmlIgnore]
        [ImmediatePostData]
        public Aria5SystemAdmin.Module.BusinessObjects.AriaObject ParentObjectID
        {
            get
            {
                return _parentObjectID;
            }
            set
            {
                SetPropertyValue("ParentObjectID", ref _parentObjectID, value);
                if (value != null)
                {
                    ParentObjectType = value.ObjectType;
                    SetPropertyValue("ParentObjectType", ref _parentObjectType, value.ObjectType);
                }
              ;
            }
        }
        [RuleRequiredField(DefaultContexts.Save)]
        [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
        public System.String ObjectName
        {
            get
            {
                return _objectName;
            }
            set
            {
                //if (IsLoading == false && ((String.IsNullOrEmpty(ObjectName) == true) || (String.IsNullOrEmpty(ObjectName) == false && ObjectName.ToLower().TrimEnd() != value.ToLower().TrimEnd())))
                //{
                //    value = System.Threading.Thread.CurrentThread.CurrentCulture.TextInfo.ToTitleCase(value.ToLower());
                //}
                //if (string.IsNullOrEmpty(ObjectName) == true)
                //{
                //    ObjectDescription = value;
                //}
                //;
                //value = value.Replace(" ", "");
                SetPropertyValue("ObjectName", ref _objectName, value);
            }
        }
        [DevExpress.Xpo.SizeAttribute(250)]
        public System.String ObjectDescription
        {
            get
            {
                return _objectDescription;
            }
            set
            {
                SetPropertyValue("ObjectDescription", ref _objectDescription, value);
            }
        }
        [DevExpress.Xpo.AssociationAttribute("AriaObjects-ObjectType")]
        [RuleRequiredField(DefaultContexts.Save)]
        public Aria5SystemAdmin.Module.BusinessObjects.ObjectType ObjectType
        {
            get
            {
                return _objectType;
            }
            set
            {
                SetPropertyValue("ObjectType", ref _objectType, value);
            }
        }
        public System.String ActiveRevision
        {
            get
            {
                return _activeRevision;
            }
            set
            {
                SetPropertyValue("ActiveRevision", ref _activeRevision, value);
            }
        }
        [XmlIgnore]
        public XPCollection<AriaObject> GetChildren
        {
            get
            {
                return new XPCollection<AriaObject>(Session, DevExpress.Data.Filtering.CriteriaOperator.Parse("ParentObjectID='" + Oid + "'"));
                //XPCollection<AriaObject> yy;
                //if (PropertyLevel == false)
                //{
                //  retun
                //}
                //else
                //{
                //  XPCollection<AriaObjectProperty> newAriaObjectProperties = new XPCollection<AriaObjectProperty>(Session, DevExpress.Data.Filtering.CriteriaOperator.Parse("AriaParentObject='" + Oid + "'"));
                //  yy = new XPCollection<AriaObject>(Session, DevExpress.Data.Filtering.CriteriaOperator.Parse("1=2"));
                //  for (int i = 0; i < newAriaObjectProperties.Count; i++)
                //  {
                //    yy.Add(newAriaObjectProperties[i].AriaObject);
                //  }
                //  ;
                //}
                //;
                //return yy;
            }
        }
        [XmlIgnore]
        [DevExpress.Xpo.AssociationAttribute("AriaObjectRevisions-AriaObject")]
        public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectRevision> AriaObjectRevisions
        {
            get
            {
                return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectRevision>("AriaObjectRevisions");
            }
        }
        public override void AfterConstruction()
        {
            base.AfterConstruction();
            BaseObject.OidInitializationMode = DevExpress.Persistent.BaseImpl.OidInitializationMode.AfterConstruction;
            ActiveRevision = "000001";
            Aria5SystemAdmin.Module.Aria5Globals aria5Globals = new Aria5Globals();
            ObjectID = aria5Globals.GetSequence("AriaObject", "ObjectID", Session, "");
        }
        protected override void OnSaving()
        {
            //MMT
            this.ConversionDate = System.DateTime.Now.Date;
            //MMT
            if (Session.IsNewObject(this))
                AddEmptyRevision();
        }
        public void AddEmptyRevision()
        {
            //   var r =  AriaObjectRevisions.where(r => String.IsNullOrWhiteSpace(r.ObjectRevision));
            //if(AriaObjectRevisions.where(r => String.IsNullOrWhiteSpace(r.ObjectRevision)) 
            AriaObjectRevision LastAriaObjectRevision = new AriaObjectRevision(Session);
            AriaObjectRevision newAriaObjectRevision = new AriaObjectRevision(Session);
            string strNextRevision = "";
            newAriaObjectRevision.AriaObject = this;
            newAriaObjectRevision.ObjectRevision = strNextRevision;
            AriaObjectRevisions.Add(newAriaObjectRevision);
            ActiveRevision = strNextRevision;
            if (Session.IsNewObject(this) == true && ObjectType != null)
            {
                // Case Aria Field Object And Other object types
                for (int i = 0; i < ObjectType.SettingTypes.Count; i++)
                {
                    AriaObjectSetting newAriaObjectSettings = new AriaObjectSetting(Session);
                    newAriaObjectSettings.AriaObjectRevision = newAriaObjectRevision;
                    newAriaObjectSettings.Value = "";
                    newAriaObjectSettings.DataType = ObjectType.SettingTypes[i].DataType.ToString().ToUpper();
                    newAriaObjectSettings.Width = ObjectType.SettingTypes[i].Width;
                    newAriaObjectSettings.DecimalPlaces = ObjectType.SettingTypes[i].Decimal;
                    newAriaObjectSettings.SettingType = ObjectType.SettingTypes[i];
                    //ATA add new field that created in setting type Actual column name to the setting 7/13/2017 [start]
                    newAriaObjectSettings.ActualColumnName = ObjectType.SettingTypes[i].ActualColumnName;
                    //ATA add new field that created in setting type Actual column name to the setting 7/13/2017 [End]

                    newAriaObjectRevision.AriaObjectSettings.Add(newAriaObjectSettings);

                }
            }
        }
        [XmlIgnore]
        public XPCollection<AriaObjectSetting> AriaObjectSettings
        {
            get
            {
                XPCollection<AriaObjectSetting> nn = new XPCollection<AriaObjectSetting>(Session);
                try
                {
                    //nn = Session.FindObject<AriaObjectRevision>(DevExpress.Data.Filtering.CriteriaOperator.Parse("ObjectRevision='"+ActiveRevision.ToString()  +"'")).AriaObjectSettingses;
                    for (int i = 0; i < AriaObjectRevisions.Count; i++)
                    {
                        //if (AriaObjectRevisions[i].ObjectRevision != null)
                        //{
                        if (AriaObjectRevisions[i].ObjectRevision == ActiveRevision)
                        {
                            nn = AriaObjectRevisions[i].AriaObjectSettings;
                            break;
                        }
                        //}
                    }
                }
                catch (Exception ex)
                {
                }
                return nn;
            }
        }
        [XmlIgnore]
        [DevExpress.Xpo.AssociationAttribute("AriaObjectEvents-AriaObject")]
        public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEvent> AriaObjectEvents
        {
            get
            {
                return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEvent>("AriaObjectEvents");
            }
        }
        [XmlIgnore]
        [DevExpress.Xpo.AssociationAttribute("AriaObjectMethods-AriaObject")]
        public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethod> AriaObjectMethods
        {
            get
            {
                return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethod>("AriaObjectMethods");
            }
        }
        [XmlIgnore]
        [NonPersistent]
        [DevExpress.Persistent.Base.VisibleInDetailViewAttribute(false)]
        [DevExpress.Persistent.Base.VisibleInListViewAttribute(false)]
        [DevExpress.Xpo.AssociationAttribute("AriaObjects-Applications")]
        public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.Application_T> Applications
        {
            get
            {
                return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Application_T>("Applications");
            }
        }
        private Application_T _app;
        // [DevExpress.Xpo.AssociationAttribute("AriaObjects-Applications")]
        [XmlIgnore]
        public Application_T Application
        {
            get { return _app; }
            set { _app = value; }
        }
        [XmlIgnore]
        [DevExpress.Xpo.AssociationAttribute("AriaObjectProperties-AriaObject")]
        public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectProperty> AriaObjectProperties
        {
            get
            {
                return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AriaObjectProperty>("AriaObjectProperties");
            }
        }
        //ATA add this association to relate this tracking entry with the aria object 2/22/2017 [6368][Start]
        [XmlIgnore]
        [DevExpress.Xpo.AssociationAttribute("AriaObject-TrackingEntries")]

        public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry> TrackingEntries
        {
            get
            {
                return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry>("TrackingEntries");
            }
        }
        //ATA add this association to relate this tracking entry with the aria object 2/22/2017 [6368][End]
        //ATA add new relation between Aria object and test cases as required from quality team 11/19/2017 [Start] 

        [XmlIgnore]
        [Association("AriaObjectEntity-TeastCases")]
        [DataSourceCriteria("IsOriginal == True")]
        public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TestCase> TestCases
        {
            get
            {
                return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TestCase>("TestCases");
            }
        }
        //ATA add new relation between Aria object and test cases as required from quality team 11/19/2017 [End] 

        private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectProperty[] _ariaObjectPropertysArray;
        public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectProperty[] AriaObjectPropertysArray
        {
            get
            {
                return _ariaObjectPropertysArray;
            }
            set
            {
                _ariaObjectPropertysArray = value;
            }
        }
        //public Aria5SystemAdmin.Module.BusinessObjects.Application_T[] ApplicationsArray
        //{
        //    get
        //    {
        //        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.Application_T>("Applications").ToArray();
        //    }
        //}
        private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethod[] _ariaObjectMethodsArray;
        public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectMethod[] AriaObjectMethodsArray
        {
            get
            {
                return _ariaObjectMethodsArray;
            }
            set
            {
                _ariaObjectMethodsArray = value;
            }
        }
        private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEvent[] _ariaObjectEventsArray;
        public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectEvent[] AriaObjectEventsArray
        {
            get
            {
                return _ariaObjectEventsArray;
            }
            set
            {
                _ariaObjectEventsArray = value;
            }
        }
        private Aria5SystemAdmin.Module.BusinessObjects.AriaObjectSetting[] _ariaObjectSettingsArray;
        public Aria5SystemAdmin.Module.BusinessObjects.AriaObjectSetting[] AriaObjectSettingsArray
        {
            get
            {
                return _ariaObjectSettingsArray;
            }
            set
            {
                _ariaObjectSettingsArray = value;
            }
        }
        private Aria5SystemAdmin.Module.BusinessObjects.AriaObject[] _getChildrenArray;
        public Aria5SystemAdmin.Module.BusinessObjects.AriaObject[] GetChildrenArray
        {
            get
            {
                return _getChildrenArray;
            }
            set
            {
                _getChildrenArray = value;
            }
        }
        [XmlIgnore]
        public Aria5SystemAdmin.Module.BusinessObjects.QADefect QADefect
        {
            get
            {
                return _qADefect;
            }
            set
            {
                if (_qADefect == value)
                    return;
                Aria5SystemAdmin.Module.BusinessObjects.QADefect prevQADefect = _qADefect;
                _qADefect = value;
                if (IsLoading)
                    return;
                if (prevQADefect != null && prevQADefect.Entity == this)
                    prevQADefect.Entity = null;
                if (_qADefect != null)
                    _qADefect.Entity = this;
                OnChanged("QADefect");
            }
        }
        [XmlIgnore]
        public Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity QAProjectEntity
        {
            get
            {
                return _qAProjectEntity;
            }
            set
            {
                if (_qAProjectEntity == value)
                    return;
                Aria5SystemAdmin.Module.BusinessObjects.QAProjectEntity prevQAProjectEntity = _qAProjectEntity;
                _qAProjectEntity = value;
                if (IsLoading)
                    return;
                if (prevQAProjectEntity != null && prevQAProjectEntity.AriaObject == this)
                    prevQAProjectEntity.AriaObject = null;
                if (_qAProjectEntity != null)
                    _qAProjectEntity.AriaObject = this;
                OnChanged("QAProjectEntity");
            }
        }

        private System.DateTime _ConversionDate;

        public System.DateTime ConversionDate
        {
            get
            {
                return _ConversionDate;
            }
            set
            {
                SetPropertyValue("ConversionDate", ref _ConversionDate, value);
            }
        }
    }
}
