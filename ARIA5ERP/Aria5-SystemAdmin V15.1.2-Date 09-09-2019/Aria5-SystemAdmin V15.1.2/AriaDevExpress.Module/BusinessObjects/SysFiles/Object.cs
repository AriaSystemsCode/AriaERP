using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using DevExpress.Persistent.AuditTrail;
using System.ComponentModel;
using DevExpress.ExpressApp.ConditionalAppearance;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [Disable_Database_For_A27()]
    [FriendlyKeyProperty("cApObjNam")]
    [Enable_Client_For_Custom_LevelOfUpgrade_Only()]
    [Persistent("SYDOBJCT")]
    public class Object : XPLiteObject, IAuditable
    {
        public Object(Session session)
            : base(session)
        {
        }


        [Key(true)]
        [Persistent("Oid")]
        private Guid Oid
        {
            get
            {
                //return PrimaryKey.ID;
                return GetPropertyValue<Guid>("Oid");
            }
            set
            {
                // _primaryKey.ID = value;
                SetPropertyValue("Oid", value);
            }
        }

        //[Key(AutoGenerate = false)]
        [RuleRequiredField()]
        [Persistent("cApObjNam"), Size(10)]
        //[RuleUniqueValue("UniqueObject", DefaultContexts.Save)]
        private string cApObjNam
        {
            get
            {
                //return PrimaryKey.ID;
                return GetPropertyValue<string>("cApObjNam");
            }
            set
            {
                // _primaryKey.ID = value;
                SetPropertyValue("cApObjNam", value);
            }
        }

        [NonPersistent()]
        public string ID
        {
            get { return cApObjNam; }
            set { cApObjNam = value; }
        }

        [Persistent("cApobjsds"), Size(40)]
        [VisibleInListView(true)]
        public string ShortName { get; set; }


        [Persistent("cprglname"), Size(40)]
        [VisibleInListView(false)]
        public string LongName { get; set; }

        [Persistent("capobjtyp"), Size(1)]
        [VisibleInListView(false)]
        public string Type { get; set; }


        [Persistent("cApp_Id"), NoForeignKey()]
        [RuleRequiredField()]
        [VisibleInListView(true)]
        [Custom("LookupEditorMode", "AllItems")]
        public Module Module { get; set; }

        [Persistent("cBaseFile"), NoForeignKey()]
        //  [Custom("LookupEditorMode", "AllItems")]
        [VisibleInListView(false)]
        //Mostafa
        public string BaseFile { get; set; }


        [Persistent("mprgfiles"), Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string ProgramFiles { get; set; }

        [Persistent("cBaseWind"), Size(10)]
        [VisibleInListView(false)]
        public string BaseWindow { get; set; }


        [Persistent("lSingUsr")]
        [VisibleInListView(false)]
        public bool SingleUser { get; set; }


        [Persistent("mRelProgs"), Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string RelatedPrograms { get; set; }


        [Persistent("lSubproc")]
        [VisibleInListView(false)]
        public bool HasSubProcesses { get; set; }


        [Persistent("MSubproc"), Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string SubProcess { get; set; }


        [Persistent("cupgrdlvl"), Size(1)]
        [ImmediatePostData(true), VisibleInListView(true)]
        [ValueConverter(typeof(EnumValueConverter<UpgradeLevelType>))]
        public UpgradeLevelType LevelOfUpgrade
        {
            get { return GetPropertyValue<UpgradeLevelType>("LevelOfUpgrade"); }
            set
            {
                SetPropertyValue("LevelOfUpgrade", value);

                if (!IsLoading && ((UpgradeLevelType)value) != UpgradeLevelType.U)
                {
                    Client = Session.GetObjectByKey<Sys_Client>("ARIA");
                }
            }
        }

        [Association("Client-Object")]
        [VisibleInListView(true)]
        [Custom("LookupEditorMode", "AllItems")]
        public Sys_Client Client
        {
            get
            {
                return GetPropertyValue<Sys_Client>("Client");
            }
            set
            {
                SetPropertyValue("Client", value);
            }
        }

        [ImmediatePostData(true)]
        [VisibleInListView(true), Size(10)]
        [ValueConverter(typeof(EnumValueConverter<Products>))]
        public Products Product
        {
            get
            {
                return GetPropertyValue<Products>("Product");
            }
            set
            {
                SetPropertyValue("Product", value);

                if (!IsLoading && value == Products.ARIA27)
                    Database = DataBaseTypes.FOX;
            }
        }

        [VisibleInListView(true), Size(10)]
        [ValueConverter(typeof(EnumValueConverter<DataBaseTypes>))]
        public DataBaseTypes Database
        {
            get
            {
                return GetPropertyValue<DataBaseTypes>("Database");
            }
            set
            {
                SetPropertyValue("Database", value);
            }
        }


        [Persistent("crep_id")]
        //[Custom("LookupEditorMode", "AllItems")]
        [VisibleInListView(false), Size(8)]
        public string ReportID { get; set; }


        [Persistent("mSkipExpr"), Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string SkipForExpression { get; set; }

        [Persistent("mMarkExpr"), Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string MarkForExpression { get; set; }


        [Persistent("A5OBJNAM"), Size(100)]
        [VisibleInListView(false)]
        public string Aria5ObjectId { get; set; }


        [Persistent("mrepfxflt"), Size(SizeAttribute.Unlimited)]
        [Browsable(false)]
        public string FixedFilters { get; set; }

        [Persistent("MrepVrFlt"), Size(SizeAttribute.Unlimited)]
        [Browsable(false)]
        public string VariableFilters { get; set; }

        [Persistent("MRepFltr"), Size(SizeAttribute.Unlimited)]
        [Browsable(false)]
        public string MRepFltr { get; set; }

        [Size(SizeAttribute.Unlimited)]
        public string mapobjdes { get; set; }

        [Size(SizeAttribute.Unlimited)]
        public string mprgnames { get; set; }

        public bool lrunalone { get; set; }

        public bool lmultinst { get; set; }

        [Size(SizeAttribute.Unlimited)]
        public string mcallmods { get; set; }

        [Size(8)]
        public string chelpfile { get; set; }

        public int nhelpcntxn { get; set; }

        [NoForeignKey()]
        [Association("Object-ObjectFilters"), Aggregated()]
        public XPCollection<ObjectFilters> Filters
        {
            get
            {
                return GetCollection<ObjectFilters>("Filters");
            }
        }


        #region Audit Trail

        protected override void OnSaved()
        {
            DevExpress.Persistent.AuditTrail.AuditTrailService.Instance.AddCustomAuditData(Session, new DevExpress.Persistent.AuditTrail.AuditDataItem(this, null, "Ticket", Ticket, DevExpress.Persistent.AuditTrail.AuditOperationType.CustomData));
            DevExpress.Persistent.AuditTrail.AuditTrailService.Instance.AddCustomAuditData(Session, new DevExpress.Persistent.AuditTrail.AuditDataItem(this, null, "Comments", Comments, DevExpress.Persistent.AuditTrail.AuditOperationType.CustomData));
            DevExpress.Persistent.AuditTrail.AuditTrailService.Instance.AddCustomAuditData(Session, new DevExpress.Persistent.AuditTrail.AuditDataItem(this, null, "TrackingEntry", TrackingEntry, DevExpress.Persistent.AuditTrail.AuditOperationType.CustomData));
            base.OnSaved();
        }

        [NonPersistent()]
        [RuleRequiredField(CustomMessageTemplate = AriaAudit.TicketRequiredMessage), Browsable(false)]
        public string Ticket
        {
            get { return GetPropertyValue<string>("Ticket"); }
            set { SetPropertyValue<string>("Ticket", value); }
        }


        [NonPersistent()]
        private string TrackingEntry
        {
            get { return GetPropertyValue<string>("TrackingEntry"); }
            set { SetPropertyValue<string>("TrackingEntry", value); }
        }

        [NonPersistent()]
        private string Comments
        {
            get { return GetPropertyValue<string>("Comments"); }
            set { SetPropertyValue<string>("Comments", value); }
        }

        private XPCollection<AriaAuditDataPersistent> auditTrail;
        public XPCollection<AriaAuditDataPersistent> AuditTrail
        {
            get
            {
                if (auditTrail == null)
                {
                    XPCollection<AuditDataItemPersistent> _auditTrail = AuditedObjectWeakReference.GetAuditTrail(Session, this);
                    if (_auditTrail != null)
                    {
                        auditTrail = new XPCollection<AriaAuditDataPersistent>(_auditTrail, CriteriaOperator.Parse("!(OldValue == 'Ticket' || OldValue == 'Comments' || OldValue == 'TrackingEntry' || PropertyName == 'Ticket' || PropertyName == 'Comments' || PropertyName == 'TrackingEntry')", null));
                        //auditTrail.DisplayableProperties = "This;UserName;ModifiedOn;OperationType;PropertyName;Ticket";
                        // auditTrail.DisplayableProperties = "Ticket;Comments";
                    }

                }
                return auditTrail;
            }
        }

        #endregion

        public override void AfterConstruction()
        {
            base.AfterConstruction();
            if (Client == null)
                Client = Session.GetObjectByKey<Sys_Client>("ARIA");
        }

        protected override void OnLoaded()
        {
            base.OnLoaded();
            this.Filters.CollectionChanged += new XPCollectionChangedEventHandler(Filters_CollectionChanged);
        }

        void Filters_CollectionChanged(object sender, XPCollectionChangedEventArgs e)
        {
            if (e.CollectionChangedType == XPCollectionChangedType.AfterAdd || e.CollectionChangedType == XPCollectionChangedType.AfterRemove)
            {
                RecalculateFilters();
            }
        }

        public void RecalculateFilters()
        {
            List<Filter> FixedFiltersList = new List<Filter>();
            List<Filter> VariableFiltersList = new List<Filter>();
            Filters.Where(x => x.FilterType == FilterType.FixedFilter)
                .OrderBy(x => x.Number)
               .ToList()
               .ForEach(y =>
               FixedFiltersList.Add(
               new Filter()
               {
                   Name = y.Name,
                   DataType = y.DataType,
                   FilterType = y.FilterType,
                   Not = y.Not,
                   Operator = y.Operator,
                   Type = y.Type,
                   Value = y.Value,
                   ValueType = y.ValueType
               }));

            Filters.Where(x => x.FilterType == FilterType.VariableFilter)
               .OrderBy(x => x.Number)
               .ToList()
               .ForEach(y =>
               VariableFiltersList.Add(
               new Filter()
               {
                   Name = y.Name,
                   DataType = y.DataType,
                   FilterType = y.FilterType,
                   Not = y.Not,
                   Operator = y.Operator,
                   Type = y.Type,
                   Value = y.Value,
                   ValueType = y.ValueType
               }));

            string fixedFilterString = Filter.Convertor(FixedFiltersList);
            string variableFilterString = Filter.Convertor(VariableFiltersList);

            FixedFilters = fixedFilterString;
            VariableFilters = variableFilterString;
            
        }
    }
}