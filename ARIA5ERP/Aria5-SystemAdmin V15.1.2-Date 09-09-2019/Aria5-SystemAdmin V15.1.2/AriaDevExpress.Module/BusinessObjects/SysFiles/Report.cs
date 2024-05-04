using System;
using System.ComponentModel;

using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.ConditionalAppearance;
using System.Collections.Generic;
using DevExpress.Persistent.AuditTrail;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [FriendlyKeyProperty("ID")]
    [Disable_Database_For_A27()]
    //  [Appearance("DisableDatabaseForA27", Criteria = "ToStr(PrimaryKey.Product) = 'ARIA27'", TargetItems = "PrimaryKey.Database", Enabled = false)]
    [Enable_Client_For_Custom_LevelOfUpgrade_Only()]
    //   [Appearance("DisableClientforSystem", Criteria = "ToStr(LevelOfUpgrade) != 'U'", TargetItems = "PrimaryKey.Client", Enabled = false)]
    [Disable_Width_For_Date_And_Logical_DataTypes()]
    [Enable_Decimal_Places_For_Numeric_DataType_Only()]
    [RuleCombinationOfPropertiesIsUnique("Test", DefaultContexts.Save, "ID,Database,Product,Client")]

    [Persistent("sydreprt")]
    public class Report : XPLiteObject, IAuditable
    {
        public Report(Session session)
            : base(session)
        {
            // This constructor is used when an object is loaded from a persistent storage.
            // Do not place any code here or place it only when the IsLoading property is false:
            // if (!IsLoading){
            //    It is now OK to place your initialization code here.
            // }
            // or as an alternative, move your initialization code into the AfterConstruction method.
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


        [Size(8), ImmediatePostData(true)]
        [Persistent("crep_id")]
        //[RuleUniqueValue("ReportIDUniqueRule", DefaultContexts.Save, CriteriaEvaluationBehavior = PersistentCriteriaEvaluationBehavior.BeforeTransaction)]
        [RuleRequiredFieldAttribute()]
        [VisibleInListView(true)]
        public string ID
        {
            get
            {
                //return PrimaryKey.ID;
                return GetPropertyValue<string>("ID");
            }
            set
            {
                // _primaryKey.ID = value;
                SetPropertyValue("ID", string.IsNullOrEmpty(value) ? value : value.ToUpper());
            }
        }

        [Persistent("crep_name"), Size(45)]
        public string Name
        {
            get { return GetPropertyValue<string>("Name"); }
            set { SetPropertyValue("Name", value); }
        }

        [Persistent("capobjtyp"), Size(1)]
        [ValueConverter(typeof(EnumValueConverter<sydReprtObjectType>))]
        public sydReprtObjectType Type
        {
            get { return GetPropertyValue<sydReprtObjectType>("Type"); }
            set { SetPropertyValue("Type", value); }
        }

        [Size(SizeAttribute.Unlimited), Persistent("mrep_desc")]
        public string Description
        {
            get { return GetPropertyValue<string>("Description"); }
            set { SetPropertyValue("Description", value); }
        }

        [Persistent("capp_id")]
        public Module ModuleID
        {
            get { return GetPropertyValue<Module>("ModuleID"); }
            set { SetPropertyValue("ModuleID", value); }
        }
        [Persistent("Creadwhen"), Size(12)]
        public string WhenProgramName
        {
            get { return GetPropertyValue<string>("WhenProgramName"); }
            set { SetPropertyValue("WhenProgramName", value); }
        }

        [Persistent("Cformvar"), Size(10)]
        public string VariableReportName
        {
            get { return GetPropertyValue<string>("VariableReportName"); }
            set { SetPropertyValue("VariableReportName", value); }
        }

        [Persistent("creadvald"), Size(12)]
        public string ValidFunctionName
        {
            get { return GetPropertyValue<string>("ValidFunctionName"); }
            set { SetPropertyValue("ValidFunctionName", value); }
        }

        [Persistent("creadshow"), Size(12)]
        public string ShowFunctionName
        {
            get { return GetPropertyValue<string>("ShowFunctionName"); }
            set { SetPropertyValue("ShowFunctionName", value); }
        }

        [Persistent("cmainrepid")]
        public string ReportProgramName
        {
            get { return ID; }
        }

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

        [Association("Client-Report")]
        [VisibleInListView(true), RuleRequiredField()]
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

        [ImmediatePostData(true), RuleRequiredField()]
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

        [VisibleInListView(true), Size(10), RuleRequiredField()]
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

        [Persistent("Cmainentr"), Size(1)]
        [ValueConverter(typeof(BoolToYNValueConverter))]
        public bool? ChildOptionGrid
        {
            get { return GetPropertyValue<bool?>("ChildOptionGrid"); }
            set { SetPropertyValue("ChildOptionGrid", value); }
        }

        [Persistent("mrelprogs"), Size(SizeAttribute.Unlimited)]
        public string RelatedPrograms
        {
            get { return GetPropertyValue<string>("RelatedPrograms"); }
            set { SetPropertyValue("RelatedPrograms", value); }
        }

        [Persistent("mcallmods"), Size(SizeAttribute.Unlimited)]
        private string mcallmods
        {
            get { return GetPropertyValue<string>("mcallmods"); }
            set { SetPropertyValue("mcallmods", value); }
        }

        [Persistent("A5OBJNAM"), Size(100)]
        public string Aria5ObjectID
        {
            get { return GetPropertyValue<string>("Aria5ObjectID"); }
            set { SetPropertyValue("Aria5ObjectID", value); }
        }

        [Persistent("MrepFxFlt"), Size(SizeAttribute.Unlimited), DbType("nvarchar(MAX)")]
        [Appearance("HideFixedFilter", Visibility = DevExpress.ExpressApp.Editors.ViewItemVisibility.Hide)]
        [ValueConverter(typeof(FilterConverter))]
        public List<Filter> FixedFilter
        {
            get
            {
                var filters = GetPropertyValue<List<Filter>>("FixedFilter");
                if (filters == null)
                    filters = new List<Filter>();
                return filters;
            }
            set { SetPropertyValue("FixedFilter", value); }
        }

        [Persistent("MrepVrFlt"), Size(SizeAttribute.Unlimited), DbType("nvarchar(MAX)")]
        [Appearance("HideVariableFilter", Visibility = DevExpress.ExpressApp.Editors.ViewItemVisibility.Hide)]
        [ValueConverter(typeof(FilterConverter))]
        public List<Filter> VariableFilter
        {
            get
            {
                var filters = GetPropertyValue<List<Filter>>("VariableFilter");
                if (filters == null)
                    filters = new List<Filter>();
                return filters;
            }
            set { SetPropertyValue("VariableFilter", value); }
        }

        [Association("ReportCalledFrom-Modules"), NoForeignKey()]
        public XPCollection<Module> CalledFrom { get { return GetCollection<Module>("CalledFrom"); } }

        [Association("Report-ReportVariables"), Aggregated()]
        public XPCollection<ReportVariable> variables
        {
            get { return GetCollection<ReportVariable>("variables"); }
        }

        protected override void OnLoaded()
        {
            base.OnLoaded();
            CalledFrom.CollectionChanged += new XPCollectionChangedEventHandler(CalledFrom_CollectionChanged);
        }


        void CalledFrom_CollectionChanged(object sender, XPCollectionChangedEventArgs e)
        {
            if (e.CollectionChangedType == XPCollectionChangedType.AfterRemove || e.CollectionChangedType == XPCollectionChangedType.AfterAdd)
            {
                //Ticket = "N/A";
                mcallmods = ReCalculateCalledModule();
              //  if (BO.ValidateWithoutTicket(this, ContextIdentifier.Save))
              //      Session.CommitTransaction();
            }
        }

        private string ReCalculateCalledModule()
        {
            string calledModuleString = "";
            if (CalledFrom != null && CalledFrom.Count > 0)
            {
                foreach (Module module in CalledFrom)
                    if (!module.IsDeleted)
                        calledModuleString += module.ModuleName + ",";
                calledModuleString = calledModuleString.EndsWith(",") ? calledModuleString.Remove(calledModuleString.Length - 1) : calledModuleString;
            }
            return calledModuleString;
        }

        public void CalculateFilters()
        {
            int fixedFilterCount = 0, variableFilterCount = 0;
            foreach (ReportVariable variable in variables)
            {
                if (variable.FilterType == FilterType.FixedFilter)
                    fixedFilterCount++;
                else if (variable.FilterType == FilterType.VariableFilter)
                    variableFilterCount++;
            }

            object[,] fixedFilterArray = new object[fixedFilterCount, 7];
            string[,] variableFilterArray = new string[variableFilterCount, 7];

            int fixedFilterindex = 0, variableFilterindex = 0;
            Array array = null;
            int index = 0;
            foreach (ReportVariable variable in variables)
            {
                if (variable.FilterType != FilterType.None)
                {
                    if (variable.FilterType == FilterType.FixedFilter)
                    {
                        array = fixedFilterArray;
                        index = fixedFilterindex;
                    }
                    else if (variable.FilterType == FilterType.VariableFilter)
                    {
                        array = variableFilterArray;
                        index = variableFilterindex;
                    }
                    array.SetValue(variable.Name, index, 0);
                    array.SetValue(variable.Type == FilterVariableType.F ? "F" : "E", index, 1);
                    array.SetValue(variable.DataType.ToString(), index, 2);
                    array.SetValue(variable.Not ? false : true, index, 3);
                    array.SetValue(variable.Operator.ToString(), index, 4);
                    array.SetValue("", index, 5);
                    array.SetValue("V", index, 6);
                    index++;
                }
            }
            string LAOGFXFLT = null, LAOGVRFLT = null;
            ArrayToMemo.Main arrayToMemo = new ArrayToMemo.Main();
            if (fixedFilterCount > 0)
                LAOGFXFLT = arrayToMemo.ArrayToMemo(fixedFilterArray, "LAOGFXFLT");
            if (variableFilterCount > 0)
                LAOGVRFLT = arrayToMemo.ArrayToMemo(variableFilterArray, "LAOGVRFLT");
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

    }

}