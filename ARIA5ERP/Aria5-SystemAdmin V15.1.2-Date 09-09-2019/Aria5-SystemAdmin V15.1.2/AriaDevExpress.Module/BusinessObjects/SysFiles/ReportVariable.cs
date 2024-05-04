using System;
using System.Xml;
using System.ComponentModel;

using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    //[AppearanceRules.Enable_Client_For_Custom_LevelOfUpgrade_Only()]
    [Disable_Width_For_Date_And_Logical_DataTypes()]
    [Enable_Decimal_Places_For_Numeric_DataType_Only()]
    [Persistent("syrepuvr")]
    public class ReportVariable : BaseObject, IAuditable
    {
        public ReportVariable(Session session)
            : base(session)
        {
            // This constructor is used when an object is loaded from a persistent storage.
            // Do not place any code here or place it only when the IsLoading property is false:
            // if (!IsLoading){
            //    It is now OK to place your initialization code here.
            // }
            // or as an alternative, move your initialization code into the AfterConstruction method.
        }

        [Association("Report-ReportVariables"), NoForeignKey()]
        public Report MainReport
        {
            get { return GetPropertyValue<Report>("MainReport"); }
            set { SetPropertyValue("MainReport", value); }
        }

        #region Link Key
        [Browsable(false), Persistent()]
        public Sys_Client Client
        {
            get
            {
                if (MainReport == null) return null;
                return MainReport.Client;
            }
        }

        [Size(10), Browsable(false), Persistent()]
        [ValueConverter(typeof(EnumValueConverter<Products>))]
        public Products Product
        {
            get
            {
                if (MainReport == null) return Products.ARIA27;
                return MainReport.Product;
            }
        }

        [Browsable(false), Size(10), Persistent()]
        [ValueConverter(typeof(EnumValueConverter<DataBaseTypes>))]
        public DataBaseTypes Database
        {
            get
            {
                if (MainReport == null) return DataBaseTypes.FOX;
                return MainReport.Database;
            }
        }

        [Browsable(false), Size(8), Persistent()]
        public string crep_id
        {
            get
            {
                if (MainReport == null) return null;
                return MainReport.ID;
            }
        }
        #endregion

        [Persistent("mfld_name"), Size(25)]
        [VisibleInListView(true)]
        public string Name
        {
            get { return GetPropertyValue<string>("Name"); }
            set
            {
                if (!IsLoading)
                {
                    if (FilterType == FilterType.FixedFilter && MainReport.FixedFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        MainReport.FixedFilter.Find((filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower())).Name = value;
                    else if (FilterType == FilterType.VariableFilter && MainReport.VariableFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        MainReport.VariableFilter.Find((filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower())).Name = value;
                }

                SetPropertyValue("Name", value);
            }
        }

        [Persistent("cfld_head"), Size(25)]
        [VisibleInListView(true)]
        public string Title
        {
            get { return GetPropertyValue<string>("Title"); }
            set { SetPropertyValue("Title", value); }
        }

        [Persistent("cdata_typ"), Size(1)]
        [ImmediatePostData(true)]
        [VisibleInListView(true)]
        [ValueConverter(typeof(EnumValueConverter<SystemDataType>))]
        public SystemDataType DataType
        {
            get { return GetPropertyValue<SystemDataType>("DataType"); }
            set
            {

                if (!IsLoading)
                {
                    if (FilterType == FilterType.FixedFilter && MainReport.FixedFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        MainReport.FixedFilter.Find((filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower())).DataType = value;
                    else if (FilterType == FilterType.VariableFilter && MainReport.VariableFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        MainReport.VariableFilter.Find((filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower())).DataType = value;
                }

                SetPropertyValue("DataType", value);

                if (!IsLoading)
                {
                    switch (value)
                    {
                        case SystemDataType.C:
                        case SystemDataType.G:
                        case SystemDataType.M:
                        case SystemDataType.N:
                            Width = 10;
                            DecimalPlaces = 0;
                            break;
                        case SystemDataType.D:
                            Width = 8;
                            DecimalPlaces = 0;
                            break;
                        case SystemDataType.L:
                            Width = 1;
                            DecimalPlaces = 0;
                            break;
                    }
                    if (value != SystemDataType.C)
                    {
                        IsCode = false;
                    }
                }
            }
        }

        [RuleRangeAttribute("ReportWidthRangeRule", DefaultContexts.Save, 1, 512)]
        [Persistent("nfld_wdth")]
        [VisibleInListView(true)]
        public int Width
        {
            get { return GetPropertyValue<int>("Width"); }
            set { SetPropertyValue("Width", value); }
        }

        [RuleRangeAttribute("ReportDecimalPlacesRangeRule", DefaultContexts.Save, 0, 16)]
        [Persistent("nfld_dec")]
        [VisibleInListView(true)]
        public int DecimalPlaces
        {
            get { return GetPropertyValue<int>("DecimalPlaces"); }
            set { SetPropertyValue("DecimalPlaces", value); }
        }


        [Persistent("cpict_str"), Size(35)]
        [VisibleInListView(false)]
        public string PictureString
        {
            get { return GetPropertyValue<string>("PictureString"); }
            set { SetPropertyValue("PictureString", value); }
        }

        [Persistent("Mvald_str"), Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string ValidFunction
        {
            get { return GetPropertyValue<string>("ValidFunction"); }
            set { SetPropertyValue("ValidFunction", value); }
        }

        [Persistent("cvldenttyp"), Size(1)]
        [ValueConverter(typeof(EnumValueConverter<ValidEntryType>))]
        [VisibleInListView(false)]
        public ValidEntryType ValidEntryType
        {
            get { return GetPropertyValue<ValidEntryType>("ValidEntryType"); }
            set { SetPropertyValue("ValidEntryType", value); }
        }

        [Persistent("Cexptype"), Size(1)]
        [ValueConverter(typeof(EnumValueConverter<FilterVariableType>))]
        [VisibleInListView(false)]
        public FilterVariableType Type
        {
            get { return GetPropertyValue<FilterVariableType>("Type"); }
            set
            {
                if (!IsLoading)
                {
                    if (FilterType == FilterType.FixedFilter && MainReport.FixedFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        MainReport.FixedFilter.Find((filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower())).Type = value;
                    else if (FilterType == FilterType.VariableFilter && MainReport.VariableFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        MainReport.VariableFilter.Find((filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower())).Type = value;
                }

                SetPropertyValue("Type", value);
            }
        }


        [Persistent("Cdefa_typ"), Size(1)]
        [ValueConverter(typeof(EnumValueConverter<SystemDataType>))]
        [VisibleInListView(false)]
        public SystemDataType DefaultValueType
        {
            get { return GetPropertyValue<SystemDataType>("DefaultValueType"); }
            set { SetPropertyValue("DefaultValueType", value); }
        }

        [Persistent("mdata_def"), Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string DefaultValue
        {
            get { return GetPropertyValue<string>("DefaultValue"); }
            set { SetPropertyValue("DefaultValue", value); }
        }

        [Persistent("Ccodes_fld"), Size(10)]
        [VisibleInListView(false)]
        public string CodeName
        {
            get { return GetPropertyValue<string>("CodeName"); }
            set { SetPropertyValue("CodeName", value); }
        }

        [Persistent("Nvarpos")]
        //[RuleUniqueValue("uniqueReportVariablePostion", DefaultContexts.Save)]
        [VisibleInListView(false)]
        public int Position
        {
            get { return GetPropertyValue<int>("Position"); }
            set { SetPropertyValue("Position", value); }
        }

        [Persistent("Mobjprmpt"), Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string PromptTitle
        {
            get { return GetPropertyValue<string>("PromptTitle"); }
            set { SetPropertyValue("PromptTitle", value); }
        }

        [Persistent("Mventries"), Size(SizeAttribute.Unlimited), Browsable(false)]
        public string Mventries
        {
            get { return GetPropertyValue<string>("Mventries"); }
            set { SetPropertyValue("Mventries", value); }
        }

        [Persistent("Mbrwfile")]
        [VisibleInListView(false)]
        [ImmediatePostData(true)]
        public File Table
        {
            get { return GetPropertyValue<File>("Table"); }
            set
            {
                if (string.IsNullOrWhiteSpace(Convert.ToString(value)))
                    value = null;
                SetPropertyValue("Table", value);
            }
        }

        [Persistent("Cbrwselfld"), Size(30)]
        [VisibleInListView(false)]
        public string ReturnedFieldName
        {
            get { return GetPropertyValue<string>("ReturnedFieldName"); }
            set { SetPropertyValue("ReturnedFieldName", value); }
        }

        [Persistent("mbrwfields"), Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string BrowseFields
        {
            get { return GetPropertyValue<string>("BrowseFields"); }
            set { SetPropertyValue("BrowseFields", value); }
        }

        [NonPersistent()]
        [ImmediatePostData()]
        public FilterType FilterType
        {
            get
            {
                if (MainReport != null)
                {
                    if (MainReport.VariableFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        return FilterType.VariableFilter;
                    else if (MainReport.FixedFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        return FilterType.FixedFilter;
                    else
                        return FilterType.None;
                }
                return FilterType.None;
            }
            set
            {
                if (MainReport != null)
                {
                    MainReport.VariableFilter.RemoveAll(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower());
                    MainReport.FixedFilter.RemoveAll(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower());

                    if (value != FilterType.None)
                    {
                        Filter Filter = new Filter();
                        Filter.Name = Name;
                        Filter.DataType = DataType;
                        Filter.FilterType = value;
                        Filter.Not = Not;
                        Filter.Operator = Operator;
                        if (value == FilterType.FixedFilter)
                            MainReport.FixedFilter.Add(Filter);
                        else if (value == FilterType.VariableFilter)
                            MainReport.VariableFilter.Add(Filter);
                    }
                }
            }
        }

        [NonPersistent()]
        public bool Not
        {
            get
            {
                if (MainReport != null)
                {
                    if (MainReport.VariableFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        return MainReport.VariableFilter.Find(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()).Not;
                    else if (MainReport.FixedFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        return MainReport.FixedFilter.Find(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()).Not;
                    else return false;
                }
                return false;
            }
            set
            {
                if (MainReport != null)
                {
                    if (FilterType == FilterType.FixedFilter && MainReport.FixedFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        MainReport.FixedFilter.Find((filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower())).Not = value;
                    else if (FilterType == FilterType.VariableFilter && MainReport.VariableFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        MainReport.VariableFilter.Find((filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower())).Not = value;
                }
            }
        }


        [NonPersistent()]
        public FilterOperator Operator
        {
            get
            {
                if (MainReport != null)
                {
                    if (MainReport.VariableFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        return MainReport.VariableFilter.Find(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()).Operator;
                    else if (MainReport.FixedFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        return MainReport.FixedFilter.Find(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()).Operator;
                    else return FilterOperator.Is;
                }
                return FilterOperator.Is;
            }
            set
            {
                if (MainReport != null)
                {
                    if (FilterType == FilterType.FixedFilter && MainReport.FixedFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        MainReport.FixedFilter.Find((filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower())).Operator = value;
                    else if (FilterType == FilterType.VariableFilter && MainReport.VariableFilter.Exists(filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower()))
                        MainReport.VariableFilter.Find((filter => filter.Name.Trim().ToLower() == Name.Trim().ToLower())).Operator = value;
                }
            }
        }

        [Persistent("mbrwfltexp")]
        [VisibleInListView(false)]
        public string FilterExpression
        {
            get { return GetPropertyValue<string>("FilterExpression"); }
            set { SetPropertyValue("FilterExpression", value); }
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

                //if (!IsLoading && ((UpgradeLevelType)value) != UpgradeLevelType.U)
                //{
                //    Client = null;
                //}
            }
        }

        //[Association("Client-ReportVariable")]
        //[VisibleInListView(true)]
        //public Client Client
        //{
        //    get { return GetPropertyValue<Client>("Client"); }
        //    set { SetPropertyValue("Client", value); }
        //}

        [Persistent("csetfunc"), Size(10)]
        [VisibleInListView(false)]
        public string SetFunction
        {
            get { return GetPropertyValue<string>("SetFunction"); }
            set { SetPropertyValue("SetFunction", value); }
        }



        [Persistent("Cclrrngsel"), Size(10)]
        [VisibleInListView(false)]
        public string ClearRangeCursorVariable
        {
            get { return GetPropertyValue<string>("ClearRangeCursorVariable"); }
            set { SetPropertyValue("ClearRangeCursorVariable", value); }
        }

        [Persistent("msupexpr"), Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string SuppressExpression
        {
            get { return GetPropertyValue<string>("SuppressExpression"); }
            set { SetPropertyValue("SuppressExpression", value); }
        }

        [Persistent("Cnonestr"), Size(10)]
        [VisibleInListView(false)]
        public string NoneString
        {
            get { return GetPropertyValue<string>("NoneString"); }
            set { SetPropertyValue("NoneString", value); }
        }

        [Persistent("Cassociate"), Size(20)]
        [VisibleInListView(false)]
        public string Associate
        {
            get { return GetPropertyValue<string>("Associate"); }
            set { SetPropertyValue("Associate", value); }
        }

        [Persistent("Lvldentry")]
        [VisibleInListView(false)]
        public bool IsCode
        {
            get { return GetPropertyValue<bool>("IsCode"); }
            set { SetPropertyValue("IsCode", value); }
        }

        [Persistent("Laskrunt")]
        [VisibleInListView(false)]
        public bool AskAtRuntime
        {
            get { return GetPropertyValue<bool>("AskAtRuntime"); }
            set { SetPropertyValue("AskAtRuntime", value); }
        }

        [Persistent("Cobj_type")]
        [ValueConverter(typeof(BoolToPAValueConverter))]
        [VisibleInListView(false)]
        public bool IsPushButton
        {
            get { return GetPropertyValue<bool>("IsPushButton"); }
            set { SetPropertyValue("IsPushButton", value); }
        }

        [Persistent("Ldispog")]
        [VisibleInListView(false)]
        public bool DisplayOptionGrid
        {
            get { return GetPropertyValue<bool>("DisplayOptionGrid"); }
            set { SetPropertyValue("DisplayOptionGrid", value); }
        }

        [NonPersistent()]
        [VisibleInListView(false)]
        public bool OpenBrowserScreen { get; set; }

        [NonPersistent()]
        [VisibleInListView(false)]
        public bool CloseBrowserScreen { get; set; }

        [NonPersistent(), DevExpress.Xpo.DisplayName("Select/Unselect Row")]
        [VisibleInListView(false)]
        public bool SelectRows { get; set; }

        [Persistent("Csetparm")]
        private string Csetparm
        {
            get
            {
                string result = (DisplayOptionGrid ? "Y" : "N");
                result += (OpenBrowserScreen ? "Y" : "N");
                result += (SelectRows ? "Y" : "N");
                return result;
            }
            set
            {
                if (!string.IsNullOrWhiteSpace(value) && value.Length == 3)
                {
                    DisplayOptionGrid = value[0] == 'Y';
                    OpenBrowserScreen = value[1] == 'Y';
                    SelectRows = value[2] == 'Y';
                }
            }
        }

        [Persistent("Lnotchflt")]
        [VisibleInListView(false)]
        public bool RecollectDataWhenReopened
        {
            get { return GetPropertyValue<bool>("RecollectDataWhenReopened"); }
            set { SetPropertyValue("RecollectDataWhenReopened", value); }
        }

        [DevExpress.Xpo.DisplayName("Valid Entries")]
        [Association("ReportVariable-ValidEntries"), Aggregated()]
        public XPCollection<ReportVariableValidEntry> ValidEntriesCollection
        {
            get
            {
                return GetCollection<ReportVariableValidEntry>("ValidEntriesCollection");
            }
        }


        #region Audit Trail

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

        protected override void OnSaved()
        {
            DevExpress.Persistent.AuditTrail.AuditTrailService.Instance.AddCustomAuditData(Session, new DevExpress.Persistent.AuditTrail.AuditDataItem(this, null, "Ticket", Ticket, DevExpress.Persistent.AuditTrail.AuditOperationType.CustomData));
            DevExpress.Persistent.AuditTrail.AuditTrailService.Instance.AddCustomAuditData(Session, new DevExpress.Persistent.AuditTrail.AuditDataItem(this, null, "Comments", Comments, DevExpress.Persistent.AuditTrail.AuditOperationType.CustomData));
            DevExpress.Persistent.AuditTrail.AuditTrailService.Instance.AddCustomAuditData(Session, new DevExpress.Persistent.AuditTrail.AuditDataItem(this, null, "TrackingEntry", TrackingEntry, DevExpress.Persistent.AuditTrail.AuditOperationType.CustomData));
            base.OnSaved();
            if (MainReport != null)
            {
                MainReport.Save();
                MainReport.Session.CommitTransaction();
            }
        }

        public override void AfterConstruction()
        {
            base.AfterConstruction();
            // Place here your initialization code.
        }
    }

}