using System;
using System.ComponentModel;

using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.ConditionalAppearance;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
   
    [Disable_Database_For_A27()]
    [Enable_Client_For_Custom_LevelOfUpgrade_Only()]
    [Appearance("HideCbarModule", TargetItems = "cbarmodule", Visibility = DevExpress.ExpressApp.Editors.ViewItemVisibility.Hide)]
    [Persistent("SycMenu")]
    public class Menu : BaseObject, IAuditable
    {
        public Menu(Session session)
            : base(session)
        {
            // This constructor is used when an object is loaded from a persistent storage.
            // Do not place any code here or place it only when the IsLoading property is false:
            // if (!IsLoading){
            //    It is now OK to place your initialization code here.
            // }
            // or as an alternative, move your initialization code into the AfterConstruction method.
        }

        [Persistent("capp_id"), DbType("nvarchr(2)")]
        public Module Module
        {
            get { return GetPropertyValue<Module>("Module"); }
            set { SetPropertyValue<Module>("Module", value); }
        }

        [Persistent("cpad_pos"), Size(2)]
        [RuleRange("PadRange", DefaultContexts.Save, 0, 99)]
        [ValueConverter(typeof(IntToStringPaddedConverter))]
        public int Pad
        {
            get { return GetPropertyValue<int>("Pad"); }
            set { SetPropertyValue<int>("Pad", value); }
        }

        [Persistent("cpop_pos"), Size(2)]
        [RuleRange("PopRange", DefaultContexts.Save, 0, 99)]
        [ValueConverter(typeof(IntToStringPaddedConverter))]
        public int Pop
        {
            get { return GetPropertyValue<int>("Pop"); }
            set { SetPropertyValue<int>("Pop", value); }
        }

        [Persistent("cbar_pos"), Size(2)]
        [RuleRange("BarRange", DefaultContexts.Save, 0, 99)]
        [ValueConverter(typeof(IntToStringPaddedConverter))]
        public int bar
        {
            get { return GetPropertyValue<int>("bar"); }
            set { SetPropertyValue<int>("bar", value); }
        }

        [Persistent("cpop_levl"), Size(2)]
        [ValueConverter(typeof(EnumValueConverter<MenuPopLevel>))]
        public MenuPopLevel PopLevel
        {
            get { return GetPropertyValue<MenuPopLevel>("PopLevel"); }
            set { SetPropertyValue<MenuPopLevel>("PopLevel", value); }
        }

        [Persistent("csub_ctg"), Size(2)]
        [ValueConverter(typeof(EnumValueConverter<MenuSubCategory>))]
        public MenuSubCategory SubCategory
        {
            get { return GetPropertyValue<MenuSubCategory>("SubCategory"); }
            set { SetPropertyValue<MenuSubCategory>("SubCategory", value); }
        }

        [Persistent("csub_typ"), Size(1)]
        [ValueConverter(typeof(EnumValueConverter<MenuSubType>))]
        public MenuSubType SubType
        {
            get { return GetPropertyValue<MenuSubType>("SubType"); }
            set { SetPropertyValue<MenuSubType>("SubType", value); }
        }

        [Persistent("csub_prpt"), Size(45)]
        public string SubPrompt
        {
            get { return GetPropertyValue<string>("SubPrompt"); }
            set { SetPropertyValue<string>("SubPrompt", value); }
        }

        [Persistent("cmstr_nam"), Size(10)]
        public string MasterName
        {
            get { return GetPropertyValue<string>("MasterName"); }
            set { SetPropertyValue<string>("MasterName", value); }
        }

        [Persistent("cpross_id"), Size(10)]
        public string ProssId
        {
            get { return GetPropertyValue<string>("ProssId"); }
            set { SetPropertyValue<string>("ProssId", value); }
        }

        [Persistent("cproctype"), Size(1)]
        [ValueConverter(typeof(EnumValueConverter<MenuProcType>))]
        public MenuProcType ProcdureType
        {
            get { return GetPropertyValue<MenuProcType>("ProcdureType"); }
            set { SetPropertyValue<MenuProcType>("ProcdureType", value); }
        }

        [Persistent("csub_pos"), Size(2)]
        [RuleRange("PositionRange", DefaultContexts.Save, 0, 99)]
        [ValueConverter(typeof(IntToStringPaddedConverter))]
        public int Position
        {
            get { return GetPropertyValue<int>("Position"); }
            set { SetPropertyValue<int>("Position", value); }
        }

        [Persistent("csub_hkey"), Size(30)]
        public string HotKey
        {
            get { return GetPropertyValue<string>("HotKey"); }
            set { SetPropertyValue<string>("HotKey", value); }
        }

        [Persistent("csub_msg"), Size(50)]
        public string Message
        {
            get { return GetPropertyValue<string>("Message"); }
            set { SetPropertyValue<string>("Message", value); }
        }

        [Persistent("cprocpath"), Size(60)]
        public string ProcedurePath
        {
            get { return GetPropertyValue<string>("ProcedurePath"); }
            set { SetPropertyValue<string>("ProcedurePath", value); }
        }

        [Persistent("Ldefault")]
        public bool Default
        {
            get { return GetPropertyValue<bool>("Default"); }
            set { SetPropertyValue<bool>("Default", value); }
        }

        [Persistent("Cmenuparam"), Size(50)]
        public string Paramter
        {
            get { return GetPropertyValue<string>("Paramter"); }
            set { SetPropertyValue<string>("Paramter", value); }
        }

        [Persistent("chlptopic"), Size(55)]
        public string HelpTopic
        {
            get { return GetPropertyValue<string>("HelpTopic"); }
            set { SetPropertyValue<string>("HelpTopic", value); }
        }

        [Persistent("CIcon"), Size(12)]
        public string Icon
        {
            get { return GetPropertyValue<string>("Icon"); }
            set { SetPropertyValue<string>("Icon", value); }
        }

        [ImmediatePostData(true), VisibleInListView(true), Size(10)]
        [ValueConverter(typeof(EnumValueConverter<Products>))]
        public Products Product
        {
            get { return GetPropertyValue<Products>("Product"); }
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
            get { return GetPropertyValue<DataBaseTypes>("Database"); }
            set { SetPropertyValue("Database", value); }
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

        [Association("Client-Menu")]
        [VisibleInListView(true)]
        [Custom("LookupEditorMode", "AllItems")]
        public Sys_Client Client
        {
            get
            {
                var client = GetPropertyValue<Sys_Client>("Client");
                return client;
            }
            set { SetPropertyValue("Client", value); }
        }


        [Persistent("cbarmodule"), Size(60)]
        public string cbarmodule
        {
            get { return GetPropertyValue<string>("cbarmodule"); }
            set { SetPropertyValue<string>("cbarmodule", value); }
        }

        [Association("Menu-Module")]
        public XPCollection<Module> BarModule
        {
            get { return GetCollection<Module>("BarModule"); }
        }

        public string ReCalculateModules()
        {
            string ModulesString = "";
            if (BarModule != null && BarModule.Count > 0)
            {
                foreach (Module module in BarModule)
                    if (!module.IsDeleted)
                        ModulesString += module.ModuleName + ",";
                ModulesString = ModulesString.EndsWith(",") ? ModulesString.Remove(ModulesString.Length - 1) : ModulesString;
            }
            return ModulesString;
        }

        protected override void OnLoaded()
        {
            base.OnLoaded();
            BarModule.CollectionChanged += new XPCollectionChangedEventHandler(BarModule_CollectionChanged);
        }

        void BarModule_CollectionChanged(object sender, XPCollectionChangedEventArgs e)
        {
            if (e.CollectionChangedType == XPCollectionChangedType.AfterAdd || e.CollectionChangedType == XPCollectionChangedType.AfterRemove)
            {
              //  Ticket = "N/A";
                cbarmodule = ReCalculateModules();
              //  if (BO.ValidateWithoutTicket(this, ContextIdentifier.Save))
              //      Session.CommitTransaction();
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

    }

}
