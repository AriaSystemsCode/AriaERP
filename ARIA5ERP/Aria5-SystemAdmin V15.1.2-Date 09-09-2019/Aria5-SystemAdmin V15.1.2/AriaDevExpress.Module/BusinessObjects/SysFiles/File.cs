using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.Persistent.AuditTrail;
using System.ComponentModel;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [Appearance("Disable No of Fields & Indices", TargetItems = "NoOfFields,NoOfIndices", Enabled = false)]
    [Disable_Database_For_A27()]
    [Enable_Client_For_Custom_LevelOfUpgrade_Only()]

    [Persistent("SYDFILES")]
    public class File : XPCustomObject, IAuditable
    {

        [RuleUniqueValue("UinqueKey", DefaultContexts.Save)]
        [Persistent("cfile_nam"), Size(30), Key(false), VisibleInListView(true)]
        public string Name
        {
            get { return GetPropertyValue<string>("Name"); }
            set { SetPropertyValue<string>("Name", string.IsNullOrEmpty(value) ? value : value.ToUpper()); }
        }

        [Persistent("cfile_ttl"), Size(34)]
        [VisibleInListView(true)]
        public string Title
        {
            get { return GetPropertyValue<string>("Title"); }
            set { SetPropertyValue<string>("Title", value); }
        }

        [Persistent("lsqlfile")]
        [VisibleInListView(false), Browsable(false)]
        public bool SqlFile
        {
            get { return GetPropertyValue<bool>("SqlFile"); }
            set { SetPropertyValue<bool>("SqlFile", value); }
        }

        [Persistent("nfld_no")]
        [ImmediatePostData(true), VisibleInListView(false)]
        public int NoOfFields
        {
            get { return GetPropertyValue<int>("NoOfFields"); }
            set { SetPropertyValue<int>("NoOfFields", value); }
        }

        [Persistent("nindx_no")]
        [ImmediatePostData(true), VisibleInListView(false)]
        public int NoOfIndices
        {
            get { return GetPropertyValue<int>("NoOfIndices"); }
            set { SetPropertyValue<int>("NoOfIndices", value); }
        }

        [Persistent("mfile_app"), Size(30)]
        [ImmediatePostData(true), VisibleInListView(false)]
        public string Application
        {
            get { return GetPropertyValue<string>("Application"); }
            set { SetPropertyValue<string>("Application", value); }
        }

        [NonPersistent()]
        [DataSourceProperty("Indices")]
        [VisibleInListView(false)]
        public Index MasterTag
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(cfile_tag))
                {
                    foreach (Index index in Indices)
                    {
                        if (index.FileTag == cfile_tag)
                            return index;
                    }

                }
                return null;
            }
            set
            {
                if (value == null)
                    cfile_tag = null;
                else
                    cfile_tag = value.FileTag;
            }
        }

        [Persistent("cfile_tag"), Size(10)]
        private string cfile_tag
        {
            get { return GetPropertyValue<string>("cfile_tag"); }
            set { SetPropertyValue<string>("cfile_tag", value); }
        }

        [Persistent("lsys_data")]
        [VisibleInListView(false)]
        public bool PermitSystemData
        {
            get { return GetPropertyValue<bool>("PermitSystemData"); }
            set { SetPropertyValue<bool>("PermitSystemData", value); }
        }

        [Persistent("lapp_data")]
        [VisibleInListView(false)]
        public bool PermitApplicationData
        {
            get { return GetPropertyValue<bool>("PermitApplicationData"); }
            set { SetPropertyValue<bool>("PermitApplicationData", value); }
        }

        [Persistent("lusr_data")]
        [VisibleInListView(false)]
        public bool PermitUserData
        {
            get { return GetPropertyValue<bool>("PermitUserData"); }
            set { SetPropertyValue<bool>("PermitUserData", value); }
        }

        [Persistent("lfldfdata")]
        [VisibleInListView(false)]
        public bool HasDefaultData
        {
            get { return GetPropertyValue<bool>("HasDefaultData"); }
            set { SetPropertyValue<bool>("HasDefaultData", value); }
        }

        [Persistent("mfldfdata"), Size(30)]
        [VisibleInListView(false)]
        public string DefaultData
        {
            get { return GetPropertyValue<string>("DefaultData"); }
            set { SetPropertyValue<string>("DefaultData", value); }
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
            set
            {
                SetPropertyValue("Database", value);
                if (!IsLoading)
                {
                    SqlFile = value == DataBaseTypes.SQL;
                }
            }
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

        [Association("Client-File")]
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

        [Persistent("cnotekey"), Size(120)]
        [VisibleInListView(false)]
        public string NotepadKey
        {
            get { return GetPropertyValue<string>("NotepadKey"); }
            set { SetPropertyValue<string>("NotepadKey", value); }
        }

        [Persistent("musr_brow"), Size(30)]
        [VisibleInListView(false)]
        public string UserBrowseFileds
        {
            get { return GetPropertyValue<string>("UserBrowseFileds"); }
            set { SetPropertyValue<string>("UserBrowseFileds", value); }
        }

        [Association("File_Index", typeof(Index)), Aggregated]
        public XPCollection Indices
        {
            get { return GetCollection("Indices"); }
        }

        [Association("File_FileField", typeof(FileField)), Aggregated]
        public XPCollection<FileField> Fields
        {
            get { return GetCollection<FileField>("Fields"); }
        }


        [Association("Module-File")]
        public XPCollection<Module> Modules
        {
            get { return GetCollection<Module>("Modules"); }
        }


        protected override void OnLoaded()
        {
            base.OnLoaded();
            Indices.CollectionChanged += new XPCollectionChangedEventHandler(index_CollectionChanged);
            Fields.CollectionChanged += new XPCollectionChangedEventHandler(Fields_CollectionChanged);
            Modules.CollectionChanged += new XPCollectionChangedEventHandler(Modules_CollectionChanged);
        }

        void Modules_CollectionChanged(object sender, XPCollectionChangedEventArgs e)
        {
            if (e.CollectionChangedType == XPCollectionChangedType.AfterAdd || e.CollectionChangedType == XPCollectionChangedType.AfterRemove)
            {
                // Ticket = string.IsNullOrEmpty(Ticket) ? "N/A" : Ticket;
                Application = ReCalculateModules();
                //if (BO.ValidateWithoutTicket(this, ContextIdentifier.Save))
                //    Session.CommitTransaction();
            }
        }

        void Fields_CollectionChanged(object sender, XPCollectionChangedEventArgs e)
        {
            if (e.CollectionChangedType == XPCollectionChangedType.AfterRemove)
            {
                //    Ticket = string.IsNullOrEmpty(Ticket) ? "N/A" : Ticket;
                NoOfFields = Fields.Count;
                //  UserBrowseFileds = ReCalculateBrowseFields();
                //    if (BO.ValidateWithoutTicket(this, ContextIdentifier.Save))
                //        Session.CommitTransaction();
            }
        }

        void index_CollectionChanged(object sender, XPCollectionChangedEventArgs e)
        {
            if (e.CollectionChangedType == XPCollectionChangedType.AfterRemove)
            {

                NoOfIndices = Indices.Count;
                //   Ticket = string.IsNullOrEmpty(Ticket) ? "N/A" : Ticket;
                //   if (BO.ValidateWithoutTicket(this, ContextIdentifier.Save))
                //       Session.CommitTransaction();
            }
        }

        // no need for this as bvrowse fields selected and calculated from custom selector window
        //public string ReCalculateBrowseFields()
        //{
        //    string browseFieldsString = "";
        //    if (Fields != null && Fields.Count > 0)
        //    {
        //        foreach (FileField field in Fields)
        //            if (!field.IsDeleted && field.IsBrowseField)
        //                browseFieldsString += field.Field.Name + "|";
        //        browseFieldsString = browseFieldsString.EndsWith("|") ? browseFieldsString.Remove(browseFieldsString.Length - 1) : browseFieldsString;
        //    }
        //    return browseFieldsString;
        //}

        public string ReCalculateModules()
        {
            string ModulesString = "";
            if (Modules != null && Modules.Count > 0)
            {
                foreach (Module module in Modules)
                    if (!module.IsDeleted)
                        ModulesString += module.ModuleName + ",";
                ModulesString = ModulesString.EndsWith(",") ? ModulesString.Remove(ModulesString.Length - 1) : ModulesString;
            }
            return ModulesString;
        }



        public File(Session session) : base(session) { }
        public File() : base(Session.DefaultSession) { }
        public override void AfterConstruction()
        {
            base.AfterConstruction();
            if (Client == null)
                Client = Session.GetObjectByKey<Sys_Client>("ARIA");
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


        [NonPersistent(), Browsable(false)]
        public string TrackingEntry
        {
            get { return GetPropertyValue<string>("TrackingEntry"); }
            set { SetPropertyValue<string>("TrackingEntry", value); }
        }

        [NonPersistent(), Browsable(false)]
        public string Comments
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

        public override string ToString()
        {
            return Name;
        }

    }
}
