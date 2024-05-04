using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.ConditionalAppearance;
using System.ComponentModel;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{

    [Appearance("DisbleDefaultSequence", TargetItems = "DefaultSequenceNumber", Enabled = false, Criteria = "!Enumerate")]
    [Disable_Database_For_A27()]
    [Enable_Client_For_Custom_LevelOfUpgrade_Only()]
    [RuleCriteria("Sequence shouldn't exceed field width", DefaultContexts.Save, "Len(DefaultSequenceNumber) <= Field.Width", "Sequence Number length cann't exceed Field width")]
    [Persistent("Sydflfld")]
    public class FileField : XPLiteObject, IAuditable
    {
        [Key(true), Persistent()]
        private int ID;

        [Association("File_FileField")]
        [Persistent("cfile_nam"), Size(30)]
        public File File
        {
            get { return GetPropertyValue<File>("File"); }
            set { SetPropertyValue<File>("File", value); }
        }

        [Persistent("cfld_name")]
        public Field Field
        {
            get { return GetPropertyValue<Field>("Field"); }
            set { SetPropertyValue<Field>("Field", value); }
        }

        [Persistent("nfld_pos")]
        public int FieldPosition
        {
            get { return GetPropertyValue<int>("FieldPosition"); }
            set { SetPropertyValue<int>("FieldPosition", value); }
        }

        [Persistent("ctransmtyp"), Size(1)]
        public string TransmissionType
        {
            get { return GetPropertyValue<string>("TransmissionType"); }
            set { SetPropertyValue<string>("TransmissionType", value); }
        }

        [ImmediatePostData(true)]
        [VisibleInListView(true), Size(10)]
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

        [Association("Client-FileField")]
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


        [Persistent("lenumerate"), VisibleInListView(false)]
        [ImmediatePostData()]
        public bool Enumerate
        {
            get { return GetPropertyValue<bool>("Enumerate"); }
            set
            {
                if (!IsLoading)
                {
                    if (!value)
                        DefaultSequenceNumber = 0;
                }
                SetPropertyValue<bool>("Enumerate", value);
            }
        }

        [Persistent("ndef_seq"), VisibleInListView(false)]
        public int DefaultSequenceNumber
        {
            get { return GetPropertyValue<int>("DefaultSequenceNumber"); }
            set { SetPropertyValue<int>("DefaultSequenceNumber", value); }
        }


        protected override void OnSaved()
        {
            base.OnSaved();
            File.NoOfFields = File.Fields.Count;
            // File.UserBrowseFileds = File.ReCalculateBrowseFields();
            //if (BO.ValidateWithoutTicket(File, ContextIdentifier.Save))
            //    Session.CommitTransaction();

            DevExpress.Persistent.AuditTrail.AuditTrailService.Instance.AddCustomAuditData(Session, new DevExpress.Persistent.AuditTrail.AuditDataItem(this, null, "Ticket", Ticket, DevExpress.Persistent.AuditTrail.AuditOperationType.CustomData));
            DevExpress.Persistent.AuditTrail.AuditTrailService.Instance.AddCustomAuditData(Session, new DevExpress.Persistent.AuditTrail.AuditDataItem(this, null, "Comments", Comments, DevExpress.Persistent.AuditTrail.AuditOperationType.CustomData));
            DevExpress.Persistent.AuditTrail.AuditTrailService.Instance.AddCustomAuditData(Session, new DevExpress.Persistent.AuditTrail.AuditDataItem(this, null, "TrackingEntry", TrackingEntry, DevExpress.Persistent.AuditTrail.AuditOperationType.CustomData));
        }

        public FileField(Session session) : base(session) { }
        public FileField() : base(Session.DefaultSession) { }
        public override void AfterConstruction()
        {
            base.AfterConstruction();
            if (Client == null)
                Client = Session.GetObjectByKey<Sys_Client>("ARIA");
        }


        #region Audit Trail


        [NonPersistent()]
        [RuleRequiredField(CustomMessageTemplate = AriaAudit.TicketRequiredMessage), Browsable(false)]
        public string Ticket
        {
            get { return GetPropertyValue<string>("Ticket"); }
            set
            {
                SetPropertyValue<string>("Ticket", value);
                this.File.Ticket = value;
            }
        }


        [NonPersistent()]
        private string TrackingEntry
        {
            get { return GetPropertyValue<string>("TrackingEntry"); }
            set
            {
                SetPropertyValue<string>("TrackingEntry", value);
                this.File.TrackingEntry = value;
            }
        }

        [NonPersistent()]
        private string Comments
        {
            get { return GetPropertyValue<string>("Comments"); }
            set
            {
                SetPropertyValue<string>("Comments", value);
                this.File.Comments = value;
            }
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
            string outreturn = "";
            if (File != null)
                outreturn = File.Name;
            if (Field != null)
                outreturn = string.IsNullOrWhiteSpace(outreturn) ? Field.Name : outreturn + " - " + Field.Name;

            return string.IsNullOrWhiteSpace(outreturn) ? base.ToString() : outreturn;
        }
    }
}