using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.ConditionalAppearance;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [Enable_Client_For_Custom_LevelOfUpgrade_Only()]
    [Disable_Database_For_A27()]
    [Persistent("SYDINDEX")]
    [FriendlyKeyProperty("FileTag")]
    public class Index : XPLiteObject, IAuditable
    {
        [Key(true), Persistent()]
        private int ID;

        [Size(10)]
        [Persistent("cfile_tag")]
        [VisibleInListView(true), ImmediatePostData(true)]
        public string FileTag
        {
            get { return GetPropertyValue<string>("FileTag"); }
            set { SetPropertyValue<string>("FileTag", string.IsNullOrEmpty(value) ? value : value.ToUpper()); }
        }

        [Persistent("cindx_nam"), Size(12)]
        [VisibleInListView(true)]
        [DisplayName("Index Name")]
        public string Indexnam
        {
            get
            {
                return Filenam.Name;
            }
        }

        [Association("File_Index")]
        [Persistent("cfile_nam")]
        [VisibleInListView(false)]
        public File Filenam
        {
            get { return GetPropertyValue<File>("Filenam"); }
            set { SetPropertyValue<File>("Filenam", value); }
        }

        [Persistent("mindx_des"), Size(30)]
        [VisibleInListView(true)]
        public string TagDescription
        {
            get { return GetPropertyValue<string>("TagDescription"); }
            set { SetPropertyValue<string>("TagDescription", value); }
        }

        [Persistent("cindx_exp"), Size(120)]
        [VisibleInListView(true)]
        public string TagExpression
        {
            get { return GetPropertyValue<string>("TagExpression"); }
            set { SetPropertyValue<string>("TagExpression", string.IsNullOrWhiteSpace(value) || value.Length <= 120 ? value : value.Remove(120)); }
        }

        [NonPersistent()]
        [VisibleInListView(true)]
        public OrderDirection Order
        {
            get { return GetPropertyValue<OrderDirection>("Order"); }
            set { SetPropertyValue<OrderDirection>("Order", value); }
        }

        [Persistent("lascend")]
        private bool AscendingIndex
        {
            get
            {
                return Order == OrderDirection.Ascending;
            }
            set
            {
                Order = value ? OrderDirection.Ascending : OrderDirection.Descending;
            }
        }

        [Persistent("lunique")]
        [VisibleInListView(true)]
        public bool UniqueIndex
        {
            get { return GetPropertyValue<bool>("UniqueIndex"); }
            set { SetPropertyValue<bool>("UniqueIndex", value); }
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

        [Association("Client-Index")]
        [VisibleInListView(true)]
        [Custom("LookupEditorMode", "AllItems")]
        public Sys_Client Client
        {
            get
            {
                return GetPropertyValue<Sys_Client>("Client");
            }
            set { SetPropertyValue("Client", value); }
        }

        [NonPersistent()]
        [VisibleInListView(false)]
        public bool MasterTag
        {
            get
            {
                return this == Filenam.MasterTag;
            }
            set
            {
                Filenam.MasterTag = value ? this : null;
            }
        }

        protected override void OnSaved()
        {
            base.OnSaved();
            Filenam.NoOfIndices = Filenam.Indices.Count;
            //if (BO.ValidateWithoutTicket(Filenam, ContextIdentifier.Save))
            //    Session.CommitTransaction();
            DevExpress.Persistent.AuditTrail.AuditTrailService.Instance.AddCustomAuditData(Session, new DevExpress.Persistent.AuditTrail.AuditDataItem(this, null, "Ticket", Ticket, DevExpress.Persistent.AuditTrail.AuditOperationType.CustomData));
            DevExpress.Persistent.AuditTrail.AuditTrailService.Instance.AddCustomAuditData(Session, new DevExpress.Persistent.AuditTrail.AuditDataItem(this, null, "Comments", Comments, DevExpress.Persistent.AuditTrail.AuditOperationType.CustomData));
            DevExpress.Persistent.AuditTrail.AuditTrailService.Instance.AddCustomAuditData(Session, new DevExpress.Persistent.AuditTrail.AuditDataItem(this, null, "TrackingEntry", TrackingEntry, DevExpress.Persistent.AuditTrail.AuditOperationType.CustomData));
        }


        #region Audit Trail

        [NonPersistent()]
        [RuleRequiredField(CustomMessageTemplate = AriaAudit.TicketRequiredMessage), System.ComponentModel.Browsable(false)]
        public string Ticket
        {
            get { return GetPropertyValue<string>("Ticket"); }
            set
            {
                SetPropertyValue<string>("Ticket", value);
                this.Filenam.Ticket = value;
            }
        }


        [NonPersistent()]
        private string TrackingEntry
        {
            get { return GetPropertyValue<string>("TrackingEntry"); }
            set
            {
                SetPropertyValue<string>("TrackingEntry", value);
                this.Filenam.TrackingEntry = value;
            }
        }

        [NonPersistent()]
        private string Comments
        {
            get { return GetPropertyValue<string>("Comments"); }
            set
            {
                SetPropertyValue<string>("Comments", value);
                this.Filenam.Comments = value;
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


        public Index(Session session) : base(session) { }
        public Index() : base(Session.DefaultSession) { }

        public override string ToString()
        {
            return string.IsNullOrWhiteSpace(FileTag) ? base.ToString() : FileTag;
        }
        public override void AfterConstruction()
        {
            base.AfterConstruction();
            if (Client == null)
                Client = Session.GetObjectByKey<Sys_Client>("ARIA");
        }
    }
}
