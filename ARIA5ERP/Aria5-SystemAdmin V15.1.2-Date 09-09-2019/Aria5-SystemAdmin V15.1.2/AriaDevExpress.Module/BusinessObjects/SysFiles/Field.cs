using System;
using System.ComponentModel;

using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using System.Collections.Generic;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.ExpressApp.Editors;
using DevExpress.Xpo.Metadata;


namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [Appearance("DisableRelatedFields+ValidEntries", TargetItems = "RelatedFields,ValidEntries", Visibility = ViewItemVisibility.Hide)]
    [Appearance("IsRelated", TargetItems = "IsRelated", Enabled = false)]
    [Appearance("DisableLocationOfValidFunc", TargetItems = "LocationOfValidFunc", Enabled = false, Criteria = "IsNullOrEmpty(ValidString)")]
    [Appearance("DisableCodeIsEditable", TargetItems = "CodeIsEditable", Enabled = false, Criteria = "!IsCode")]
    [Appearance("DisableIsCode", TargetItems = "IsCode", Enabled = false, Criteria = "ToStr(DataType) != 'C'")]
    [Appearance("HideRelatedFiedls", TargetItems = "RelatedFieldsCollection", Visibility = ViewItemVisibility.Hide, Criteria = "!IsCode")]
    [Appearance("Hide Valid Entries", TargetItems = "ValidEntriesCollection", Visibility = ViewItemVisibility.Hide, Criteria = "ToStr(DataType) != 'C'")]
    [Enable_Decimal_Places_For_Numeric_DataType_Only()]
    [Disable_Width_For_Date_And_Logical_DataTypes()]
    [Disable_Database_For_A27()]
    [Enable_Client_For_Custom_LevelOfUpgrade_Only()]

    [RuleCriteria("Cannot Delete IF Related", DefaultContexts.Delete, "!IsRelated", CustomMessageTemplate = "Can't Delete this field because it is related to another field!")]
    [RuleCriteria("Cannot Delete IF Used By Table", DefaultContexts.Delete, "UsedBy.Count = 0", CustomMessageTemplate = "Can't Delete this field because it is used by table(s)!")]
    //[RuleCriteria("NamingConvention", DefaultContexts.Save, "Substring(Name,0,1) = Substring(ToStr(DataType),0,1)", "Name doesn't follw selected DataType Naming Convention !")]
    [Persistent("SYDFIELD")]
    public class Field : XPLiteObject, IAuditable
    {
        public Field(Session session)
            : base(session)
        {
            // This constructor is used when an object is loaded from a persistent storage.
            // Do not place any code here or place it only when the IsLoading property is false:
            // if (!IsLoading){
            //    It is now OK to place your initialization code here.
            // }
            // or as an alternative, move your initialization code into the AfterConstruction method.
        }

        [Key(AutoGenerate = false)]
        [RuleUniqueValue("UniqueRule", DefaultContexts.Save, CriteriaEvaluationBehavior = DevExpress.Persistent.Validation.CriteriaEvaluationBehavior.BeforeTransaction)]
        [RuleRequiredFieldAttribute()]
        [ImmediatePostData(true)]
        [Persistent("cfld_name"), Size(30)]
        [VisibleInListView(true)]
        public string Name
        {
            get { return GetPropertyValue<string>("Name"); }
            set { SetPropertyValue("Name", string.IsNullOrEmpty(value) ? value : value.ToUpper()); }
        }

        [Persistent("cfld_head"), Size(25)]
        [ImmediatePostData()]
        [VisibleInListView(true)]
        public string Header
        {
            get { return GetPropertyValue<string>("Header"); }
            set
            {
                SetPropertyValue("Header", value);
                if (!IsLoading)
                {
                    if (string.IsNullOrWhiteSpace(Message))
                    {
                        Message = "Enter the " + value;
                    }
                }
            }
        }

        [Persistent("cdata_typ"), Size(1)]
        [ImmediatePostData(true)]
        [DevExpress.Xpo.DisplayName("Data Type")]
        [VisibleInListView(true)]
        [ValueConverter(typeof(EnumValueConverter<SystemDataType>))]
        public SystemDataType DataType
        {
            get { return GetPropertyValue<SystemDataType>("DataType"); }
            set
            {
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

        [RuleRangeAttribute("FieldWidthRangeRule", DefaultContexts.Save, 1, 512)]
        [Persistent("nfld_wdth")]
        [VisibleInListView(true)]
        public int Width
        {
            get { return GetPropertyValue<int>("Width"); }
            set { SetPropertyValue("Width", value); }
        }

        [RuleRangeAttribute("DecimalPlacesRangeRule", DefaultContexts.Save, 0, 16)]
        [Persistent("nfld_dec")]
        [VisibleInListView(false)]
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

        [NonPersistent(), Browsable(false)]
        public string ProductString { get { return Product.ToString(); } }

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

        [Association("Client-Field")]
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

        [Persistent("mvald_str"), Size(SizeAttribute.Unlimited)]
        [ImmediatePostData(true)]
        [VisibleInListView(false)]
        public string ValidString
        {
            get { return GetPropertyValue<string>("ValidString"); }
            set
            {
                SetPropertyValue("ValidString", value);
                if (!IsLoading)
                {
                    if (string.IsNullOrEmpty(value))
                    {
                        LocationOfValidFunc = "";
                    }
                }
            }
        }

        [Persistent("cvldfnloc"), Size(30)]
        [VisibleInListView(false)]
        public string LocationOfValidFunc
        {
            get { return GetPropertyValue<string>("LocationOfValidFunc"); }
            set { SetPropertyValue("LocationOfValidFunc", value); }
        }

        [Persistent("cfld_msg"), Size(50)]
        [VisibleInListView(false)]
        public string Message
        {
            get { return GetPropertyValue<string>("Message"); }
            set { SetPropertyValue("Message", value); }
        }

        [ImmediatePostData(true)]
        [Persistent("Lvldentry")]
        [VisibleInListView(true)]
        public bool IsCode
        {
            get { return GetPropertyValue<bool>("IsCode"); }
            set
            {
                SetPropertyValue("IsCode", value);
                if (!IsLoading)
                {
                    if (!value)
                    {
                        while (ValidEntriesCollection.Count > 0)
                            ValidEntriesCollection[0].Delete();
                        CodeIsEditable = false;
                    }
                }
            }
        }

        [NonPersistent()]
        [ImmediatePostData()]
        [VisibleInListView(false)]
        public bool CodeIsEditable
        {
            get { return GetPropertyValue<bool>("CodeIsEditable"); }
            set
            {
                SetPropertyValue("CodeIsEditable", value);
                if (!IsLoading)
                {
                    if (value)
                    {
                        CompanySequence = false;
                    }
                }
            }
        }

        [NonPersistent()]
        [ImmediatePostData()]
        [VisibleInListView(false)]
        public bool CompanySequence
        {
            get { return GetPropertyValue<bool>("CompanySequence"); }
            set
            {
                SetPropertyValue("CompanySequence", value);
                if (!IsLoading)
                {
                    if (value)
                    {
                        CodeIsEditable = false;
                    }
                }
            }
        }

        [Persistent("lrelated")]
        [VisibleInListView(false)]
        public bool IsRelated
        {
            get { return GetPropertyValue<bool>("IsRelated"); }
            set { SetPropertyValue("IsRelated", value); }
        }

        [Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        [Persistent("mfld_des")]
        public string Notes
        {
            get { return GetPropertyValue<string>("Notes"); }
            set { SetPropertyValue("Notes", value); }
        }

        [Persistent("mrltfields")]
        [Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string RelatedFields
        {
            get { return GetPropertyValue<string>("RelatedFields"); }
            set { SetPropertyValue("RelatedFields", value); }
        }

        [Size(SizeAttribute.Unlimited)]
        [Persistent("mventries")]
        [ImmediatePostData()]
        public string ValidEntries
        {
            get { return GetPropertyValue<string>("ValidEntries"); }
            set { SetPropertyValue("ValidEntries", value); }
        }

        [Size(SizeAttribute.Unlimited)]
        [Persistent("mcodeinfo")]
        private string CodeInformation
        {
            get
            {
                string value = "";
                if (CompanySequence)
                    value = "SEQPERCOMP";
                if (CodeIsEditable)
                    value = "EDITABLE";
                return value;

            }
            set
            {
                if (string.IsNullOrEmpty(value))
                {
                    CompanySequence = CodeIsEditable = false;
                }
                else if (value == "SEQPERCOMP")
                    CompanySequence = true;
                else if (value == "EDITABLE")
                    CodeIsEditable = true;
            }
        }

        [Persistent("LRLTFIELDS")]
        private bool HasRelatedFields
        {
            get
            {
                return !string.IsNullOrWhiteSpace(RelatedFields);
            }
        }

        [Association("Field-ValidEntry", typeof(FieldValidEntry)), Aggregated()]
        [DevExpress.Xpo.DisplayName("Valid Entries")]
        public XPCollection<FieldValidEntry> ValidEntriesCollection
        {
            get
            {
                return GetCollection<FieldValidEntry>("ValidEntriesCollection");
            }
        }

        [Association("Field-RelatedField", typeof(RelatedField)), Aggregated()]
        [DevExpress.Xpo.DisplayName("Related Fields")]
        public XPCollection<RelatedField> RelatedFieldsCollection
        {
            get
            {
                return GetCollection<RelatedField>("RelatedFieldsCollection");
            }
        }

        private XPCollection<FileField> _UsedBy;
        [NonPersistent()]
        public XPCollection<FileField> UsedBy
        {
            get
            {
                if (_UsedBy == null)
                {
                    _UsedBy = new XPCollection<FileField>(Session);
                    _UsedBy.BindingBehavior = CollectionBindingBehavior.AllowNone;
                    _UsedBy.CriteriaString = "Field.Name = '" + Name + "'";
                }
                return _UsedBy;
            }
        }


        protected override void OnLoaded()
        {
            base.OnLoaded();
            ValidEntriesCollection.CollectionChanged += new XPCollectionChangedEventHandler(ValidEntriesCollection_CollectionChanged);
            RelatedFieldsCollection.CollectionChanged += new XPCollectionChangedEventHandler(RelatedFieldsCollection_CollectionChanged);
        }

        void RelatedFieldsCollection_CollectionChanged(object sender, XPCollectionChangedEventArgs e)
        {
            if (e.CollectionChangedType == XPCollectionChangedType.AfterRemove)
            {
                //   Ticket = string.IsNullOrEmpty(Ticket) ? "N/A" : Ticket;
                RelatedFields = ReCalculateRelatedFieldsString();
                //  if (BO.ValidateWithoutTicket(this, ContextIdentifier.Save))
                //      Session.CommitTransaction();
            }
        }

        void ValidEntriesCollection_CollectionChanged(object sender, XPCollectionChangedEventArgs e)
        {
            if (e.CollectionChangedType == XPCollectionChangedType.AfterRemove)
            {
                //  Ticket = string.IsNullOrEmpty(Ticket) ? "N/A" : Ticket;
                ValidEntries = ReCalculateValidEntries();
                //  if (BO.ValidateWithoutTicket(this, ContextIdentifier.Save))
                //      Session.CommitTransaction();
            }
        }

        public string ReCalculateRelatedFieldsString()
        {
            string relatedFieldsString = "";
            if (RelatedFieldsCollection != null && RelatedFieldsCollection.Count > 0)
            {
                foreach (RelatedField relatedField in RelatedFieldsCollection)
                    if (!relatedField.IsDeleted)
                        relatedFieldsString += (relatedField.IsMandatory ? "$" : "") + relatedField.Field.Name + "|";
                relatedFieldsString = relatedFieldsString.EndsWith("|") ? relatedFieldsString.Remove(relatedFieldsString.Length - 1) : relatedFieldsString;
            }
            return relatedFieldsString;
        }

        public XPCollection<RelatedField> ReCalculateRelatedFieldsCollection(string relatedFieldsString)
        {
            XPCollection<RelatedField> RelatedFieldCollection = new XPCollection<RelatedField>(Session);
            if (!string.IsNullOrWhiteSpace(relatedFieldsString))
            {
                string[] relatedfieldsArray = relatedFieldsString.Split(new string[] { "|" }, StringSplitOptions.RemoveEmptyEntries);
                foreach (string relatedfieldArrayItem in relatedfieldsArray)
                {
                    RelatedField relatedFieldObj = new RelatedField(this.Session);
                    relatedFieldObj.ParentField = this;
                    relatedFieldObj.IsMandatory = relatedfieldArrayItem.StartsWith("$");
                    relatedFieldObj.Field = Session.GetObjectByKey<Field>(relatedFieldObj.IsMandatory ? relatedfieldArrayItem.Remove(0, 1) : relatedfieldArrayItem);
                    RelatedFieldCollection.Add(relatedFieldObj);
                }
            }
            return RelatedFieldCollection;
        }


        public XPCollection<FieldValidEntry> ReCalculateValidEntriesCollection(string ValidEntriesString)
        {
            XPCollection<FieldValidEntry> ValidEntryCollection = new XPCollection<FieldValidEntry>(Session);
            if (!string.IsNullOrWhiteSpace(ValidEntriesString))
            {
                string[] ValidEntryArray = ValidEntriesString.Split(new string[] { "~" }, StringSplitOptions.RemoveEmptyEntries);
                if (ValidEntryArray.Length == 2)
                {
                    string[] ValidEntryNamesArray = ValidEntryArray[0].Split(new string[] { "|" }, StringSplitOptions.RemoveEmptyEntries);
                    string[] ValidEntryCodesArray = ValidEntryArray[1].Split(new string[] { "|" }, StringSplitOptions.RemoveEmptyEntries);
                    for (int i = 0; i < ValidEntryNamesArray.Length; i++)
                    {
                        FieldValidEntry collectionItem = new FieldValidEntry(Session);
                        collectionItem.Field = this;
                        collectionItem.Name = ValidEntryNamesArray[i];
                        collectionItem.Code = ValidEntryCodesArray[i];
                        ValidEntryCollection.Add(collectionItem);
                    }
                }
            }
            return ValidEntryCollection;
        }

        public string ReCalculateValidEntries()
        {
            string validEntriesString = "";
            if (ValidEntriesCollection != null && ValidEntriesCollection.Count > 0)
            {
                foreach (FieldValidEntry validEntry in ValidEntriesCollection)
                    if (!validEntry.IsDeleted)
                        validEntriesString += validEntry.Name + "|";
                validEntriesString = validEntriesString.EndsWith("|") ? validEntriesString.Remove(validEntriesString.Length - 1) : validEntriesString;

                validEntriesString += "~";

                foreach (FieldValidEntry validEntry in ValidEntriesCollection)
                    if (!validEntry.IsDeleted)
                        validEntriesString += validEntry.Code + "|";
                validEntriesString = validEntriesString.EndsWith("|") ? validEntriesString.Remove(validEntriesString.Length - 1) : validEntriesString;
            }
            return validEntriesString;
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

        [NonPersistent(),Browsable(false)]
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

        public override void AfterConstruction()
        {
            base.AfterConstruction();
            if (Client == null)
                Client = Session.GetObjectByKey<Sys_Client>("ARIA");
        }

        public override string ToString()
        {
            return Name;
        }

    }
}