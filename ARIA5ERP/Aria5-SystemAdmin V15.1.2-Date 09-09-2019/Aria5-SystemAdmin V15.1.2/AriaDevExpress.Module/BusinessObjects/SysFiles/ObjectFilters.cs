using System;
using DevExpress.Xpo;
using System.ComponentModel;
using DevExpress.Persistent.Validation;
using DevExpress.Persistent.Base;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [Persistent("SYDOBJCT_FILTERS")]
    public class ObjectFilters : XPLiteObject
    {
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

        [Persistent("cApp_Id")]
        [RuleRequiredField()]
        [VisibleInListView(true)]
        [Custom("LookupEditorMode", "AllItems")]
        public Module Module { get; set; }

        [Association("Object-ObjectFilters"), Browsable(false)]
        public Object Object { get; set; }

        [Persistent("cApObjNam"), Size(10), DevExpress.Xpo.DisplayName("cApObjNam")]
        public string cApObjNam { get { return Object.ID; } }


        [Persistent("nFld_No")]
        public int Number { get; set; }

        [VisibleInListView(true)]
        [Custom("LookupEditorMode", "AllItems")]
        public Sys_Client Client
        {
            get { return Object.Client; }
        }

        [ImmediatePostData(true), RuleRequiredField()]
        [VisibleInListView(true), Size(10)]
        [ValueConverter(typeof(EnumValueConverter<Products>))]
        public Products Product
        {
            get
            {
                return Object.Product;
            }
        }

        [VisibleInListView(true), Size(10), RuleRequiredField()]
        [ValueConverter(typeof(EnumValueConverter<DataBaseTypes>))]
        public DataBaseTypes Database
        {
            get
            {
                return Object.Database;
            }
        }

        //1
        [Persistent("mFld_name")]
        public string Name { get; set; }

        //2
        [Persistent("cFld_Type")]
        [ValueConverter(typeof(EnumValueConverter<FilterVariableType>))]
        public FilterVariableType Type { get; set; }

        //3
        [Persistent("cData_Type")]
        [ValueConverter(typeof(EnumValueConverter<SystemDataType>))]
        public SystemDataType DataType { get; set; }


        //4
        [Persistent("Is_True"), DevExpress.Xpo.DisplayName("Not")]
        public bool Not { get; set; }

        //5
        [Persistent("Operator")]
        [ValueConverter(typeof(EnumValueConverter<FilterOperator>))]
        public FilterOperator Operator { get; set; }


        //6
        public string Value { get; set; }


        //7
        [Persistent("Value_Type")]
        [ValueConverter(typeof(EnumValueConverter<FilterValueType>))]
        public FilterValueType ValueType { get; set; }


        [Persistent("Filter_Type")]
        [ValueConverter(typeof(EnumValueConverter<FilterType>))]
        [Size(1)]
        public FilterType FilterType { get; set; }


        public ObjectFilters()
            : base()
        {
            // This constructor is used when an object is loaded from a persistent storage.
            // Do not place any code here.
        }

        public ObjectFilters(Session session)
            : base(session)
        {
            // This constructor is used when an object is loaded from a persistent storage.
            // Do not place any code here.
        }

        protected override void OnSaved()
        {
            base.OnSaved();
            Object.RecalculateFilters();
            Object.Ticket = "N/A";
            if (BO.ValidateWithoutTicket(Object, ContextIdentifier.Save))
                Object.Session.CommitTransaction();
        }

        public override void AfterConstruction()
        {
            base.AfterConstruction();
            // Place here your initialization code.
        }


    }

}