using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    public class Category : XPLiteObject
    {
        int fCategory_ID;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public int Category_ID
        {
            get { return fCategory_ID; }
            set { SetPropertyValue<int>("Category_ID", ref fCategory_ID, value); }
        }
        string fCategory_Desc;
        public string Category_Desc
        {
            get { return fCategory_Desc; }
            set { SetPropertyValue<string>("Category_Desc", ref fCategory_Desc, value); }
        }
        short fRowRefrence;
        public short RowRefrence
        {
            get { return fRowRefrence; }
            set { SetPropertyValue<short>("RowRefrence", ref fRowRefrence, value); }
        }
        short fColumnReference;
        public short ColumnReference
        {
            get { return fColumnReference; }
            set { SetPropertyValue<short>("ColumnReference", ref fColumnReference, value); }
        }
        string fMenuDesc;
        [Size(200)]
        public string MenuDesc
        {
            get { return fMenuDesc; }
            set { SetPropertyValue<string>("MenuDesc", ref fMenuDesc, value); }
        }
        public Category(Session session) : base(session) { }
        public Category() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
