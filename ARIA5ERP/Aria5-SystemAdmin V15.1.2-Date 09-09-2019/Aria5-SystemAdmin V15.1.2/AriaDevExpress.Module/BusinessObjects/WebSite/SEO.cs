using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    public class SEO : XPLiteObject
    {
        int fID;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public int ID
        {
            get { return fID; }
            set { SetPropertyValue<int>("ID", ref fID, value); }
        }
        string fPageLink;
        [Size(50)]
        public string PageLink
        {
            get { return fPageLink; }
            set { SetPropertyValue<string>("PageLink", ref fPageLink, value); }
        }
        string fTitle;
        [Size(200)]
        public string Title
        {
            get { return fTitle; }
            set { SetPropertyValue<string>("Title", ref fTitle, value); }
        }
        string fDescription;
        [Size(1000)]
        public string Description
        {
            get { return fDescription; }
            set { SetPropertyValue<string>("Description", ref fDescription, value); }
        }
        string fKeyWords;
        [Size(1000)]
        public string KeyWords
        {
            get { return fKeyWords; }
            set { SetPropertyValue<string>("KeyWords", ref fKeyWords, value); }
        }
        public SEO(Session session) : base(session) { }
        public SEO() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
