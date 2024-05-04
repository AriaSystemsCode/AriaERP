using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    public class SiteMap : XPLiteObject
    {
        int fID;
        [Key]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public int ID
        {
            get { return fID; }
            set { SetPropertyValue<int>("ID", ref fID, value); }
        }
        int fParentID;
        public int ParentID
        {
            get { return fParentID; }
            set { SetPropertyValue<int>("ParentID", ref fParentID, value); }
        }
        string fTitle;
        [Size(200)]
        public string Title
        {
            get { return fTitle; }
            set { SetPropertyValue<string>("Title", ref fTitle, value); }
        }
        string fURL;
        [Size(200)]
        public string URL
        {
            get { return fURL; }
            set { SetPropertyValue<string>("URL", ref fURL, value); }
        }
        public SiteMap(Session session) : base(session) { }
        public SiteMap() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }


}
