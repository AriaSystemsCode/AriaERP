using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.HR
{
    [Persistent("Country_T")]
    public class Country1 : XPLiteObject
    {
        long fCountry_KEY;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public long Country_KEY
        {
            get { return fCountry_KEY; }
            set { SetPropertyValue<long>("Country_KEY", ref fCountry_KEY, value); }
        }
        string fCountry_Name;
        [Size(100)]
        public string Country_Name
        {
            get { return fCountry_Name; }
            set { SetPropertyValue<string>("Country_Name", ref fCountry_Name, value); }
        }
        string fCountry_ISO2;
        [Size(2)]
        public string Country_ISO2
        {
            get { return fCountry_ISO2; }
            set { SetPropertyValue<string>("Country_ISO2", ref fCountry_ISO2, value); }
        }
        string fCountry_ISO3;
        [Size(3)]
        public string Country_ISO3
        {
            get { return fCountry_ISO3; }
            set { SetPropertyValue<string>("Country_ISO3", ref fCountry_ISO3, value); }
        }
        public Country1(Session session) : base(session) { }
        public Country1() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
