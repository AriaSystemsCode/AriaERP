using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.HR
{
    [Persistent("Address_T")]
    public class HR_Address1 : XPLiteObject
    {
        Int32 fAddress_KEY;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        [DisplayName("Address")]
        public Int32 Address_KEY
        {
            get { return fAddress_KEY; }
            set { SetPropertyValue<Int32>("Address_KEY", ref fAddress_KEY, value); }
        }

        Country1 fCountry_KEY;
        [DisplayName("Country")]
        public Country1 Country_KEY
        {
            get { return fCountry_KEY; }
            set { SetPropertyValue<Country1>("Country_KEY", ref fCountry_KEY, value); }
        }
        string fAddress1;
        [Size(30)]
        public string Address1
        {
            get { return fAddress1; }
            set { SetPropertyValue<string>("Address1", ref fAddress1, value); }
        }
        string fAddress2;
        [Size(30)]
        public string Address2
        {
            get { return fAddress2; }
            set { SetPropertyValue<string>("Address2", ref fAddress2, value); }
        }
        string fAddress3;
        [Size(30)]
        public string Address3
        {
            get { return fAddress3; }
            set { SetPropertyValue<string>("Address3", ref fAddress3, value); }
        }
        string fAddress4;
        [Size(30)]
        public string Address4
        {
            get { return fAddress4; }
            set { SetPropertyValue<string>("Address4", ref fAddress4, value); }
        }
        string fAddress5;
        [Size(30)]
        public string Address5
        {
            get { return fAddress5; }
            set { SetPropertyValue<string>("Address5", ref fAddress5, value); }
        }
        string fAddress6;
        [Size(20)]
        public string Address6
        {
            get { return fAddress6; }
            set { SetPropertyValue<string>("Address6", ref fAddress6, value); }
        }
        string fDescription;
        [Size(200)]
        public string Description
        {
            get { return fDescription; }
            set { SetPropertyValue<string>("Description", ref fDescription, value); }
        }
        public HR_Address1(Session session) : base(session) { }
        public HR_Address1() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
