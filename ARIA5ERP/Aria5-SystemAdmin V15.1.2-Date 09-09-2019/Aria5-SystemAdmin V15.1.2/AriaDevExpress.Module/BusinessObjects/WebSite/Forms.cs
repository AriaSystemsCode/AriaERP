using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    [DevExpress.Xpo.Custom("Caption", "Requests")]
    public class Forms : XPLiteObject
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
        string fFormName;
        [Size(50)]
        public string FormName
        {
            get { return fFormName; }
            set { SetPropertyValue<string>("FormName", ref fFormName, value); }
        }
        string fFirstName;
        [Size(50)]
        public string FirstName
        {
            get { return fFirstName; }
            set { SetPropertyValue<string>("FirstName", ref fFirstName, value); }
        }
        string fLastName;
        [Size(50)]
        public string LastName
        {
            get { return fLastName; }
            set { SetPropertyValue<string>("LastName", ref fLastName, value); }
        }
        string fTitle;
        [Size(50)]
        public string Title
        {
            get { return fTitle; }
            set { SetPropertyValue<string>("Title", ref fTitle, value); }
        }
        string fPhone;
        [Size(50)]
        public string Phone
        {
            get { return fPhone; }
            set { SetPropertyValue<string>("Phone", ref fPhone, value); }
        }
        string fEmail;
        [Size(50)]
        public string Email
        {
            get { return fEmail; }
            set { SetPropertyValue<string>("Email", ref fEmail, value); }
        }
        string fCompanyName;
        [Size(50)]
        public string CompanyName
        {
            get { return fCompanyName; }
            set { SetPropertyValue<string>("CompanyName", ref fCompanyName, value); }
        }
        string fCompanySize;
        [Size(50)]
        [VisibleInListView(false)]
        public string CompanySize
        {
            get { return fCompanySize; }
            set { SetPropertyValue<string>("CompanySize", ref fCompanySize, value); }
        }
        string fAddress1;
        [Size(50)]
        [VisibleInListView(false)]
        public string Address1
        {
            get { return fAddress1; }
            set { SetPropertyValue<string>("Address1", ref fAddress1, value); }
        }
        string fAddress2;
        [Size(50)]
        [VisibleInListView(false)]
        public string Address2
        {
            get { return fAddress2; }
            set { SetPropertyValue<string>("Address2", ref fAddress2, value); }
        }
        string fCity;
        [Size(50)]
        [VisibleInListView(false)]
        public string City
        {
            get { return fCity; }
            set { SetPropertyValue<string>("City", ref fCity, value); }
        }
        string fState;
        [Size(50)]
        [VisibleInListView(false)]
        public string State
        {
            get { return fState; }
            set { SetPropertyValue<string>("State", ref fState, value); }
        }
        string fCountry;
        [Size(50)]
        [VisibleInListView(false)]
        public string Country
        {
            get { return fCountry; }
            set { SetPropertyValue<string>("Country", ref fCountry, value); }
        }
        string fZip;
        [Size(50)]
        [VisibleInListView(false)]
        public string Zip
        {
            get { return fZip; }
            set { SetPropertyValue<string>("Zip", ref fZip, value); }
        }
        string fBusinessType;
        [Size(50)]
        [VisibleInListView(false)]
        public string BusinessType
        {
            get { return fBusinessType; }
            set { SetPropertyValue<string>("BusinessType", ref fBusinessType, value); }
        }
        string fProductCategory;
        [Size(50)]
        [VisibleInListView(false)]
        public string ProductCategory
        {
            get { return fProductCategory; }
            set { SetPropertyValue<string>("ProductCategory", ref fProductCategory, value); }
        }
        string fProductInterest;
        [Size(50)]
        [VisibleInListView(false)]
        public string ProductInterest
        {
            get { return fProductInterest; }
            set { SetPropertyValue<string>("ProductInterest", ref fProductInterest, value); }
        }
        string fComments;
        [Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string Comments
        {
            get { return fComments; }
            set { SetPropertyValue<string>("Comments", ref fComments, value); }
        }
        string fURL;
        [Size(500)]
        [VisibleInListView(false)]
        public string URL
        {
            get { return fURL; }
            set { SetPropertyValue<string>("URL", ref fURL, value); }
        }
        string fHearAboutUs;
        [Size(50)]
        [VisibleInListView(false)]
        public string HearAboutUs
        {
            get { return fHearAboutUs; }
            set { SetPropertyValue<string>("HearAboutUs", ref fHearAboutUs, value); }
        }
        string fcustomfield1;
        [Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string customfield1
        {
            get { return fcustomfield1; }
            set { SetPropertyValue<string>("customfield1", ref fcustomfield1, value); }
        }
        string fcustomfield2;
        [Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string customfield2
        {
            get { return fcustomfield2; }
            set { SetPropertyValue<string>("customfield2", ref fcustomfield2, value); }
        }
        string fcustomfield3;
        [Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string customfield3
        {
            get { return fcustomfield3; }
            set { SetPropertyValue<string>("customfield3", ref fcustomfield3, value); }
        }
        string fcustomfield4;
        [Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string customfield4
        {
            get { return fcustomfield4; }
            set { SetPropertyValue<string>("customfield4", ref fcustomfield4, value); }
        }
        string fcustomfield5;
        [Size(SizeAttribute.Unlimited)]
        [VisibleInListView(false)]
        public string customfield5
        {
            get { return fcustomfield5; }
            set { SetPropertyValue<string>("customfield5", ref fcustomfield5, value); }
        }
        [Custom("DisplayFormat", "{0:dd/MM/yyyy}")]
        public DateTime EnteredDate { get; set; }
        public Forms(Session session) : base(session) { }
        public Forms() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
