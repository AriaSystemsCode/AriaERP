using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    public class Registeration : XPLiteObject
    {
        int fCourseID;
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public int CourseID
        {
            get { return fCourseID; }
            set { SetPropertyValue<int>("CourseID", ref fCourseID, value); }
        }
        string fName;
        [Size(60)]
        public string Name
        {
            get { return fName; }
            set { SetPropertyValue<string>("Name", ref fName, value); }
        }
        string fTitle;
        [Size(60)]
        public string Title
        {
            get { return fTitle; }
            set { SetPropertyValue<string>("Title", ref fTitle, value); }
        }
        string fCompany;
        [Size(60)]
        public string Company
        {
            get { return fCompany; }
            set { SetPropertyValue<string>("Company", ref fCompany, value); }
        }
        string fPhone;
        [Size(60)]
        public string Phone
        {
            get { return fPhone; }
            set { SetPropertyValue<string>("Phone", ref fPhone, value); }
        }
        string fFax;
        [Size(60)]
        public string Fax
        {
            get { return fFax; }
            set { SetPropertyValue<string>("Fax", ref fFax, value); }
        }
        string fEmail;
        [Size(200)]
        public string Email
        {
            get { return fEmail; }
            set { SetPropertyValue<string>("Email", ref fEmail, value); }
        }
        string fAdd1;
        [Size(60)]
        public string Add1
        {
            get { return fAdd1; }
            set { SetPropertyValue<string>("Add1", ref fAdd1, value); }
        }
        string fAdd2;
        [Size(60)]
        public string Add2
        {
            get { return fAdd2; }
            set { SetPropertyValue<string>("Add2", ref fAdd2, value); }
        }
        string fCity;
        [Size(60)]
        public string City
        {
            get { return fCity; }
            set { SetPropertyValue<string>("City", ref fCity, value); }
        }
        string fState;
        [Size(60)]
        public string State
        {
            get { return fState; }
            set { SetPropertyValue<string>("State", ref fState, value); }
        }
        string fZip;
        [Size(60)]
        public string Zip
        {
            get { return fZip; }
            set { SetPropertyValue<string>("Zip", ref fZip, value); }
        }
        string fCountry;
        [Size(60)]
        public string Country
        {
            get { return fCountry; }
            set { SetPropertyValue<string>("Country", ref fCountry, value); }
        }
        DateTime fCourseDate;
        public DateTime CourseDate
        {
            get { return fCourseDate; }
            set { SetPropertyValue<DateTime>("CourseDate", ref fCourseDate, value); }
        }
        int fRowID;
        [Key(true)]
        public int RowID
        {
            get { return fRowID; }
            set { SetPropertyValue<int>("RowID", ref fRowID, value); }
        }
        public Registeration(Session session) : base(session) { }
        public Registeration() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }

}
