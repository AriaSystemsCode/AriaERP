using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    public class Register : XPLiteObject
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
        string fName;
        public string Name
        {
            get { return fName; }
            set { SetPropertyValue<string>("Name", ref fName, value); }
        }
        string fTitle;
        [Size(50)]
        public string Title
        {
            get { return fTitle; }
            set { SetPropertyValue<string>("Title", ref fTitle, value); }
        }
        string fComapny;
        [Size(50)]
        public string Comapny
        {
            get { return fComapny; }
            set { SetPropertyValue<string>("Comapny", ref fComapny, value); }
        }
        string fTelephone;
        [Size(50)]
        public string Telephone
        {
            get { return fTelephone; }
            set { SetPropertyValue<string>("Telephone", ref fTelephone, value); }
        }
        string fFax;
        [Size(50)]
        public string Fax
        {
            get { return fFax; }
            set { SetPropertyValue<string>("Fax", ref fFax, value); }
        }
        string fEmail;
        [Size(50)]
        public string Email
        {
            get { return fEmail; }
            set { SetPropertyValue<string>("Email", ref fEmail, value); }
        }
        string fAddress1;
        [Size(200)]
        public string Address1
        {
            get { return fAddress1; }
            set { SetPropertyValue<string>("Address1", ref fAddress1, value); }
        }
        string fAddress2;
        [Size(200)]
        public string Address2
        {
            get { return fAddress2; }
            set { SetPropertyValue<string>("Address2", ref fAddress2, value); }
        }
        string fCity;
        [Size(30)]
        public string City
        {
            get { return fCity; }
            set { SetPropertyValue<string>("City", ref fCity, value); }
        }
        string fState;
        [Size(30)]
        public string State
        {
            get { return fState; }
            set { SetPropertyValue<string>("State", ref fState, value); }
        }
        string fZip;
        [Size(20)]
        public string Zip
        {
            get { return fZip; }
            set { SetPropertyValue<string>("Zip", ref fZip, value); }
        }
        string fCountry;
        [Size(20)]
        public string Country
        {
            get { return fCountry; }
            set { SetPropertyValue<string>("Country", ref fCountry, value); }
        }
        string fCourseId;
        [Size(50)]
        public string CourseId
        {
            get { return fCourseId; }
            set { SetPropertyValue<string>("CourseId", ref fCourseId, value); }
        }
        DateTime fRegisterDate;
        public DateTime RegisterDate
        {
            get { return fRegisterDate; }
            set { SetPropertyValue<DateTime>("RegisterDate", ref fRegisterDate, value); }
        }
        int fCoursePeriodId;
        public int CoursePeriodId
        {
            get { return fCoursePeriodId; }
            set { SetPropertyValue<int>("CoursePeriodId", ref fCoursePeriodId, value); }
        }
        public Register(Session session) : base(session) { }
        public Register() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
