using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.OneTouchAway
{
    public class OTAWAYEventAttendee : XPLiteObject
    {
        OTAWAYEvent fEventID;
        [Association("OTAWAYEventAttendeeee")]
        [DisplayName("Title")]
        public OTAWAYEvent EventID
        {
            get { return fEventID; }
            set { SetPropertyValue<OTAWAYEvent>("EventID", ref fEventID, value); }
        }

        string fFirstName;
        [Size(150)]
        public string FirstName
        {
            get { return fFirstName; }
            set { SetPropertyValue<string>("FirstName", ref fFirstName, value); }
        }
        string fLastName;
        [Size(150)]
        public string LastName
        {
            get { return fLastName; }
            set { SetPropertyValue<string>("LastName", ref fLastName, value); }
        }
        string fTitle;
        [Size(150)]
        public string Title
        {
            get { return fTitle; }
            set { SetPropertyValue<string>("Title", ref fTitle, value); }
        }
        string fEmail;
        [Size(300)]
        public string Email
        {
            get { return fEmail; }
            set { SetPropertyValue<string>("Email", ref fEmail, value); }
        }
        string fCompanyName;
        [Size(150)]
        public string CompanyName
        {
            get { return fCompanyName; }
            set { SetPropertyValue<string>("CompanyName", ref fCompanyName, value); }
        }
        string fCountry;
        [Size(50)]
        public string Country
        {
            get { return fCountry; }
            set { SetPropertyValue<string>("Country", ref fCountry, value); }
        }

        DateTime fRegisterDatetime;
        public DateTime RegisterDatetime
        {
            get { return fRegisterDatetime; }
            set { SetPropertyValue<DateTime>("RegisterDatetime", ref fRegisterDatetime, value); }
        }

        bool fAttended;
        public bool Attended
        {
            get { return fAttended; }
            set { SetPropertyValue<bool>("Attended", ref fAttended, value); }
        }

        int fEventAttendeeID;
        [Key(true)]
        public int EventAttendeeID
        {
            get { return fEventAttendeeID; }
            set { SetPropertyValue<int>("EventAttendeeID", ref fEventAttendeeID, value); }
        }
        public OTAWAYEventAttendee(Session session) : base(session) { }
        public OTAWAYEventAttendee() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
