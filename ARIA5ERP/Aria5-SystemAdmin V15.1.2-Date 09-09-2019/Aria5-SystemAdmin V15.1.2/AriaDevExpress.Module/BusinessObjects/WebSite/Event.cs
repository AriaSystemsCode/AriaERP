using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
       [DevExpress.Xpo.Custom("Caption", "Event Schedule")]
    public class Event : XPLiteObject
    {
        int fEventID;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public int EventID
        {
            get { return fEventID; }
            set { SetPropertyValue<int>("EventID", ref fEventID, value); }
        }
        Subject fSubjectID;
        public Subject SubjectID
        {
            get { return fSubjectID; }
            set { SetPropertyValue<Subject>("SubjectID", ref fSubjectID, value); }
        }
        string fOutline;
        [Size(SizeAttribute.Unlimited)]
        public string Outline
        {
            get { return fOutline; }
            set { SetPropertyValue<string>("Outline", ref fOutline, value); }
        }

        EventStatus fStatus;
        public EventStatus Status
        {
            get { return fStatus; }
            set { SetPropertyValue<EventStatus>("Status", ref fStatus, value); }
        }

        string fInstructor;
        [Size(150)]
        public string Instructor
        {
            get { return fInstructor; }
            set { SetPropertyValue<string>("Instructor", ref fInstructor, value); }
        }
        DateTime fFromDatetime;
        [Custom("DisplayFormat", "{0: ddd, dd MMMM yyyy hh:mm:ss tt}")]
        [Custom("EditMask", "ddd, dd MMMM yyyy hh:mm:ss tt")]
        public DateTime FromDatetime
        {
            get { return fFromDatetime; }
            set { SetPropertyValue<DateTime>("FromDatetime", ref fFromDatetime, value); }
        }
        DateTime fToDatetime;
        [Custom("DisplayFormat", "{0: ddd, dd MMMM yyyy hh:mm:ss tt}")]
        [Custom("EditMask", "ddd, dd MMMM yyyy hh:mm:ss tt")]
        public DateTime ToDatetime
        {
            get { return fToDatetime; }
            set { SetPropertyValue<DateTime>("ToDatetime", ref fToDatetime, value); }
        }
        string fRecordedFile;
        [Size(200)]
        public string RecordedFile
        {
            get { return fRecordedFile; }
            set { SetPropertyValue<string>("RecordedFile", ref fRecordedFile, value); }
        }
        string fAudience;
        [Size(200)]
        public string Audience
        {
            get { return fAudience; }
            set { SetPropertyValue<string>("Audience", ref fAudience, value); }
        }
        string fTitle;
        [Size(250)]
        public string Title
        {
            get { return fTitle; }
            set { SetPropertyValue<string>("Title", ref fTitle, value); }
        }
        string fDescription;
        [Size(SizeAttribute.Unlimited)]
        public string Description
        {
            get { return fDescription; }
            set { SetPropertyValue<string>("Description", ref fDescription, value); }
        }


        [Association("EventAttendeeee", typeof(EventAttendee)), Aggregated]
        public XPCollection EventAttendeee
        {
            get { return GetCollection("EventAttendeee"); }
        }


        public Event(Session session) : base(session) { }
        public Event() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
