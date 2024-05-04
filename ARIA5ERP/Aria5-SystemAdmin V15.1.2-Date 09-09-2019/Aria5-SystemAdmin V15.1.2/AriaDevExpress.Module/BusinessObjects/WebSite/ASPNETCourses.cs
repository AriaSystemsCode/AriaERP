using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    [DevExpress.Xpo.Custom("Caption", "Courses")]
    public class ASPNETCourses : XPLiteObject
    {
        string fCourseID;
        [Key]
        [Size(50)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public string CourseID
        {
            get { return fCourseID; }
            set { SetPropertyValue<string>("CourseID", ref fCourseID, value); }
        }
        string fCourseName;
        public string CourseName
        {
            get { return fCourseName; }
            set { SetPropertyValue<string>("CourseName", ref fCourseName, value); }
        }
        string fCost;
        [Size(10)]
        public string Cost
        {
            get { return fCost; }
            set { SetPropertyValue<string>("Cost", ref fCost, value); }
        }
        string fSubject;
        [Size(60)]
        public string Subject
        {
            get { return fSubject; }
            set { SetPropertyValue<string>("Subject", ref fSubject, value); }
        }
        string fOutline;
        [Size(500)]
        public string Outline
        {
            get { return fOutline; }
            set { SetPropertyValue<string>("Outline", ref fOutline, value); }
        }
        string fImage;
        [Size(60)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public string Image
        {
            get { return fImage; }
            set { SetPropertyValue<string>("Image", ref fImage, value); }
        }
        string fDuration;
        [Size(30)]
        public string Duration
        {
            get { return fDuration; }
            set { SetPropertyValue<string>("Duration", ref fDuration, value); }
        }

        string fcObjective;  
        [DisplayName("Objectives")]
        [Size(500)]
        public string cObjective
        {
            get { return fcObjective; }
            set { SetPropertyValue<string>("cObjective", ref fcObjective, value); }
        }
        Guid frowguid;
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public Guid rowguid
        {
            get { return frowguid; }
            set { SetPropertyValue<Guid>("rowguid", ref frowguid, value); }
        }
        string fIntendedAudience;
        [Size(500)]
        public string IntendedAudience
        {
            get { return fIntendedAudience; }
            set { SetPropertyValue<string>("IntendedAudience", ref fIntendedAudience, value); }
        }
        string fDataPrerequisites;
        [Size(500)]
        public string DataPrerequisites
        {
            get { return fDataPrerequisites; }
            set { SetPropertyValue<string>("DataPrerequisites", ref fDataPrerequisites, value); }
        }
        public ASPNETCourses(Session session) : base(session) { }
        public ASPNETCourses() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
