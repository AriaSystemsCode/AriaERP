using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
     [DevExpress.Xpo.Custom("Caption", "Event")]
    public class Subject : XPLiteObject
    {
        int fSubjectID;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public int SubjectID
        {
            get { return fSubjectID; }
            set { SetPropertyValue<int>("SubjectID", ref fSubjectID, value); }
        }
        string fOutline;
        [Size(400)]
        public string Outline
        {
            get { return fOutline; }
            set { SetPropertyValue<string>("Outline", ref fOutline, value); }
        }
        string fDescription;
        [Size(SizeAttribute.Unlimited)]
        public string Description
        {
            get { return fDescription; }
            set { SetPropertyValue<string>("Description", ref fDescription, value); }
        }

        FileDataEx fImagePath;
        public FileDataEx ImagePath
        {
            get { return fImagePath; }
            set { SetPropertyValue<FileDataEx>("ImagePath", ref fImagePath, value); }
        }

        [Persistent()]
        private string ImageURL
        {
            get
            {
                if (ImagePath != null)
                    return ImagePath.RealFileName;
                return null;
            }
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
        public Subject(Session session) : base(session) { }
        public Subject() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
