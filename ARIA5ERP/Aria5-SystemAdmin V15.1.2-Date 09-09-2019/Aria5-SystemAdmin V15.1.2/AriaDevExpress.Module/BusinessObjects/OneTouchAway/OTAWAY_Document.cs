using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.OneTouchAway
{
    public class OTAWAYDocument : XPLiteObject
    {
        int fDocID;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public int DocID
        {
            get { return fDocID; }
            set { SetPropertyValue<int>("DocID", ref fDocID, value); }
        }

        [Size(500)]
        [Persistent()]
        private string DocName
        {
            get
            {
                if (fUploadDoc != null)
                    return fUploadDoc.RealFileName;
                else return null;
            }
        }

        OTAWAYFileDataEx fUploadDoc;
        [Aggregated, ExpandObjectMembers(ExpandObjectMembers.Never)]
        public OTAWAYFileDataEx UploadDoc
        {
            get { return fUploadDoc; }
            set
            {
                SetPropertyValue<OTAWAYFileDataEx>("UploadDoc", ref fUploadDoc, value);
            }
        }

        string fDescription;
        [Size(250)]
        public string Description
        {
            get { return fDescription; }
            set { SetPropertyValue<string>("Description", ref fDescription, value); }
        }
        char fDocType;
        public char DocType
        {
            get { return fDocType; }
            set { SetPropertyValue<char>("DocType", ref fDocType, value); }
        }
        DateTime fUploadDate;
        public DateTime UploadDate
        {
            get { return fUploadDate; }
            set { SetPropertyValue<DateTime>("UploadDate", ref fUploadDate, value); }
        }
        DateTime fUploadTime;
        [Custom("DisplayFormat", "{0: ddd, dd MMMM yyyy hh:mm:ss tt}")]
        [Custom("EditMask", "ddd, dd MMMM yyyy hh:mm:ss tt")]
        public DateTime UploadTime
        {
            get { return fUploadTime; }
            set { SetPropertyValue<DateTime>("UploadTime", ref fUploadTime, value); }
        }
        string fUploadBy;
        [Size(50)]
        public string UploadBy
        {
            get { return fUploadBy; }
            set { SetPropertyValue<string>("UploadBy", ref fUploadBy, value); }
        }
        bool fAvailable;
        public bool Available
        {
            get { return fAvailable; }
            set { SetPropertyValue<bool>("Available", ref fAvailable, value); }
        }
        bool fDOF;
        public bool DOF
        {
            get { return fDOF; }
            set { SetPropertyValue<bool>("DOF", ref fDOF, value); }
        }
        public OTAWAYDocument(Session session) : base(session) { }
        public OTAWAYDocument()
            : base(Session.DefaultSession)
        {
        }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
