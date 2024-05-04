using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.HR
{
    [Persistent("Document_Attachment_T")]
    [DevExpress.Xpo.Custom("Caption", "Paper")]
    public class HR_Attachment1 : XPLiteObject
    {
        Int32 fAttachment_KEY;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public Int32 Attachment_KEY
        {
            get { return fAttachment_KEY; }
            set { SetPropertyValue<Int32>("Attachment_KEY", ref fAttachment_KEY, value); }
        }

        FileData fOID;
        [Persistent("OID")]
        public FileData File
        {
            get { return fOID; }
            set { SetPropertyValue<FileData>("File", ref fOID, value); }
        }
        string fDescription;
        [Size(200)]
        public string Description
        {
            get { return fDescription; }
            set { SetPropertyValue<string>("Description", ref fDescription, value); }
        }

      
        Employee employee;
        [Association("Papers")]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public Employee Employee
        {
            get { return employee; }
            set { SetPropertyValue<Employee>("Employee", ref employee, value); }
        }
      
        public HR_Attachment1(Session session) : base(session) { }
        public HR_Attachment1() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }

    //public class Document_T : XPLiteObject
    //{
    //    long fDocument_KEY;
    //    [Key]
    //    public long Document_KEY
    //    {
    //        get { return fDocument_KEY; }
    //        set { SetPropertyValue<long>("Document_KEY", ref fDocument_KEY, value); }
    //    }
    //    long fAttachment_KEY;
    //    public long Attachment_KEY
    //    {
    //        get { return fAttachment_KEY; }
    //        set { SetPropertyValue<long>("Attachment_KEY", ref fAttachment_KEY, value); }
    //    }
    //    public Document_T(Session session) : base(session) { }
    //    public Document_T() : base(Session.DefaultSession) { }
    //    public override void AfterConstruction() { base.AfterConstruction(); }
    //}
}
