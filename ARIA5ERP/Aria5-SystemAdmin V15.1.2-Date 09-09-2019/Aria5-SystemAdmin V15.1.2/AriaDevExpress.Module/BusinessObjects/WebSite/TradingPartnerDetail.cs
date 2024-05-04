using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;


namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    [DevExpress.Xpo.Custom("Caption", "Detail")]
    [Persistent("sycedipd")]
    public class TradingPartnerDetail : XPLiteObject
    {
        string fcpartcode;
        [Size(6)]
        public string cpartcode
        {
            get { return fcpartcode; }
            set { SetPropertyValue<string>("cpartcode", ref fcpartcode, value); }
        }
        string fceditrntyp;
        [Size(3)]
        public string ceditrntyp
        {
            get { return fceditrntyp; }
            set { SetPropertyValue<string>("ceditrntyp", ref fceditrntyp, value); }
        }
        char fctranactv;
        public char ctranactv
        {
            get { return fctranactv; }
            set { SetPropertyValue<char>("ctranactv", ref fctranactv, value); }
        }
        string fcversion;
        [Size(12)]
        public string cversion
        {
            get { return fcversion; }
            set { SetPropertyValue<string>("cversion", ref fcversion, value); }
        }
        string fcpartqual;
        [Size(2)]
        public string cpartqual
        {
            get { return fcpartqual; }
            set { SetPropertyValue<string>("cpartqual", ref fcpartqual, value); }
        }
        string fcpartid;
        [Size(15)]
        public string cpartid
        {
            get { return fcpartid; }
            set { SetPropertyValue<string>("cpartid", ref fcpartid, value); }
        }
        string fcmapset;
        [Size(3)]
        public string cmapset
        {
            get { return fcmapset; }
            set { SetPropertyValue<string>("cmapset", ref fcmapset, value); }
        }
        string fcpartgsid;
        [Size(15)]
        public string cpartgsid
        {
            get { return fcpartgsid; }
            set { SetPropertyValue<string>("cpartgsid", ref fcpartgsid, value); }
        }
        char fctrantype;
        public char ctrantype
        {
            get { return fctrantype; }
            set { SetPropertyValue<char>("ctrantype", ref fctrantype, value); }
        }
        string fCNETWORK;
        [Size(6)]
        public string CNETWORK
        {
            get { return fCNETWORK; }
            set { SetPropertyValue<string>("CNETWORK", ref fCNETWORK, value); }
        }
        int fnRevision;
        public int nRevision
        {
            get { return fnRevision; }
            set { SetPropertyValue<int>("nRevision", ref fnRevision, value); }
        }
        int fnTrnRvsnNo;
        public int nTrnRvsnNo
        {
            get { return fnTrnRvsnNo; }
            set { SetPropertyValue<int>("nTrnRvsnNo", ref fnTrnRvsnNo, value); }
        }
        DateTime fdIssueDate;
        public DateTime dIssueDate
        {
            get { return fdIssueDate; }
            set { SetPropertyValue<DateTime>("dIssueDate", ref fdIssueDate, value); }
        }
        DateTime fdTPIssDate;
        public DateTime dTPIssDate
        {
            get { return fdTPIssDate; }
            set { SetPropertyValue<DateTime>("dTPIssDate", ref fdTPIssDate, value); }
        }
        Guid frowguid;
        [Key(true), Persistent()]
        private Guid rowguid
        {
            get { return frowguid; }
            set { SetPropertyValue<Guid>("rowguid", ref frowguid, value); }
        }
        string fCgdlnlnk;
        [Size(250)]
        public string Cgdlnlnk
        {
            get { return fCgdlnlnk; }
            set { SetPropertyValue<string>("Cgdlnlnk", ref fCgdlnlnk, value); }
        }
        string fCGDattch;
        [Size(10)]
        public string CGDattch
        {
            get { return fCGDattch; }
            set { SetPropertyValue<string>("CGDattch", ref fCGDattch, value); }
        }
        string fcgdrel;
        [Size(50)]
        public string cgdrel
        {
            get { return fcgdrel; }
            set { SetPropertyValue<string>("cgdrel", ref fcgdrel, value); }
        }
        string fcgdweb;
        [Size(250)]
        public string cgdweb
        {
            get { return fcgdweb; }
            set { SetPropertyValue<string>("cgdweb", ref fcgdweb, value); }
        }
        public TradingPartnerDetail(Session session) : base(session) { }
        public TradingPartnerDetail() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }



}