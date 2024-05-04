using System;
using System.ComponentModel;

using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;


namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    [DevExpress.Xpo.Custom("Caption", "Transaction")]
    [Persistent("syceditr")]
    public class TradingPartnerTransaction : XPLiteObject
    {
        string fceditrncod;
        [Size(2)]
        public string ceditrncod
        {
            get { return fceditrncod; }
            set { SetPropertyValue<string>("ceditrncod", ref fceditrncod, value); }
        }
        string fceditrntyp;
        [Size(3)]
        public string ceditrntyp
        {
            get { return fceditrntyp; }
            set { SetPropertyValue<string>("ceditrntyp", ref fceditrntyp, value); }
        }
        string fceditrnnam;
        [Size(40)]
        public string ceditrnnam
        {
            get { return fceditrnnam; }
            set { SetPropertyValue<string>("ceditrnnam", ref fceditrnnam, value); }
        }
        char fctrantype;
        public char ctrantype
        {
            get { return fctrantype; }
            set { SetPropertyValue<char>("ctrantype", ref fctrantype, value); }
        }
        string fmrclassnam;
        [Size(SizeAttribute.Unlimited)]
        public string mrclassnam
        {
            get { return fmrclassnam; }
            set { SetPropertyValue<string>("mrclassnam", ref fmrclassnam, value); }
        }
        string fmsclassnam;
        [Size(SizeAttribute.Unlimited)]
        public string msclassnam
        {
            get { return fmsclassnam; }
            set { SetPropertyValue<string>("msclassnam", ref fmsclassnam, value); }
        }
        string fcicrclass;
        [Size(30)]
        public string cicrclass
        {
            get { return fcicrclass; }
            set { SetPropertyValue<string>("cicrclass", ref fcicrclass, value); }
        }
        string fcicsclass;
        [Size(30)]
        public string cicsclass
        {
            get { return fcicsclass; }
            set { SetPropertyValue<string>("cicsclass", ref fcicsclass, value); }
        }
        string fcbarmodule;
        [Size(60)]
        public string cbarmodule
        {
            get { return fcbarmodule; }
            set { SetPropertyValue<string>("cbarmodule", ref fcbarmodule, value); }
        }
        Guid frowguid;
        [Key(true), Persistent()]
        private Guid rowguid
        {
            get { return frowguid; }
            set { SetPropertyValue<Guid>("rowguid", ref frowguid, value); }
        }
        int fnRevision;
        public int nRevision
        {
            get { return fnRevision; }
            set { SetPropertyValue<int>("nRevision", ref fnRevision, value); }
        }
        string fcfacttrn;
        [Size(6)]
        public string cfacttrn
        {
            get { return fcfacttrn; }
            set { SetPropertyValue<string>("cfacttrn", ref fcfacttrn, value); }
        }
        public TradingPartnerTransaction(Session session) : base(session) { }
        public TradingPartnerTransaction() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
