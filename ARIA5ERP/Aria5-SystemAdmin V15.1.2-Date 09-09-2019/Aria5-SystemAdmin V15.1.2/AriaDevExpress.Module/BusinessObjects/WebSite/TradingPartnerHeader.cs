using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
   [DevExpress.Xpo.Custom("Caption", "Header")]
    [Persistent("sycediph")]
    [FriendlyKeyProperty("cpartcode")]
    public class TradingPartnerHeader : XPLiteObject
    {
        string fcpartcode;
        [Size(6)]
        public string cpartcode
        {
            get { return fcpartcode; }
            set { SetPropertyValue<string>("cpartcode", ref fcpartcode, value); }
        }
        string fcParentCode;
        [Size(6)]
        public string cParentCode
        {
            get { return fcParentCode; }
            set { SetPropertyValue<string>("cParentCode", ref fcParentCode, value); }
        }

        string fcparentname;
        [Size(30)]
        public string cParentName
        {
            get { return fcparentname; }
            set { SetPropertyValue<string>("cParentName", ref fcparentname, value); }
        }

        string fcpartname;
        [Size(30)]
        public string cpartname
        {
            get { return fcpartname; }
            set { SetPropertyValue<string>("cpartname", ref fcpartname, value); }
        }
        string fcintchgver;
        [Size(5)]
        public string cintchgver
        {
            get { return fcintchgver; }
            set { SetPropertyValue<string>("cintchgver", ref fcintchgver, value); }
        }
        string fcversion;
        [Size(12)]
        public string cversion
        {
            get { return fcversion; }
            set { SetPropertyValue<string>("cversion", ref fcversion, value); }
        }
        string fcnetwork;
        [Size(6)]
        public string cnetwork
        {
            get { return fcnetwork; }
            set { SetPropertyValue<string>("cnetwork", ref fcnetwork, value); }
        }
        string fcfieldsep;
        [Size(10)]
        public string cfieldsep
        {
            get { return fcfieldsep; }
            set { SetPropertyValue<string>("cfieldsep", ref fcfieldsep, value); }
        }
        string fclinesep;
        [Size(10)]
        public string clinesep
        {
            get { return fclinesep; }
            set { SetPropertyValue<string>("clinesep", ref fclinesep, value); }
        }
        string fcasnlbl1;
        [Size(3)]
        public string casnlbl1
        {
            get { return fcasnlbl1; }
            set { SetPropertyValue<string>("casnlbl1", ref fcasnlbl1, value); }
        }
        string fcasnlbl2;
        [Size(3)]
        public string casnlbl2
        {
            get { return fcasnlbl2; }
            set { SetPropertyValue<string>("casnlbl2", ref fcasnlbl2, value); }
        }
        char fccrtntype;
        public char ccrtntype
        {
            get { return fccrtntype; }
            set { SetPropertyValue<char>("ccrtntype", ref fccrtntype, value); }
        }
        bool flpltshp;
        public bool lpltshp
        {
            get { return flpltshp; }
            set { SetPropertyValue<bool>("lpltshp", ref flpltshp, value); }
        }
        string fcpltlbl;
        [Size(3)]
        public string cpltlbl
        {
            get { return fcpltlbl; }
            set { SetPropertyValue<string>("cpltlbl", ref fcpltlbl, value); }
        }
        char fcIsaCnStId;
        public char cIsaCnStId
        {
            get { return fcIsaCnStId; }
            set { SetPropertyValue<char>("cIsaCnStId", ref fcIsaCnStId, value); }
        }
        char fcSubEleSep;
        public char cSubEleSep
        {
            get { return fcSubEleSep; }
            set { SetPropertyValue<char>("cSubEleSep", ref fcSubEleSep, value); }
        }
        Guid frowguid;
        [Key(true), Persistent()]
        private Guid rowguid
        {
            get { return frowguid; }
            set { SetPropertyValue<Guid>("rowguid", ref frowguid, value); }
        }
        public TradingPartnerHeader(Session session) : base(session) { }
        public TradingPartnerHeader() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }



}
