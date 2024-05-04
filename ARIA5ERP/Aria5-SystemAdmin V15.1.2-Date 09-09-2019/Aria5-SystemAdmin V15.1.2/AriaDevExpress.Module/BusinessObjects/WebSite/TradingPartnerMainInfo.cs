using System;
using System.Linq;
using DevExpress.Xpo;

using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using System.Collections.Generic;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    public class TradingPartnerMainInfo : XPLiteObject
    {
        [NonPersistent()]
        public IList<TradingPartnerHeader> UniqueCParent
        {
            get
            {
                var TradingPartners = new XPQuery<TradingPartnerHeader>(Session).OrderBy(tradingPartner => tradingPartner.cParentName);
                string lastValue = "";
                List<TradingPartnerHeader> TradingPartnersList = new List<TradingPartnerHeader>();

                foreach (var tradingPartner in TradingPartners)
                {
                    if (tradingPartner.cParentName != lastValue)
                        TradingPartnersList.Add(tradingPartner);
                    lastValue = tradingPartner.cParentName;
                }

                return TradingPartnersList;
            }
        }


        [Custom("LookupEditorMode", "AllItems")]
        [DataSourceProperty("UniqueCParent")]
        public TradingPartnerHeader TradingPartner { get; set; }

        [Persistent()]
        [RuleUniqueValue("UniqueTradingPartner", DefaultContexts.Save)]
        private string TradingPartnerPartCode
        {
            get
            {
                if (TradingPartner != null)
                    return TradingPartner.cParentName;
                return null;
            }
        }

        [Size(SizeAttribute.Unlimited)]
        public string Description { get; set; }

        [Size(300)]
        public string WebSiteURL { get; set; }

        [Aggregated, ExpandObjectMembers(ExpandObjectMembers.Never)]
        public FileDataEx Logo { get; set; }

        [Size(500)]
        [Persistent()]
        private string LogoUrl
        {
            get
            {
                if (Logo != null)
                    return Logo.RealFileName;
                else
                    return null;
            }
        }


        Guid frowguid;
        [Key(true), Persistent()]
        private Guid rowguid
        {
            get { return frowguid; }
            set { SetPropertyValue<Guid>("rowguid", ref frowguid, value); }
        }
        public TradingPartnerMainInfo(Session session) : base(session) { }
        public TradingPartnerMainInfo() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }



}
