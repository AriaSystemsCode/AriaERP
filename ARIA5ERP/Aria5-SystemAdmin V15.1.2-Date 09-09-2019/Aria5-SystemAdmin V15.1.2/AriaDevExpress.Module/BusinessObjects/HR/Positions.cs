using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;
using DevExpress.Persistent.Base;

namespace AriaDevExpress.Module.BusinessObjects.HR
{
    public class Positions : XPLiteObject
    {
        long fPosition_Key;
        [Key]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public long Position_Key
        {
            get { return fPosition_Key; }
            set { SetPropertyValue<long>("Position_Key", ref fPosition_Key, value); }
        }

        string fPositionTitle;
        [Persistent(@"Position Title")]
        public string PositionTitle
        {
            get { return fPositionTitle; }
            set { SetPropertyValue<string>("PositionTitle", ref fPositionTitle, value); }
        }

        string fPositionDescription;
        public string PositionDescription
        {
            get { return fPositionDescription; }
            set { SetPropertyValue<string>("PositionDescription", ref fPositionDescription, value); }
        }

        public Positions(Session session) : base(session) { }
        public Positions() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }

}
