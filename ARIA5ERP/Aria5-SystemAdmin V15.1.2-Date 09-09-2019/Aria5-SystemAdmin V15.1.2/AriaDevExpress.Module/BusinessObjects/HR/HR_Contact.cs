using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;
using DevExpress.Persistent.Base;

namespace AriaDevExpress.Module.BusinessObjects.HR
{


    [Persistent("Contact_T")]
   // [NonPersistent]
    public class HR_Contact : XPLiteObject
    {
        Int32 fContact_KEY;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        [Persistent("Contact_KEY")]
        [DisplayName("Contact")]
        public Int32 Contact_KEY
        {
            get { return fContact_KEY; }
            set { SetPropertyValue<Int32>("Contact_KEY", ref fContact_KEY, value); }
        }
        string fContact_Email;
        [Size(50)]
        [Persistent("Contact_Email")]
        public string ContactEmail
        {
            get { return fContact_Email; }
            set { SetPropertyValue<string>("ContactEmail", ref fContact_Email, value); }
        }

        string fTelephone_No;
        [Size(16)]
        [Persistent("Telephone_No")]
        public string TelephoneNo
        {
            get { return fTelephone_No; }
            set { SetPropertyValue<string>("TelephoneNo", ref fTelephone_No, value); }
        }
        string fMobile_No;
        [Size(16)]
        [Persistent("Mobile_No")]
        public string MobileNo
        {
            get { return fMobile_No; }
            set { SetPropertyValue<string>("MobileNo", ref fMobile_No, value); }
        }
        string fDescription;
        [Size(200)]
        public string Description
        {
            get { return fDescription; }
            set { SetPropertyValue<string>("Description", ref fDescription, value); }
        }

        HR_Address1 fAddress_KEY;
        [Persistent("Address_KEY")]
        public HR_Address1 Address
        {
            get { return fAddress_KEY; }
            set { SetPropertyValue<HR_Address1>("Address", ref fAddress_KEY, value); }
        }
        public HR_Contact(Session session) : base(session) { }
        public HR_Contact() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
