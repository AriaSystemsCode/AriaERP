using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;
using DevExpress.Persistent.Base;

namespace AriaDevExpress.Module.BusinessObjects.HR
{

    [Persistent("Branch_T")]
    public class Branch : XPLiteObject
    {
        Int32 fBranch_KEY;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        [Persistent("Branch_KEY")]
        [DisplayName("Branch")]
        public Int32 Branch_Key
        {
            get { return fBranch_KEY; }
            set { SetPropertyValue<Int32>("Branch_Key", ref fBranch_KEY, value); }
        }
        string fBranch_Code;
        [Size(5)]
        [Persistent("Branch_Code")]
        public string BranchCode
        {
            get { return fBranch_Code; }
            set { SetPropertyValue<string>("BranchCode", ref fBranch_Code, value); }
        }
        string fBranch_name;
        [Size(60)]
        [Persistent("Branch_name")]
        public string BranchName
        {
            get { return fBranch_name; }
            set { SetPropertyValue<string>("BranchName", ref fBranch_name, value); }
        }

        HR_Contact fContact_KEY;
        [Persistent("Contact_KEY")]
        public HR_Contact Contact
        {
            get { return fContact_KEY; }
            set { SetPropertyValue<HR_Contact>("Contact", ref fContact_KEY, value); }
        }
        string fDescription;
        [Size(200)]
        public string Description
        {
            get { return fDescription; }
            set { SetPropertyValue<string>("Description", ref fDescription, value); }
        }
        public Branch(Session session) : base(session) { }
        public Branch() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
