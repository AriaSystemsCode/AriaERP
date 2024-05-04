using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;
using DevExpress.Persistent.Base;

namespace AriaDevExpress.Module.BusinessObjects.HR
{
    [Persistent("Jobs_T")]
    public class Jobs : XPLiteObject
    {
        Int32 fJob_KEY;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public Int32 Job_KEY
        {
            get { return fJob_KEY; }
            set { SetPropertyValue<Int32>("Job_KEY", ref fJob_KEY, value); }
        }

        string fJob_Code;
        [Size(14)]
        [Persistent("Job_Code")]
        public string JobCode
        {
            get { return fJob_Code; }
            set { SetPropertyValue<string>("JobCode", ref fJob_Code, value); }
        }
        string fDescription;
        [Size(200)]
        public string Description
        {
            get { return fDescription; }
            set { SetPropertyValue<string>("Description", ref fDescription, value); }
        }
        HR_Department1 fDepartment_KEY;
        [Persistent("Department_KEY")]
        public HR_Department1 Department
        {
            get { return fDepartment_KEY; }
            set { SetPropertyValue<HR_Department1>("Department", ref fDepartment_KEY, value); }
        }
        Branch fBranch_KEY;
        [Persistent("Branch_KEY")]
        public Branch Branch
        {
            get { return fBranch_KEY; }
            set { SetPropertyValue<Branch>("Branch", ref fBranch_KEY, value); }
        }

        Positions fPositions_KEY;
        public Positions Positions_KEY
        {
            get { return fPositions_KEY; }
            set { SetPropertyValue<Positions>("Positions_KEY", ref fPositions_KEY, value); }
        }

        public Jobs(Session session) : base(session) { }
        public Jobs() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
