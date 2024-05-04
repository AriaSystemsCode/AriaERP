using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;
using DevExpress.Persistent.Base;

namespace AriaDevExpress.Module.BusinessObjects.HR
{

    [Persistent("Department_T")]
    public class HR_Department1 : XPLiteObject
    {
        Int32 fDepartment_KEY;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public Int32 Department_KEY
        {
            get { return fDepartment_KEY; }
            set { SetPropertyValue<Int32>("Department_KEY", ref fDepartment_KEY, value); }
        }
        string fDepartment_Code;
        [Size(5)]
        [Persistent("Department_Code")]
        public string DepartmentCode
        {
            get { return fDepartment_Code; }
            set { SetPropertyValue<string>("DepartmentCode", ref fDepartment_Code, value); }
        }
        string fDepartment_name;
        [Size(60)]
        [Persistent("Department_name")]
        public string DepartmentName
        {
            get { return fDepartment_name; }
            set { SetPropertyValue<string>("DepartmentName", ref fDepartment_name, value); }
        }
        string fDescription;
        [Size(200)]
        public string Description
        {
            get { return fDescription; }
            set { SetPropertyValue<string>("Description", ref fDescription, value); }
        }
        public HR_Department1(Session session) : base(session) { }
        public HR_Department1() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
