using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.HR
{
    [Persistent("EMPLOYEE_T")]
    public class Employee : XPLiteObject
    {
        Int32 fEMPLOYEE_KEY;
        [Key(true)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public Int32 EMPLOYEE_KEY
        {
            get { return fEMPLOYEE_KEY; }
            set { SetPropertyValue<Int32>("EMPLOYEE_KEY", ref fEMPLOYEE_KEY, value); }
        }

        [ValueConverter(typeof(DevExpress.Xpo.Metadata.ImageValueConverter)), Delayed]
        public System.Drawing.Image Photo
        {
            get { return GetDelayedPropertyValue<System.Drawing.Image>("Photo"); }
            set { SetDelayedPropertyValue<System.Drawing.Image>("Photo", value); }
        }

        string fEmployee_Code;
        [Size(14)]
        [Persistent("Employee_Code")]
        public string EmployeeCode
        {
            get { return fEmployee_Code; }
            set { SetPropertyValue<string>("EmployeeCode", ref fEmployee_Code, value); }
        }

        Title fTitle_VE;
        [Persistent("Title_VE")]
        public Title Title
        {
            get { return fTitle_VE; }
            set { SetPropertyValue<Title>("Title", ref fTitle_VE, value); }
        }

        string fFname;
        [Size(20)]
        [RuleRequiredField]
        [Persistent("Fname")]
        public string FirstName
        {
            get { return fFname; }
            set { SetPropertyValue<string>("FirstName", ref fFname, value); }
        }

        string fMname;
        [Size(20)]
        [Persistent("Mname")]
        public string MiddleName
        {
            get { return fMname; }
            set { SetPropertyValue<string>("MiddleName", ref fMname, value); }
        }

        string fLname;
        [Size(20)]
        [Persistent("Lname")]
        public string LastName
        {
            get { return fLname; }
            set { SetPropertyValue<string>("LastName", ref fLname, value); }
        }

        Country1 fNationality_KEY;
        [Persistent("Nationality_KEY")]
        public Country1 Nationality
        {
            get { return fNationality_KEY; }
            set { SetPropertyValue<Country1>("Nationality", ref fNationality_KEY, value); }
        }

        DateTime fEmployment_Date;
        [Persistent("Employment_Date")]
        public DateTime EmploymentDate
        {
            get { return fEmployment_Date; }
            set { SetPropertyValue<DateTime>("EmploymentDate", ref fEmployment_Date, value); }
        }

        Employment_status fStatus_VE;
        [Persistent("Status_VE")]
        public Employment_status EmploymentStatus
        {
            get { return fStatus_VE; }
            set { SetPropertyValue<Employment_status>("EmploymentStatus", ref fStatus_VE, value); }
        }

        Gender fGender_VE;
        [Persistent("Gender_VE")]
        public Gender Gender
        {
            get { return fGender_VE; }
            set { SetPropertyValue<Gender>("Gender", ref fGender_VE, value); }
        }

        DateTime fDate_of_Birth;
        [Persistent("Date_of_Birth")]
        public DateTime DateOfBirth
        {
            get { return fDate_of_Birth; }
            set { SetPropertyValue<DateTime>("DateOfBirth", ref fDate_of_Birth, value); }
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
        string fmail_Address;
        [Size(50)]
        [RuleRegularExpression("", DefaultContexts.Save, @"^[\w-]+(\.[\w-]+)*@([a-z0-9-]+(\.[a-z0-9-]+)*?\.[a-z]{2,6}|(\d{1,3}\.){3}\d{1,3})(:\d{4})?$")]
        [Persistent("mail_Address")]
        public string MailAddress
        {
            get { return fmail_Address; }
            set { SetPropertyValue<string>("MailAddress", ref fmail_Address, value); }
        }
        string fSocial_Insurance_No;
        [Size(10)]
        [Persistent("Social_Insurance_No")]
        public string SocialInsuranceNo
        {
            get { return fSocial_Insurance_No; }
            set { SetPropertyValue<string>("SocialInsuranceNo", ref fSocial_Insurance_No, value); }
        }
        Military_status fMilitary_status_VE;
        [Persistent("Military_status_VE")]
        public Military_status MilitaryStatus
        {
            get { return fMilitary_status_VE; }
            set { SetPropertyValue<Military_status>("MilitaryStatus", ref fMilitary_status_VE, value); }
        }
        Marital_status fMarital_status_VE;
        [Persistent("Marital_status_VE")]
        public Marital_status MaritalStatus
        {
            get { return fMarital_status_VE; }
            set { SetPropertyValue<Marital_status>("MaritalStatus", ref fMarital_status_VE, value); }
        }

        [Association("Papers", typeof(HR_Attachment1)), Aggregated]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public XPCollection Documents
        {
            get { return GetCollection("Documents"); }
        }

        Jobs fJob_KEY;
        [Persistent("Job_KEY")]
        public Jobs Job
        {
            get { return fJob_KEY; }
            set { SetPropertyValue<Jobs>("Job", ref fJob_KEY, value); }
        }
        string fDescription;
        [Size(200)]
        public string Description
        {
            get { return fDescription; }
            set { SetPropertyValue<string>("Description", ref fDescription, value); }
        }
        HR_Address1 fAddress_KEY;
        [VisibleInLookupListView(false)]
        [Persistent("Address_KEY")]
        public HR_Address1 Address
        {
            get { return fAddress_KEY; }
            set { SetPropertyValue<HR_Address1>("Address", ref fAddress_KEY, value); }
        }

        public Employee(Session session) : base(session) { }
        public Employee() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
