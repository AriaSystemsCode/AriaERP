using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.HR
{
    [Persistent("Applicants_T")]
    public class Applicants : XPLiteObject
    {
        long fApplicant_KEY;
        [Key]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public long Applicant_KEY
        {
            get { return fApplicant_KEY; }
            set { SetPropertyValue<long>("Applicant_KEY", ref fApplicant_KEY, value); }
        }
        string fFname;
        [Size(20)]
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
        [Persistent("mail_Address")]
        public string EmailAddress
        {
            get { return fmail_Address; }
            set { SetPropertyValue<string>("EmailAddress", ref fmail_Address, value); }
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

        string fSecondarySchool_VE;
        [Size(20)]
        [Persistent(@"Secondary School_VE")]
        public string SecondarySchool
        {
            get { return fSecondarySchool_VE; }
            set { SetPropertyValue<string>("SecondarySchool", ref fSecondarySchool_VE, value); }
        }
        FileData fResume_KEY;
        [Persistent("Resume_KEY")]
        public FileData Resume
        {
            get { return fResume_KEY; }
            set { SetPropertyValue<FileData>("Resume", ref fResume_KEY, value); }
        }
        DateTime fQes1_StartDate;
        public DateTime Qes1_StartDate
        {
            get { return fQes1_StartDate; }
            set { SetPropertyValue<DateTime>("Qes1_StartDate", ref fQes1_StartDate, value); }
        }
        string fQes2_why;
        public string Qes2_why
        {
            get { return fQes2_why; }
            set { SetPropertyValue<string>("Qes2_why", ref fQes2_why, value); }
        }
        string fQes3_Skills;
        public string Qes3_Skills
        {
            get { return fQes3_Skills; }
            set { SetPropertyValue<string>("Qes3_Skills", ref fQes3_Skills, value); }
        }
        ApplicantsEmploymentHistory fEmployment_History_KEY;
        public ApplicantsEmploymentHistory Employment_History_KEY
        {
            get { return fEmployment_History_KEY; }
            set { SetPropertyValue<ApplicantsEmploymentHistory>("Employment_History_KEY", ref fEmployment_History_KEY, value); }
        }
        string fUniversity;
        public string University
        {
            get { return fUniversity; }
            set { SetPropertyValue<string>("University", ref fUniversity, value); }
        }
        string fFaculty;
        public string Faculty
        {
            get { return fFaculty; }
            set { SetPropertyValue<string>("Faculty", ref fFaculty, value); }
        }
        string fMajor;
        public string Major
        {
            get { return fMajor; }
            set { SetPropertyValue<string>("Major", ref fMajor, value); }
        }
        DateTime fGraduation_Year;
        public DateTime Graduation_Year
        {
            get { return fGraduation_Year; }
            set { SetPropertyValue<DateTime>("Graduation_Year", ref fGraduation_Year, value); }
        }

        [Association("Applicants Certificates", typeof(ApplicantsCertificates)), Aggregated]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public XPCollection Certificates_KEY
        {
            get { return GetCollection("Certificates_KEY"); }
        }

        ApplicantVacancies fVacancies_KEY;
        public ApplicantVacancies Vacancies_KEY
        {
            get { return fVacancies_KEY; }
            set { SetPropertyValue<ApplicantVacancies>("Vacancies_KEY", ref fVacancies_KEY, value); }
        }
        bool fConfirm;
        public bool Confirm
        {
            get { return fConfirm; }
            set { SetPropertyValue<bool>("Confirm", ref fConfirm, value); }
        }
        bool fMonday;
        public bool Monday
        {
            get { return fMonday; }
            set { SetPropertyValue<bool>("Monday", ref fMonday, value); }
        }
        bool fTuesday;
        [Persistent(@"Tuesday ")]
        public bool Tuesday
        {
            get { return fTuesday; }
            set { SetPropertyValue<bool>("Tuesday", ref fTuesday, value); }
        }
        bool fWednesday;
        [Persistent(@"Wednesday ")]
        public bool Wednesday
        {
            get { return fWednesday; }
            set { SetPropertyValue<bool>("Wednesday", ref fWednesday, value); }
        }
        bool fThursday;
        [Persistent(@"Thursday ")]
        public bool Thursday
        {
            get { return fThursday; }
            set { SetPropertyValue<bool>("Thursday", ref fThursday, value); }
        }
        bool fFriday;
        [Persistent(@"Friday ")]
        public bool Friday
        {
            get { return fFriday; }
            set { SetPropertyValue<bool>("Friday", ref fFriday, value); }
        }
        bool fSaturday;
        [Persistent(@"Saturday ")]
        public bool Saturday
        {
            get { return fSaturday; }
            set { SetPropertyValue<bool>("Saturday", ref fSaturday, value); }
        }
        bool fSunday;
        [Persistent(@"Sunday ")]
        public bool Sunday
        {
            get { return fSunday; }
            set { SetPropertyValue<bool>("Sunday", ref fSunday, value); }
        }
        DateTime fHours_from;
        public DateTime Hours_from
        {
            get { return fHours_from; }
            set { SetPropertyValue<DateTime>("Hours_from", ref fHours_from, value); }
        }
        DateTime fHours_to;
        public DateTime Hours_to
        {
            get { return fHours_to; }
            set { SetPropertyValue<DateTime>("Hours_to", ref fHours_to, value); }
        }
        public Applicants(Session session) : base(session) { }
        public Applicants() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
