using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;
using DevExpress.Persistent.Base;

namespace AriaDevExpress.Module.BusinessObjects.HR
{
    
    [Persistent("Applicants_EmploymentHistory_T")]
    public class ApplicantsEmploymentHistory : XPLiteObject
    {
        long fEmployment_History_KEY;
        [Key]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public long Employment_History_KEY
        {
            get { return fEmployment_History_KEY; }
            set { SetPropertyValue<long>("Employment_History_KEY", ref fEmployment_History_KEY, value); }
        }
        string fEmployer;
        public string Employer
        {
            get { return fEmployer; }
            set { SetPropertyValue<string>("Employer", ref fEmployer, value); }
        }
        string fPositionTitle;
        [Persistent(@"Position Title")]
        public string PositionTitle
        {
            get { return fPositionTitle; }
            set { SetPropertyValue<string>("PositionTitle", ref fPositionTitle, value); }
        }
        DateTime ffrom;
        public DateTime from
        {
            get { return ffrom; }
            set { SetPropertyValue<DateTime>("from", ref ffrom, value); }
        }
        DateTime fto;
        public DateTime to
        {
            get { return fto; }
            set { SetPropertyValue<DateTime>("to", ref fto, value); }
        }
        string fResponsibilities;
        [Size(300)]
        public string Responsibilities
        {
            get { return fResponsibilities; }
            set { SetPropertyValue<string>("Responsibilities", ref fResponsibilities, value); }
        }
        double fCurrentsalary;
        [Persistent(@"Current salary")]
        public double Salary
        {
            get { return fCurrentsalary; }
            set { SetPropertyValue<double>("Salary", ref fCurrentsalary, value); }
        }
        string fReasonsforleaving;
        [Persistent(@"Reasons for leaving")]
        public string ReasonsOfLeaving
        {
            get { return fReasonsforleaving; }
            set { SetPropertyValue<string>("ReasonsOfLeaving", ref fReasonsforleaving, value); }
        }
        bool fPresent;
        public bool Present
        {
            get { return fPresent; }
            set { SetPropertyValue<bool>("Present", ref fPresent, value); }
        }
        public ApplicantsEmploymentHistory(Session session) : base(session) { }
        public ApplicantsEmploymentHistory() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }

}
