using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;
using DevExpress.Persistent.Base;

namespace AriaDevExpress.Module.BusinessObjects.HR
{
    [Persistent("Applicant_Vacancies_T")]
    public class ApplicantVacancies : XPLiteObject
    {
        long fVacancies_KEY;
        [Key]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public long Vacancies_KEY
        {
            get { return fVacancies_KEY; }
            set { SetPropertyValue<long>("Vacancies_KEY", ref fVacancies_KEY, value); }
        }
        string fVacancie_Code;
        [Size(10)]
        [Persistent("Vacancie_Code")]
        public string VacancyCode
        {
            get { return fVacancie_Code; }
            set { SetPropertyValue<string>("VacancyCode", ref fVacancie_Code, value); }
        }
        string fDescription;
        [Persistent("Description")]
        public string VacancyDescription
        {
            get { return fDescription; }
            set { SetPropertyValue<string>("VacancyDescription", ref fDescription, value); }
        }
        DateTime fVacancie_StartDate;
        [Persistent("Vacancie_StartDate")]
        public DateTime VacancyStartDate
        {
            get { return fVacancie_StartDate; }
            set { SetPropertyValue<DateTime>("VacancyStartDate", ref fVacancie_StartDate, value); }
        }

        Positions fPositions_KEY;
        [Persistent("Positions_KEY")]
        public Positions Position
        {
            get { return fPositions_KEY; }
            set { SetPropertyValue<Positions>("Position", ref fPositions_KEY, value); }
        }
        public ApplicantVacancies(Session session) : base(session) { }
        public ApplicantVacancies() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
