using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;
using DevExpress.Persistent.Base;

namespace AriaDevExpress.Module.BusinessObjects.HR
{
    [Persistent("Applicants_Certificates_T")]
    public class ApplicantsCertificates : XPLiteObject
    {
        long fCertificates_KEY;
        [Key]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public long Certificates_KEY
        {
            get { return fCertificates_KEY; }
            set { SetPropertyValue<long>("Certificates_KEY", ref fCertificates_KEY, value); }
        }
        string fCertificate_Name;
        [Size(50)]
        [Persistent("Certificate_Name")]
        public string CertificateName
        {
            get { return fCertificate_Name; }
            set { SetPropertyValue<string>("CertificateName", ref fCertificate_Name, value); }
        }
        DateTime fCertificate_From;
        public DateTime CertificateFrom
        {
            get { return fCertificate_From; }
            set { SetPropertyValue<DateTime>("CertificateFrom", ref fCertificate_From, value); }
        }
        DateTime fCertificate_to;
        public DateTime CertificateTo
        {
            get { return fCertificate_to; }
            set { SetPropertyValue<DateTime>("CertificateTo", ref fCertificate_to, value); }
        }
        string fCertificate_Description;
        [Persistent("Certificate_Description")]
        public string CertificateDescription
        {
            get { return fCertificate_Description; }
            set { SetPropertyValue<string>("CertificateDescription", ref fCertificate_Description, value); }
        }

        Applicants fApplicants;
        [Association("Applicants Certificates")]
        public Applicants Applicants
        {
            get { return fApplicants; }
            set { SetPropertyValue<Applicants>("Applicants", ref fApplicants, value); }
        }
        public ApplicantsCertificates(Session session) : base(session) { }
        public ApplicantsCertificates() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
