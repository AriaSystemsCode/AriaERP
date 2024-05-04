using Aria5SystemAdmin.Module.Managers;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class HRJobVacancy
    {
        public override void AfterConstruction()
        {
            this._requestNumber = new XPCollection<HRJobVacancy>(Session).Count + 1;
            this.RequestedDate = DateTime.Now.Date;
            this.Status = HRJobVacancy.VacancyStatus.Submited;
            AriaSecuritySystemUser user = this.Session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("UserName = '"+SecuritySystem.CurrentUserName+"'")); 
            if(user != null)
            {
                this.RequestedBy = user.Employee;
            }
         
            base.AfterConstruction();
        }
        protected override void OnSaving()
        {
            if (this.Session.IsNewObject(this))
            {
                Email vacancyemail = new Email();
                vacancyemail.ToEmail = "HR@ariasystems.biz";
                vacancyemail.EmailSubject = "New job Vacancy submitted";
                vacancyemail.EmailTitle = "New job Vacancy submitted";
                vacancyemail.EmailBody = string.Format("Hi All,<br/> Kindly be noted there are new job vacancy submitted on the system admin for '{0}' Job position kindly check it and take the nescessary actions",this.JobPosition.Name);
            }
            base.OnSaving();
        }

    }
}
