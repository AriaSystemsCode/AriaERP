using Aria5SystemAdmin.Module.Managers;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class Rework
    {
        public override void AfterConstruction()
        {
            if(this.Session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("UserName = '" + SecuritySystem.CurrentUserName.ToString() + "'")) != null)
            {
                this.TestedBy = this.Session.FindObject<Resources>(CriteriaOperator.Parse("UserName = '" + this.Session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("UserName = '" + SecuritySystem.CurrentUserName.ToString() + "'")).Oid + "'"));

            }
            this.TestedDate = DateTime.Now.Date;
            base.AfterConstruction();
        }

        protected override void OnSaving()
        {
            if (this.Session.IsNewObject(this))
            {
                int reNum = new XPCollection<Rework>(Session).Count + 1;
                this.SeqNum = reNum.ToString("0000");
            }
            else
            {
                if (this._isRework == ReworkConfirmation.Yes)
                {
                    if (this.RootCauseCategory == null || (string.IsNullOrEmpty(this.TempSolution) || string.IsNullOrEmpty(this.PermanentSolution)))
                    {
                        throw new Exception("Can't Save the Rework without temp or permenant solution and routcausecategory because you set the is work property to yes");
                    }
                    else
                    {
                        this.Status = ReworkStatus.InWork;
                    }
                }
            }
            
            base.OnSaving();
        }

        [DevExpress.Persistent.Base.Action]
        public void SendRework()
        {

            Email Reworkmail = new Email();
            Reworkmail.ToEmail = this.Programmer.CurrentEmailAddress;
            Reworkmail.FromEmail = "qua@ariasystems.biz";
            Reworkmail.EmailPassword = "quaity_123"; 
            //Reworkmail.FromEmail = "ahmed.t@ariany.com";
            //Reworkmail.EmailPassword = "Aria@2016"; 
            Reworkmail.EmailSubject = "Reporting Rework";
            Reworkmail.EmailTitle = "Reporting Rework";
            Reworkmail.EmailCC = this.ProductManager.CurrentEmailAddress+","+this.TestedBy.CurrentEmailAddress+","+this.TeamLeader.CurrentEmailAddress+","+this.Reviewer.CurrentEmailAddress;
            Reworkmail.EmailBody = "Hi " + this.Programmer.Name + "<br /> <br /> Kindly find the testing feedback below for task " + this.TaskNumber + " that was tested on " + this.TestedDate.Date + " by Tested by " + this.TestedBy.Name + " :<br />";
            Reworkmail.EmailBody += this.ReworkFeedback;
            Reworkmail.SendEmail();

        }
         [DevExpress.Persistent.Base.Action]
        public void Complete()
        {
            this.Status = ReworkStatus.Complete;
            this.Save();
        }
    }
}
