using Aria5SystemAdmin.Module.Managers;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class Reworkfeedback
    {
        public override void AfterConstruction()
        {
            this.AddedBy = this.Session.FindObject<AriaSecuritySystemUser>(CriteriaOperator.Parse("[Oid] ='" + SecuritySystem.CurrentUserId + "'"));            
            base.AfterConstruction();
        }
        protected override void OnSaving()
        {
            Email Reworkmail = new Email();
            Reworkmail.ToEmail = this.Rework.Programmer.CurrentEmailAddress+","+this.Rework.TestedBy.CurrentEmailAddress;
            Reworkmail.FromEmail = "qua@ariasystems.biz";
            Reworkmail.EmailPassword = "quality_123"; 
            //Reworkmail.FromEmail = "ahmed.t@ariany.com";
            //Reworkmail.EmailPassword = "Aria@2016"; 
            Reworkmail.EmailSubject = "Reporting Rework";
            Reworkmail.EmailTitle = "Reporting Rework";
            Reworkmail.EmailCC =this.Rework.ProductManager.CurrentEmailAddress+","+this.Rework.TestedBy.CurrentEmailAddress;
            Reworkmail.EmailBody = "Hi All<br /> <br /> Kindly be noted that user '"+this.AddedBy.UserName+"' add a new activity to rework number: '"+this.Rework.SeqNum+"' and refrence to TicketNumber/Project : '"+this.Rework.Ticketorproject+"'<br />";
            Reworkmail.SendEmail();
        
            base.OnSaving();
        }
       
    }
}
