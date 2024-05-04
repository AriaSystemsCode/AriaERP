using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Security;
using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QAUseCase
    {
        protected override void OnSaving()
        {

            if ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters != null)
            {


                string userName = ((AuthenticationStandardLogonParameters)SecuritySystem.LogonParameters).UserName;

                this.CreatedBy = userName;
                this.CreationDate = DateTime.Now;
                //ATA 17/7/2016 notification module customization [statrt]

                //ATA modify the notification message for the project template notification 
                if (this.ProjectTemplate != null)
                {
                    if (this.ApproveStatus == ApproveStatu.Ready)
                    {
                        if (this.UseCaseFlows.Count == 0)
                        {
                            throw new Exception("can't save this use case as ready without flows ");
                        }
                    }
                    this.Session.Save(this.ProjectTemplate);
                }
                //ATA 17/7/2016 notification module customization [end]
                base.OnSaving();
            }
        }
        //protected override void Unlink(SimpleActionExecuteEventArgs args)
        //{
        //    base.Unlink(args);

        //    foreach (object item in args.SelectedObjects)
        //    {
        //        Contact contact = item as Contact;
        //        //Dennis: Or you can limit the execution of this code to certain View (check View.XXX properties for more information about the current context).
        //        if (contact != null)
        //            SendEmailToContact(contact);
        //    }
        //}
        //protected override void OnDeleting()
        //{
        //    if (Session.CollectReferencingObjects(this).Count > 0)
        //    {
        //        throw new Exception("Can not Delete Use Case!");
        //    }
        //    else
        //    {

        //        base.OnDeleting();

        //    }


        //}
        protected override void OnDeleting()
        {
            if (DateTime.Today > CalcEndate.CalcEnddate(this.ProjectTemplate.StartDate, 2))
            {
                base.OnDeleting();
            }
            else if (DateTime.Today >= CalcEndate.CalcEnddate(this.ProjectTemplate.StartDate, 2).Subtract(new TimeSpan(10, 0, 0, 0)))
            {


                    this.Session.Save(this.ProjectTemplate);

                base.OnDeleting();
            }
        }
        public override void AfterConstruction()
        {

            if (_projectTemplate != null)
            {
                this.Application = _projectTemplate.Application;
                //ATA, 1,bgn,15/3/2016,check if _enddate not calculated and it is assign to a project then calculate it according to project type 
                if (Enddate == DateTime.MinValue)
                {
                    if (_projectTemplate.Type.ToString() == "Key")
                    {
                        Enddate = CalcEndate.CalcEnddate(_projectTemplate.StartDate, 2);

                    }
                    else
                    {
                        Enddate = _projectTemplate.EndDate;
                    }
                }       
            }
            base.AfterConstruction();
        }
    }
}
