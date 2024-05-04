using Aria5SystemAdmin.Module.Controllers;
using DevExpress.Data.Filtering;
using DevExpress.Persistent.Validation;
using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class TrackingEntry
    {
        public enum ModificationTypes
        {
            
            Modify = 1,
            Add = 0
        };

        
        protected override void OnSaving()
        {
            //MMT
            if ((!string.IsNullOrEmpty (this.Description) && !string.IsNullOrEmpty(this.ReleaseNote)) && (this.Description.Trim() == this.ReleaseNote.Trim()))
            {
                throw new Exception("Tracking Entry Release Notes is the same as the description");
            }
            if ((!string.IsNullOrEmpty(this.Description) && !string.IsNullOrEmpty(this.TechnicalInformation)) && (this.Description.Trim() == this.TechnicalInformation.Trim () ))
            {
                throw new Exception("Tracking Entry Technical Consideration is the same as the description");
            }
            //MMT

            if (this.ProjectEntity != null && this.EntityFeatures!=null)
            {
                this.EntityFeatures.AddRange(this.ProjectEntity.MainFeatures);
    
            }
            if (Disable == false)
            {
                if (ProjectTemplate != null && ProjectTemplate.Activities != null && ProjectTemplate.Activities.Count > 0)
                {
                    foreach (Activity activity in ProjectTemplate.Activities)
                    {
                        TrackingTask task = new TrackingTask(this.Session);
                        task.Task = activity;
                        task.Description = activity.Description;
                        this.TrackingTasks.Add(task);
                    }
                }
            }
            if (ProjectTemplate != null)
            {
                ProjectTemplate.TestCases.AddRange(this.TestCases);
                ProjectTemplate.TestRuns.AddRange(this.TestRuns);
                ProjectTemplate.TestPlans.AddRange(this.TestPlans);
                if (ProjectTemplate.Type == Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate.ProjectType.Standard)
                {
                    if (this.ParentTrackingEntry == null)
                    {
                        QAWBS newphase = Session.FindObject<QAWBS>(CriteriaOperator.Parse("[PhaseTitle] = '" + this.ID + " - " + this.ReferenceNo + "'"));
                        if (newphase == null)
                        {
                            newphase = new QAWBS(this.Session);
                            newphase.PhaseTitle = this.ID + " - " + this.ReferenceNo;
                            newphase.UseCasePoints = this.ProjectTemplate.UseCasePoints;
                            this.ProjectTemplate.ProjectWBS.Add(newphase);
                        }
                    }
                    else
                    {
                        //ATA check if this is child tracking entry we create it as asubphase or activity in the phase
                        QAWBS newphase = Session.FindObject<QAWBS>(CriteriaOperator.Parse("[PhaseTitle] = '" + this.ParentTrackingEntry.ID + " - " + this.ParentTrackingEntry.ReferenceNo + "'"));
                        if (newphase != null)
                        {
                            QAActivity newsubphase = new QAActivity(this.Session);
                            newsubphase.Phase = true;
                            newsubphase.Activity = this.ID + " - " + this.ReferenceNo;
                            newsubphase.QAWBS = newphase;
                        }
                        else
                        {
                            newphase = new QAWBS(this.Session);
                            newphase.PhaseTitle = this.ParentTrackingEntry.ID + " - " + this.ParentTrackingEntry.ReferenceNo;
                            newphase.UseCasePoints = this.ProjectTemplate.UseCasePoints;
                            this.ProjectTemplate.ProjectWBS.Add(newphase);
                            QAActivity newsubphase = new QAActivity(this.Session);
                            newsubphase.Phase = true;
                            newsubphase.Activity = this.ID + " - " + this.ReferenceNo;
                            newsubphase.QAWBS = newphase;
                        }

                    }
                   

                   
                    
                   // newphase.Session.CommitTransaction();
                }
            }
            base.OnSaving();
            //TrackingTask task = new TrackingTask(this.Session);
            //task.Task = this.ProjectTemplate.Name;
            //task.Description = this.ProjectTemplate.Description;
            ////this.TrackingTasks.Add(task);
            //var taskExist = this.Session.FindObject<TrackingTask>(CriteriaOperator.Parse("Task ='" + this.ProjectTemplate.Name + "'"));
            //if (taskExist == null)
            //{
            //    this.TrackingTasks.Add(task);
            //}
            Disable = true;
           // CheckInComment = TicketNumber + " - " +ID.ToString() + " - " + this.TrackingTasks.assignedSource;


        }

        public override void AfterConstruction()
        {
            base.AfterConstruction();
            Disable = false;   
            IsNew = this.Session.IsNewObject(this);

            this.Type = TrackingType.Bug;
            Aria5SystemAdmin.Module.Aria5Globals aria5Globals = new Aria5Globals();
           //ATA enhnce calculate id for new entry only 22/6/2016  [start]
            if (IsNew = true)
            {
                this.ID = aria5Globals.GetSequence("TrackingEntry", "ID", Session, "");
                this.Status = TrackingStatus.New;

            }
            //ATA enhnce calculate id for new entry only 22/6/2016  [start] Doaa
            try
            {
                this.Application = this.Session.FindObject<Application_T>(DevExpress.Data.Filtering.CriteriaOperator.Parse(""));
                this.Entity = this.Application.AriaObjects[0];
                this.ReleaseNo = this.Application.ReleaseNo == null ? 0 : int.Parse(this.Application.ReleaseNo);
                this.ServicePackNo = this.Application.ServicePack == null ? 0 : int.Parse(this.Application.ServicePack);
            }
            catch 
            {
            }


            this.Priority = TrackingPriority.Normal;
            this.EnteredDate = System.DateTime.Today;
            if (IsNew) RequestedDate = DateTime.Now.Date;
            //base.AfterConstruction();
        }

        protected override void OnLoaded()
        {
            base.OnLoaded();

            IsNew = this.Session.IsNewObject(this);
        }



        [CodeRule]
        public class TrackingEntryCodeRule : RuleBase<TrackingEntry>
        {
            protected override bool IsValidInternal(TrackingEntry target, out string errorMessageTemplate)
            {
                if (target.Entity != null && target.Application != null && target.Entity.ObjectName != null && target.Application.Name != null)
                {
                    if(target.Entity.Applications.Where(r => r.Oid == target.Application.Oid).Count() == 0)
                    {
                        errorMessageTemplate = "Can't save, entity must be related to the application";
                        return false;
                    }
                }

                errorMessageTemplate = "";
                return true;
            }



            public TrackingEntryCodeRule() : base("", DefaultContexts.Save) { }
            public TrackingEntryCodeRule(IRuleBaseProperties properties) : base(properties) { }
        }
    }
}
