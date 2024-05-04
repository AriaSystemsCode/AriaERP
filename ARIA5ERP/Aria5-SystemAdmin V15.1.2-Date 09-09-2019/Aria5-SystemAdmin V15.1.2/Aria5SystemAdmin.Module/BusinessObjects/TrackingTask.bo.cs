using System;
using System.Collections.Generic;
using System.Text;
using Aria5SystemAdmin.Module.Managers;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class TrackingTask
    {
        public enum StatusTypes { New, InWork, Hold, Complete }

        protected override void OnSaving()
        {
            if (this.TrackingEntry != null)
            {
                this.TrackingEntry.CheckInComment = "";
            }
           if (this.Resources != null && !string.IsNullOrWhiteSpace(Resources.ToString()) && this.Task.CheckOut==true && this.TrackingEntry != null)
            {
                this.TrackingEntry.CheckInComment = this.TrackingEntry.TicketNumber + " - " + this.TrackingEntry.ID.ToString() + " - " + this.Resources.Name.ToString();
                this.TrackingEntry.Save();
             
            }
           if (this.Session.IsNewObject(this))
           {
               if (this.TrackingEntry != null) //&& this.TrackingEntry.ProjectEntity != null)
               {
                   //ATA add current tracking entry project to the tracking task [start]
                   if (TrackingEntry != null && TrackingEntry.ProjectTemplate != null)
                   {
                       this.Project = TrackingEntry.ProjectTemplate;
                   }
                   //ATA add current tracking entry project to the tracking task [End]
                   //5/6/2016 check duplication before save [start ]
                   if (this.TrackingEntry.TrackingTasks.Count > 0)
                   {
                       //foreach (TrackingTask existtask in this.TrackingEntry.TrackingTasks)
                       //{
                       //    if (existtask.Oid != Guid.Empty)
                       //    {
                       //        if (existtask.Task == this.Task && existtask.Resources == this.Resources)
                       //        {
                       //            throw new Exception("there is already task of type " + this.Task.Name.ToString() + " assigned to '" + this.Resources.Name.ToString() + "'");
                       //        }
                       //    }

                       //}
                   }
                    Aria5SystemAdmin.Module.Aria5Globals aria5Globals = new Aria5Globals();
                    this.ID = aria5Globals.GetSequence("TrackingTask", "ID", this.Session, "[TrackingEntry] = '" + this.TrackingEntry.Oid + "'");
                    //switch (this.Task.Name)
                    //{
                    //    case "Design":
                    //        this.TrackingEntry.ProjectEntity.DetailDesignTime += this.Duration;
                    //        break;
                    //    case "Programming":
                    //        this.TrackingEntry.ProjectEntity.ProgrammingTime += this.Duration;
                    //        break;
                    //}
                    // Doaa 05/01/2019 {start}
                    if (this.Task == null)
                    {
                        throw new Exception("Tracking Entry doesn't have tasks to start");
                    }
                    // Doaa 05/01/2019 {start}
                    Tittle = "[Tracking" + this.TrackingEntry.ID + "+" + this.TrackingEntry.Entity.ObjectName + "]_" + this.Task.Name.ToString() + this.ID.ToString();

                }
                else
               {
                   throw new Exception("tracking Entry don't have project entity");
               }
               #region test
               alarmTime = EndDate;
               NotificationMessage = string.Format("today is your due date to finish this task '{0}'", this.Tittle);
               //Email testemail = new Email();
               //testemail.FromEmail = "khaled.m@ariasystems.biz";
               //testemail.EmailPassword = "Kamag@2016";
               //testemail.ToEmail = this.Resources.CurrentEmailAddress.ToString();
               //testemail.EmailSubject = "Task created ";
               //testemail.EmailBody = "Dear '" + this.Resources.Name.ToString() + "' <br />";
               //testemail.EmailBody += " " + this.Tittle + " of type '" + this.Task.Name + "' task is created and assigned to you ";
               //testemail.SendEmail();
               #endregion
               //ATA add new resource share if the resource not found in this project resource share 2/19/2017 [start]
               if (this.Resources != null)
               {
                   if (this.WBSMonth != null && this.WBSMonth.UseCasePoints != null && this.WBSMonth.UseCasePoints.ResourceShares.FindIndex(x => x.ResourceName == this.Resources) <= 0)
                   {
                       QAResourceShare share = new QAResourceShare(this.Session);
                       share.UseCasePoints = this.WBSMonth.UseCasePoints;
                       share.ResourceName = this.Resources;
                       share.DepartmentName = this.Resources.Department;
                   }
               }
               //ATA add new resource share if the resource not found in this project resource share 2/19/2017 [End]

           }
           base.OnSaving();

          
        }

        public override void AfterConstruction()
        {
            base.AfterConstruction();
            RemindIn = TimeSpan.Parse("01:00:00");
          
        }
        protected override void OnDeleting()
        {
            base.OnDeleting();
        }
    }
}
