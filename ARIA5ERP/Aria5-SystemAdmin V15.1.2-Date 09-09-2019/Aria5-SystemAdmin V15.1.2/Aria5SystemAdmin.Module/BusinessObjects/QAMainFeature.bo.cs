using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QAMainFeature
    {
        //ATA bgn 15/3/2016
        /*
        public DateTime CalcEnddate(DateTime StartDatetime, int MonthesCount)
        {
            DateTime end = DateTime.Now;
            DateTime firstDayafterNMonth = StartDatetime.AddDays(-StartDatetime.Day + 1).AddMonths(MonthesCount);
            int diff = 7 - (int)firstDayafterNMonth.DayOfWeek;
            end = firstDayafterNMonth.AddDays(diff);


            return end;
        }
        //ATA 
         */
        protected override void OnSaving()
        {
            //ATA extend the alarm time to be for the detail design for this project entity 17/7/2016 [start] 
            if (Session.IsNewObject(this))
            {
                //if (this.ProjectEntity != null)
                //{
                //    if (this.ProjectEntity.EntitySystemDesign.Count == 0 && this.ProjectEntity.AlarmTime >= this.ProjectEntity.Enddate.Subtract(new TimeSpan(10, 0, 0, 0)))
                //    {
                //        this.ProjectEntity.AlarmTime = this.ProjectEntity.AlarmTime.Value.Add(new TimeSpan(30, 0, 0, 0));
                //    }
                //    this.ProjectEntity.NotificationMessage.Replace("Entity Feature ", " ");
                //    this.Session.Save(this.ProjectEntity);
                //}
            }
            //ATA extend the alarm time to be for the detail design for this project entity 17/7/2016 [End] 

            base.OnSaving();
        }
        //ATA 20/7/2016 [start]
        protected override void OnDeleting()
        {
            if (DateTime.Today > this.ProjectEntity.Enddate)
            {
                base.OnDeleting();
            }
            else if (DateTime.Today >= this.ProjectEntity.Enddate.Subtract(new TimeSpan(10, 0, 0, 0)))
            {
                if (this.ProjectEntity.MainFeatures.Count == 1)
                {
                    //ProjectEntity.NotificationMessage = string.Format("you need to add this items before this date '{0}' for this project enity '{1}'", this.ProjectEntity.Enddate, this.ProjectEntity.Name);
                    //ProjectEntity.NotificationMessage += "  Main feature ";
                    //if (this.ProjectEntity.EntitySystemDesign.Count == 0)
                    //{
                    //    ProjectEntity.NotificationMessage += " Entity system design";
                    //}
                    //this.ProjectEntity.AlarmTime = DateTime.Today;
                    //this.Session.Save(this.ProjectEntity);
                    //base.OnDeleting();
                }
            }
        }
        //ATA 20/7/2016 [start]
    }
}
