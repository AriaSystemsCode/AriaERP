using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class QAUseCasePoints
    {
        /*
        public DateTime CalcEnddate(DateTime StartDatetime, int MonthesCount)
        {
            DateTime end = DateTime.Now;
            DateTime firstDayafterNMonth = StartDatetime.AddDays(-StartDatetime.Day + 1).AddMonths(MonthesCount);
            int diff = 7 - (int)firstDayafterNMonth.DayOfWeek;
            end = firstDayafterNMonth.AddDays(diff);


            return end;
        }
         */
        protected override void OnSaving()
        {
            //if (this.ProjectEfforts > 0)
            //{
            //    foreach (QAWBS phase in this.WBS)
            //    {
            //        phase.ProjEfrt = this.ProjectEfforts;
            //        phase.Save();
            //        phase.Session.CommitTransaction();
            //    }
            //}
            base.OnSaving();
        }
        public override void AfterConstruction()
        {
            //base.AfterConstruction();
            //if (this.ProjectEfforts > 0)
            //{
            //    foreach (QAWBS phase in this.WBS)
            //    {
            //        phase.ProjEfrt = this.ProjectEfforts;
            //        phase.Save();
            //        phase.Session.CommitTransaction();
            //    }
            //}
        }
    }
}
