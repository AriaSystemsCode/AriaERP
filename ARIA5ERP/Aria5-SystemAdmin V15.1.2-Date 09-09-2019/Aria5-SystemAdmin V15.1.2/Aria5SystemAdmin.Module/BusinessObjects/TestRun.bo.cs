using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class TestRun
    {
        //protected override void OnDeleting()
        //{
        //    if (Session.CollectReferencingObjects(this).Count > 0)
        //    {
        //        throw new Exception("Can not Delete Test Run!");
        //    }
        //    else
        //    {
        //        base.OnDeleting();
        //    }

        //}
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
    }
}
