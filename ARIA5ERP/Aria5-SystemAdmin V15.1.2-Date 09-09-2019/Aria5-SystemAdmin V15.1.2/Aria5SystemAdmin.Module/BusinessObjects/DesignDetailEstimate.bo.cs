using System;
using System.Collections.Generic;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class DesignDetailEstimate
    {
        //ATA calculate edit end date 
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
    }
}
