using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aria5SystemAdmin.Module
{
   public static class CalcEndate
    {

       public static DateTime CalcEnddate(DateTime StartDatetime, int MonthesCount)
       {
          // DateTime end = DateTime.Now;
          // DateTime firstDayafterNMonth = StartDatetime.AddDays(-StartDatetime.Day + 1).AddMonths(MonthesCount);
          // int diff = 7 - (int)firstDayafterNMonth.DayOfWeek;
          //end = firstDayafterNMonth.AddDays(diff);

           
           DateTime end = StartDatetime.AddDays(-StartDatetime.Day + 1).AddMonths(MonthesCount);
          return ((7 - (int)end.DayOfWeek) == 7)?  end: end.AddDays((7 - (int)end.DayOfWeek));
          // end = firstDayafterNMonth.AddDays(diff);
          // return end;
       }
    }
}
