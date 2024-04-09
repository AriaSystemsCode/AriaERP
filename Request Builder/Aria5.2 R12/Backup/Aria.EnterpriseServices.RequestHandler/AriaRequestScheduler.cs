using System;
using Aria.DataTypes.RequestHandler;

namespace Aria.EnterpriseServices.RequestHandler
{
    /// <summary>
    /// This class responsible on run request depend on Schedule (this Schedule may be every day or once per days or week per days).
    /// </summary>
    public class AriaRequestScheduler
    {
        public bool CheckNextChildRequestDateTime(AriaRequest request)
        {
            Xml.AriaXmlSerializer ser = new Aria.Xml.AriaXmlSerializer();
            AriaRequest newRequest = (AriaRequest)ser.ConvertFromXml(ser.ConvertToXml(request));

            Schedule(newRequest);

            if (newRequest.StartAfterDate.Date == request.StartAfterDate.Date &&
                newRequest.StartAfterDate.Date.Add(new TimeSpan(newRequest.RequestStartTime.Hour, newRequest.RequestStartTime.Minute, newRequest.RequestStartTime.Second)) < DateTime.Now)
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        public void Schedule(AriaRequest request)
        {
            switch (request.RecurrenceType)
            {
                case AriaRequestRecurrenceTypes.Daily:
                    this.SetDailyRequestNextChildDateTime(request);
                    break;

                case AriaRequestRecurrenceTypes.Weekly:
                    this.SetWeeklyRequestNextDateTime(request);
                    break;

                case AriaRequestRecurrenceTypes.Monthly:
                    this.SetMonthlyRequestNextDateTime(request);
                    break;
            }
        }

        private void SetDailyRequestNextChildDateTime(AriaRequest request)
        {
            switch (request.DailyType)
            {
                #region EveryDay
                case AriaRequestDailyTypes.EveryDay:
                    if (request.Status == AriaRequestStatusTypes.OnHold)
                    {
                        SetNextRequestInfo(request, GetRequestStartAfterDateTime(request));
                    }
                    else
                    {
                        SetNextRequestInfo(request, request.NextChildRequestDateTime.AddDays(1));
                    }
                    break;
                #endregion EveryDay

                #region OncePerDays
                case AriaRequestDailyTypes.OncePerDays:
                    if (request.Status == AriaRequestStatusTypes.OnHold)
                    {
                        SetNextRequestInfo(request, GetRequestStartAfterDateTime(request));
                    }
                    else
                    {
                        SetNextRequestInfo(request, request.NextChildRequestDateTime.AddDays(request.OncePerDays));
                    }
                    break;
                #endregion OncePerDays

                #region WeekDays
                case AriaRequestDailyTypes.WeekDays:
                    if (request.Status == AriaRequestStatusTypes.OnHold)
                    {
                        if (request.StartAfterDate.DayOfWeek == DayOfWeek.Saturday)
                        {
                            SetNextRequestInfo(request, GetRequestStartAfterDateTime(request).AddDays(2));
                        }
                        else if (request.StartAfterDate.DayOfWeek == DayOfWeek.Sunday)
                        {
                            SetNextRequestInfo(request, GetRequestStartAfterDateTime(request).AddDays(1));
                        }
                        else
                        {
                            SetNextRequestInfo(request, GetRequestStartAfterDateTime(request));
                        }
                    }
                    else
                    {
                        if (request.NextChildRequestDateTime.DayOfWeek == DayOfWeek.Friday)
                        {
                            SetNextRequestInfo(request, request.NextChildRequestDateTime.AddDays(3));
                        }
                        else if (request.NextChildRequestDateTime.DayOfWeek == DayOfWeek.Saturday)
                        {
                            SetNextRequestInfo(request, request.NextChildRequestDateTime.AddDays(2));
                        }
                        else
                        {
                            SetNextRequestInfo(request, request.NextChildRequestDateTime.AddDays(1));
                        }
                    }
                    break;
                #endregion WeekDays
            }
        }

        private void SetWeeklyRequestNextDateTime(AriaRequest request)
        {
            if (request.Status == AriaRequestStatusTypes.OnHold)
            {
                for (int index = 0; index <= 7; index++)
                {
                    if (request.Days.Contains(GetRequestStartAfterDateTime(request).AddDays(index).DayOfWeek.ToString()))
                    {
                        SetNextRequestInfo(request, GetRequestStartAfterDateTime(request).AddDays(index)); 
                        break;
                    }
                }
            }
            else
            {
                int addDays = 7 * (request.OncePerWeeks - 1);
                
                for (int index = 1; index <= 7; index++)
                {
                    if (request.Days.Contains(request.NextChildRequestDateTime.AddDays(index).DayOfWeek.ToString()))
                    {
                        if (request.NextChildRequestDateTime.AddDays(index).DayOfWeek > request.NextChildRequestDateTime.DayOfWeek)
                        {

                            SetNextRequestInfo(request, request.NextChildRequestDateTime.AddDays(index));
                        }
                        else
                        {
                            SetNextRequestInfo(request, request.NextChildRequestDateTime.AddDays(index + addDays));
                        }
                        break;
                    }
                }
            }
        }

        private void SetMonthlyRequestNextDateTime(AriaRequest request)
        {
            switch (request.MonthlyType)
            {
                #region MonthDay
                case AriaRequestMonthlyTypes.MonthDay:
                    if (request.Status == AriaRequestStatusTypes.OnHold)
                    {
                        for (int monthIndex = 0; monthIndex <= 12; monthIndex++)
                        {
                            if (request.Months.Contains(((AriaYearMonths)
                                    GetRequestStartAfterDateTime(request).AddMonths(monthIndex).Month).ToString()))
                            {

                                DateTime dayInMonth = new DateTime(GetRequestStartAfterDateTime(request).AddMonths(monthIndex).Year,
                                                                     GetRequestStartAfterDateTime(request).AddMonths(monthIndex).Month,
                                                                       request.MonthDay,
                                                                        request.RequestStartTime.Hour,
                                                                           request.RequestStartTime.Minute,
                                                                               request.RequestStartTime.Second);

                                SetNextRequestInfo(request, dayInMonth);

                                break;
                            }
                        }
                    }
                    else
                    {
                        for (int monthIndex = 1; monthIndex <= 12; monthIndex++)
                        {
                            if (request.Months.Contains(((AriaYearMonths)request.NextChildRequestDateTime.AddMonths(monthIndex).Month).ToString()))
                            {
                                SetNextRequestInfo(request, request.NextChildRequestDateTime.AddMonths(monthIndex));
                                break;
                            }
                        }
                    }

                    break;
                #endregion MonthDay

                #region MonthWeekDay
                case AriaRequestMonthlyTypes.MonthWeekDay:

                    int index = 0;
                    DateTime tempDateTime = DateTime.MinValue;

                    if (request.Status == AriaRequestStatusTypes.OnHold)
                    {
                        index = 0;
                        tempDateTime = GetRequestStartAfterDateTime(request);
                    }
                    else
                    {
                        index = 1;
                        tempDateTime = request.NextChildRequestDateTime;
                    }

                    for (; index <= 12; index++)
                    {                                                
                        if (request.Months.Contains(((AriaYearMonths)tempDateTime.AddMonths(index).Month).ToString()))
                        {
                            DateTime firstDayInMonth = new DateTime(tempDateTime.AddMonths(index).Year,
                                                                        tempDateTime.AddMonths(index).Month,
                                                                            1, request.RequestStartTime.Hour,
                                                                                request.RequestStartTime.Minute,
                                                                                    request.RequestStartTime.Second);

                            DateTime firstDayInTargetWeekInMonth = firstDayInMonth.AddDays((7 * (int)request.MonthWeek) - 1);

                            while (!(firstDayInTargetWeekInMonth = firstDayInTargetWeekInMonth.AddDays(1)).DayOfWeek.Equals(request.MonthWeekDay)) ;

                            if (!firstDayInTargetWeekInMonth.Month.Equals(firstDayInMonth.Month))
                            {
                                firstDayInTargetWeekInMonth = firstDayInMonth.AddDays((7 * (int)AriaMonthWeeks.Fourth) - 1);
                                
                                while (!(firstDayInTargetWeekInMonth = firstDayInTargetWeekInMonth.AddDays(1)).DayOfWeek.Equals(request.MonthWeekDay)) ;
                            }

                            SetNextRequestInfo(request, firstDayInTargetWeekInMonth);
                                                        
                            break;
                        }
                    }
                    break;
                #endregion MonthWeekDay
            }
        }

        private DateTime GetRequestStartAfterDateTime(AriaRequest request)
        {
            return request.StartAfterDate.AddHours(request.RequestStartTime.Hour).
                                                    AddMinutes(request.RequestStartTime.Minute).
                                                        AddSeconds(request.RequestStartTime.Second);
        }

        private void SetNextRequestInfo(AriaRequest request, DateTime nextRequestDateTime)
        {
            if (IsRequestEnded(request, nextRequestDateTime) == true)
            {
                request.Occurence++;

                request.Status = AriaRequestStatusTypes.Ended;
            }
            else
            {
                request.Occurence++;
                
                request.NextChildRequestDateTime = nextRequestDateTime;
            }
        }

        private bool IsRequestEnded(AriaRequest request, DateTime nextRequestDataTime)
        {
            if (request.EndRequestType == AriaRequestEndAfterTypes.AfterOccurence)
            {
                return (request.Occurence + 1 >= request.EndAfterOccurence);
            }
            else if (request.EndRequestType == AriaRequestEndAfterTypes.AfterDate)
            {
                return (nextRequestDataTime.Date > request.EndAfterDate);
            }
            else
            {
                return false;
            }
        }
    }
}
