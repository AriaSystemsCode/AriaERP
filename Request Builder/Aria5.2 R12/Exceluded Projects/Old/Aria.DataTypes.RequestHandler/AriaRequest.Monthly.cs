using System;

namespace Aria.DataTypes.RequestHandler
{
    public partial class AriaRequest
    {
        private AriaRequestMonthlyTypes _monthlyType = AriaRequestMonthlyTypes.NotSet;
        public AriaRequestMonthlyTypes MonthlyType
        {
            get { return _monthlyType; }
            set { _monthlyType = value; }
        }

        private string _months = "";
        public string Months
        {
            get { return _months; }
            set { _months = value; }
        }

        private int _monthDay = 0;
        public int MonthDay
        {
            get { return _monthDay; }
            set { _monthDay = value; }
        }

        private DayOfWeek _monthWeekDay;
        public DayOfWeek MonthWeekDay
        {
            get { return _monthWeekDay; }
            set { _monthWeekDay = value; }
        }

        private AriaMonthWeeks _monthWeek;
        public AriaMonthWeeks MonthWeek
        {
            get { return _monthWeek; }
            set { _monthWeek = value; }
        }

        public void SetOnMonthDayRequestSettings(DateTime requestTime,
                int monthDay, bool jan, bool feb, bool mar, bool apr, bool may, bool jun, bool jul, bool aug,
                    bool sep, bool oct, bool nov, bool dec)
        {
            _recurrenceType = AriaRequestRecurrenceTypes.Monthly;
            _monthlyType = AriaRequestMonthlyTypes.MonthDay;
            _requestStartTime = requestTime;
            _monthDay = monthDay;
            _months = GetMonthsString(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec);        
        }

        public void SetOnMonthWeekDayRequestSettings(DateTime requestTime,
                AriaMonthWeeks monthWeek, DayOfWeek monthWeekDay, bool jan, bool feb, bool mar, bool apr, bool may,
                    bool jun, bool jul, bool aug, bool sep, bool oct, bool nov, bool dec)
        {
            _recurrenceType = AriaRequestRecurrenceTypes.Monthly;
            _monthlyType = AriaRequestMonthlyTypes.MonthWeekDay;
            _requestStartTime = requestTime;
            _monthWeek = monthWeek;
            _monthWeekDay = monthWeekDay;
            _months = GetMonthsString(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec);                
        }

        private string GetMonthsString(bool jan, bool feb, bool mar, bool apr, bool may, bool jun, bool jul,
                                        bool aug, bool sep, bool oct, bool nov, bool dec)
        {
            return  (jan == true ? AriaYearMonths.January.ToString() + "," : "") +
                    (feb == true ? AriaYearMonths.February.ToString() + "," : "") +
                    (mar == true ? AriaYearMonths.March.ToString() + "," : "") +
                    (apr == true ? AriaYearMonths.April.ToString() + "," : "") +
                    (may == true ? AriaYearMonths.May.ToString() + "," : "") +
                    (jun == true ? AriaYearMonths.June.ToString() + "," : "") +
                    (jul == true ? AriaYearMonths.July.ToString() + "," : "") +
                    (aug == true ? AriaYearMonths.August.ToString() + "," : "") +
                    (sep == true ? AriaYearMonths.September.ToString() + "," : "") +
                    (oct == true ? AriaYearMonths.October.ToString() + "," : "") +
                    (nov == true ? AriaYearMonths.November.ToString() + "," : "") +
                    (dec == true ? AriaYearMonths.December.ToString() + "," : "");
        }
    }
}