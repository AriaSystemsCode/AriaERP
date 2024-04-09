using System;

namespace Aria.DataTypes.RequestHandler
{
    public partial class AriaRequest
    {
        private int _oncePerWeeks = 0;
        public int OncePerWeeks
        {
            get { return _oncePerWeeks; }
            set { _oncePerWeeks = value; }
        }

        private string _days = "";
        public string Days
        {
            get { return _days; }
            set { _days = value; }
        }

        public void SetWeeklyRequestSettings(DateTime requestTime,
                int oncePerWeeks, bool mon, bool tue, bool wed, bool thu, bool fri, bool sat, bool sun)
        {
            _recurrenceType = AriaRequestRecurrenceTypes.Weekly;
            _requestStartTime = requestTime;
            _oncePerWeeks = oncePerWeeks;

            _days =
                (mon == true ? DayOfWeek.Monday.ToString() + "," : "") +
                (tue == true ? DayOfWeek.Tuesday.ToString() + "," : "") +
                (wed == true ? DayOfWeek.Wednesday.ToString() + "," : "") +
                (thu == true ? DayOfWeek.Thursday.ToString() + "," : "") +
                (fri == true ? DayOfWeek.Friday.ToString() + "," : "") +
                (sat == true ? DayOfWeek.Saturday.ToString() + "," : "") +
                (sun == true ? DayOfWeek.Sunday.ToString() + "," : "");
        }
    }
}