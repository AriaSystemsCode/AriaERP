using System;

namespace Aria.DataTypes.RequestHandler
{
    public partial class AriaRequest
    {
        private AriaRequestDailyTypes _dailyType = AriaRequestDailyTypes.NotSet;
        public AriaRequestDailyTypes DailyType
        {
            get { return _dailyType; }
            set { _dailyType = value; }
        }

        private int _oncePerDays = 0;
        public int OncePerDays
        {
            get { return _oncePerDays; }
            set { _oncePerDays = value; }
        }

        public void SetEveryDayRequestSettings(DateTime requestTime)
        {
            _recurrenceType = AriaRequestRecurrenceTypes.Daily;
            _dailyType = AriaRequestDailyTypes.EveryDay;
            _requestStartTime = requestTime;
        }

        public void SetWeekDaysRequestSettings(DateTime requestTime)
        {
            _recurrenceType = AriaRequestRecurrenceTypes.Daily;
            _dailyType = AriaRequestDailyTypes.WeekDays;
            _requestStartTime = requestTime;
        }

        public void SetOncePerDaysRequestSettings(DateTime requestTime, int oncePerDays)
        {
            _recurrenceType = AriaRequestRecurrenceTypes.Daily;
            _dailyType = AriaRequestDailyTypes.OncePerDays;
            _requestStartTime = requestTime;
            _oncePerDays = oncePerDays;
        }
    }
}