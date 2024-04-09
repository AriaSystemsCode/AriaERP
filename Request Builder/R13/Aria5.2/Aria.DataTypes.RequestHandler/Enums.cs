namespace Aria.DataTypes.RequestHandler
{
    public enum AriaLoginTypes
    {
        Aria,
        Others,
        NotSet
    }

    public enum AriaRequestPriorityTypes
    {
        AboveNormal,
        BelowNormal,
        Highest,
        Lowest,
        Normal
    }

    public enum AriaRequestRecurrenceTypes
    {
        Immediate,
        Once,
        Daily,
        Weekly,
        Monthly,
        ComputerStart,
        Event
    }

    public enum AriaRequestDailyTypes
    {
        EveryDay,
        WeekDays,
        OncePerDays,
        NotSet
    }

    public enum AriaRequestMonthlyTypes
    {
        MonthDay,
        MonthWeekDay,
        NotSet
    }

    public enum AriaRequestRepeatUntilTypes
    {
        Time,
        Duration,
        NotSet
    }

    public enum AriaRequestStatusTypes
    {
        OnHold,
        Started,
        Ended,
        Running,
        Completed,
        Canceled,
        Failed,
        Removed
    }

    public enum AriaMonthWeeks
    {
        First = 0,
        Second = 1,
        Third = 2,
        Fourth = 3,
        Last = 4,
        NotSet
    }

    public enum AriaYearMonths
    {
        NotSet = 0,
        January = 1,
        February = 2,
        March = 3,
        April = 4,
        May = 5,
        June = 6,
        July = 7,
        August = 8,
        September = 9,
        October = 10,
        November = 11,
        December = 12,
    }

    public enum AriaRequestEndAfterTypes
    {
        NotEnded,
        AfterOccurence,
        AfterDate
    }

    public enum AriaDayOfWeek
    {
        Monday = 1,
        Tuesday = 2,
        Wednesday = 3,
        Thursday = 4,
        Friday = 5,
        Saturday = 6,
        Sunday = 7,
        NotSet
   }
}