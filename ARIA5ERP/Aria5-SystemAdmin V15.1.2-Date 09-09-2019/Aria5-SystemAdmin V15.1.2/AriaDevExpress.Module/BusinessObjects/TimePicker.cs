using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo;
using DevExpress.Xpo.Metadata;

namespace AriaDevExpress.Module.BusinessObjects.TimePicker
{

    public enum TimeValues
    {
        [DisplayName("0:00 AM")]
        Zero0AM,
        [DisplayName("0:15 AM")]
        Zero1AM,
        [DisplayName("0:30 AM")]
        Zero2AM,
        [DisplayName("0:45 AM")]
        Zero3AM,

        [DisplayName("1:00 AM")]
        One0AM,
        [DisplayName("1:15 AM")]
        One1AM,
        [DisplayName("1:30 AM")]
        One2AM,
        [DisplayName("1:45 AM")]
        One3AM,

        [DisplayName("2:00 AM")]
        Two0AM,
        [DisplayName("2:15 AM")]
        Two1AM,
        [DisplayName("2:30 AM")]
        Two2AM,
        [DisplayName("2:45 AM")]
        Two3AM,

        [DisplayName("3:00 AM")]
        Three0AM,
        [DisplayName("3:15 AM")]
        Three1AM,
        [DisplayName("3:30 AM")]
        Three2AM,
        [DisplayName("3:45 AM")]
        Three3AM,

        [DisplayName("4:00 AM")]
        Four0AM,
        [DisplayName("4:15 AM")]
        Four1AM,
        [DisplayName("4:30 AM")]
        Four2AM,
        [DisplayName("4:45 AM")]
        Four3AM,

        [DisplayName("5:00 AM")]
        Five0AM,
        [DisplayName("5:15 AM")]
        Five1AM,
        [DisplayName("5:30 AM")]
        Five2AM,
        [DisplayName("5:45 AM")]
        Five3AM,

        [DisplayName("6:00 AM")]
        Six0AM,
        [DisplayName("6:15 AM")]
        Six1AM,
        [DisplayName("6:30 AM")]
        Six2AM,
        [DisplayName("6:45 AM")]
        Six3AM,

        [DisplayName("7:00 AM")]
        Seven0AM,
        [DisplayName("7:15 AM")]
        Seven1AM,
        [DisplayName("7:30 AM")]
        Seven2AM,
        [DisplayName("7:45 AM")]
        Seven3AM,

        [DisplayName("8:00 AM")]
        Eight0AM,
        [DisplayName("8:15 AM")]
        Eight1AM,
        [DisplayName("8:30 AM")]
        Eight2AM,
        [DisplayName("8:45 AM")]
        Eight3AM,

        [DisplayName("9:00 AM")]
        Nine0AM,
        [DisplayName("9:15 AM")]
        Nine1AM,
        [DisplayName("9:30 AM")]
        Nine2AM,
        [DisplayName("9:45 AM")]
        Nine3AM,

        [DisplayName("10:00 AM")]
        Ten0AM,
        [DisplayName("10:15 AM")]
        Ten1AM,
        [DisplayName("10:30 AM")]
        Ten2AM,
        [DisplayName("10:45 AM")]
        Ten3AM,

        [DisplayName("11:00 AM")]
        Eleven0AM,
        [DisplayName("11:15 AM")]
        Eleven1AM,
        [DisplayName("11:30 AM")]
        Eleven2AM,
        [DisplayName("11:45 AM")]
        Eleven3AM,

        [DisplayName("12:00 PM")]
        Twelve0PM,
        [DisplayName("12:15 PM")]
        Twelve1PM,
        [DisplayName("12:30 PM")]
        Twelve2PM,
        [DisplayName("12:45 PM")]
        Twelve3PM,

        [DisplayName("1:00 PM")]
        One0PM,
        [DisplayName("1:15 PM")]
        One1PM,
        [DisplayName("1:30 PM")]
        One2PM,
        [DisplayName("1:45 PM")]
        One3PM,

        [DisplayName("2:00 PM")]
        Two0PM,
        [DisplayName("2:15 PM")]
        Two1PM,
        [DisplayName("2:30 PM")]
        Two2PM,
        [DisplayName("2:45 PM")]
        Two3PM,

        [DisplayName("3:00 PM")]
        Three0PM,
        [DisplayName("3:15 PM")]
        Three1PM,
        [DisplayName("3:30 PM")]
        Three2PM,
        [DisplayName("3:45 PM")]
        Three3PM,

        [DisplayName("4:00 PM")]
        Four0PM,
        [DisplayName("4:15 PM")]
        Four1PM,
        [DisplayName("4:30 PM")]
        Four2PM,
        [DisplayName("4:45 PM")]
        Four3PM,

        [DisplayName("5:00 PM")]
        Five0PM,
        [DisplayName("5:15 PM")]
        Five1PM,
        [DisplayName("5:30 PM")]
        Five2PM,
        [DisplayName("5:45 PM")]
        Five3PM,

        [DisplayName("6:00 PM")]
        Six0PM,
        [DisplayName("6:15 PM")]
        Six1PM,
        [DisplayName("6:30 PM")]
        Six2PM,
        [DisplayName("6:45 PM")]
        Six3PM,

        [DisplayName("7:00 PM")]
        Seven0PM,
        [DisplayName("7:15 PM")]
        Seven1PM,
        [DisplayName("7:30 PM")]
        Seven2PM,
        [DisplayName("7:45 PM")]
        Seven3PM,

        [DisplayName("8:00 PM")]
        Eight0PM,
        [DisplayName("8:15 PM")]
        Eight1PM,
        [DisplayName("8:30 PM")]
        Eight2PM,
        [DisplayName("8:45 PM")]
        Eight3PM,

        [DisplayName("9:00 PM")]
        Nine0PM,
        [DisplayName("9:15 PM")]
        Nine1PM,
        [DisplayName("9:30 PM")]
        Nine2PM,
        [DisplayName("9:45 PM")]
        Nine3PM,

        [DisplayName("10:00 PM")]
        Ten0PM,
        [DisplayName("10:15 PM")]
        Ten1PM,
        [DisplayName("10:30 PM")]
        Ten2PM,
        [DisplayName("10:45 PM")]
        Ten3PM,

        [DisplayName("11:00 PM")]
        Eleven0PM,
        [DisplayName("11:15 PM")]
        Eleven1PM,
        [DisplayName("11:30 PM")]
        Eleven2PM,
        [DisplayName("11:45 PM")]
        Eleven3PM
    }


    public class TimePickerValueConverter : ValueConverter
    {
        public override object ConvertFromStorageType(object value)
        {
            TimeValues result = TimeValues.Zero0AM;
            if (value != null && value is DateTime)
            {
                string val = ((DateTime)value).ToShortTimeString();
                foreach (TimeValues time in Enum.GetValues(typeof(TimeValues)))
                {
                    object[] attributes = time.GetType().GetMember(time.ToString())[0].GetCustomAttributes(typeof(DisplayNameAttribute), false);
                    if (((DisplayNameAttribute)attributes[0]).DisplayName == val)
                    {
                        result = time;
                        break;
                    }
                }
            }
            return result;
        }

        public override object ConvertToStorageType(object value)
        {
            TimeValues time = (TimeValues)value;
            object[] attributes = time.GetType().GetMember(time.ToString())[0].GetCustomAttributes(typeof(DisplayNameAttribute), false);
            DateTime result = DateTime.Parse(((DisplayNameAttribute)attributes[0]).DisplayName);
            return result;
        }

        public override Type StorageType
        {
            get
            {
                return typeof(DateTime);
            }
        }
    }
}
