using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using System.ComponentModel;

namespace AriaDevExpress.Module.BusinessObjects.WebSite
{
    [DevExpress.Xpo.Custom("Caption", "Course Schedule")]
    public class ASPNETCoursePeriod : XPLiteObject
    {
        ASPNETCourses fCourseID;
        [Size(50)]
        [VisibleInDetailView(false)]
        [VisibleInListView(false)]
        [VisibleInLookupListView(false)]
        public ASPNETCourses CourseID
        {
            get { return fCourseID; }
            set { SetPropertyValue<ASPNETCourses>("CourseID", ref fCourseID, value); }
        }


        DateTime fDateFrom;
        [Custom("DisplayFormat", "{0: ddd, dd MMMM yyyy}")]
        [Custom("EditMask", "ddd, dd MMMM yyyy")]
        public DateTime DateFrom
        {
            get { return fDateFrom; }
            set { SetPropertyValue<DateTime>("DateFrom", ref fDateFrom, value); }
        }


        DateTime fDateTo;
        [Custom("DisplayFormat", "{0: ddd, dd MMMM yyyy}")]
        [Custom("EditMask", "ddd, dd MMMM yyyy")]
        public DateTime DateTo
        {
            get { return fDateTo; }
            set { SetPropertyValue<DateTime>("DateTo", ref fDateTo, value); }
        }


        TimePicker.TimeValues fTimeFrom;
        [ValueConverter(typeof(TimePicker.TimePickerValueConverter))]
        public TimePicker.TimeValues TimeFrom
        {
            get { return fTimeFrom; }
            set { SetPropertyValue<TimePicker.TimeValues>("TimeFrom", ref fTimeFrom, value); }
        }


        TimePicker.TimeValues fTimeTo;
        [ValueConverter(typeof(TimePicker.TimePickerValueConverter))]
        public TimePicker.TimeValues TimeTo
        {
            get { return fTimeTo; }
            set { SetPropertyValue<TimePicker.TimeValues>("TimeTo", ref fTimeTo, value); }
        }

        Guid frowguid;
        [Browsable(false)]
        public Guid rowguid
        {
            get { return frowguid; }
            set { SetPropertyValue<Guid>("rowguid", ref frowguid, value); }
        }

        int fID;
        [Key(true)]
        public int ID
        {
            get { return fID; }
            set { SetPropertyValue<int>("ID", ref fID, value); }
        }

        public ASPNETCoursePeriod(Session session) : base(session) { }
        public ASPNETCoursePeriod() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
