using System;
using System.ComponentModel;

using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [RuleCriteria("CheckTicketLength", DefaultContexts.Save, "Len(Trim(TicketNumber)) ==14", CustomMessageTemplate = "Ticket# must follow pattern TYYYYMMDD.xxxx")]
    [NonPersistent()]
    public class AriaAudit : XPLiteObject
    {
        public const string TicketRequiredMessage = "Ticket# is required ! Please use Audit button to enter it.";

        public AriaAudit(Session session)
            : base(session)
        {
            // This constructor is used when an object is loaded from a persistent storage.
            // Do not place any code here or place it only when the IsLoading property is false:
            // if (!IsLoading){
            //    It is now OK to place your initialization code here.
            // }
            // or as an alternative, move your initialization code into the AfterConstruction method.
        }

        [Key(AutoGenerate = true), Persistent()]
        private int ID { get; set; }

        [RuleRegularExpression("CheckTicketFormat", DefaultContexts.Save, @"T201\d(0[1-9]|1[012])([012][1-9]|[123]0|31)\.\d{4}", CustomMessageTemplate = "Ticket# must follow pattern TYYYYMMDD.xxxx")]
        [RuleRequiredField()]
        public string TicketNumber
        {
            get { return GetPropertyValue<string>("TicketNumber"); }
            set { SetPropertyValue<string>("TicketNumber", value != null ? value.ToUpper() : value); }
        }

        public string TrackingEntry
        {
            get { return GetPropertyValue<string>("TrackingEntry"); }
            set { SetPropertyValue("TrackingEntry", value); }
        }
        [Size(SizeAttribute.Unlimited)]
        public string Comments { get; set; }
    }

}