using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Persistent.BaseImpl;
using DevExpress.ExpressApp.Utils;
using DevExpress.Xpo;
using DevExpress.Persistent.Base;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [DevExpress.Xpo.Custom("Caption", "Aria Audit")]
    public class AriaAuditDataPersistent : AuditDataItemPersistent
    {

        public AriaAuditDataPersistent(DevExpress.Xpo.Session session) : base(session) { }

        public AriaAuditDataPersistent(DevExpress.Xpo.Session session, string userName, DateTime modifiedOn, string description) : base(session, userName, modifiedOn, description) { }

        [Persistent()]
        [Size(SizeAttribute.Unlimited)]
        public string Comments
        {
            get { return GetPropertyValue<string>("Comments"); }
            set { SetPropertyValue("Comments", value); }
        }

        [DevExpress.Xpo.DisplayName("Ticket #")]
        [DevExpress.Xpo.Persistent()]
        //  [VisibleInListView(true)]
        //  [VisibleInLookupListView(true)]
        public string Ticket
        {
            get { return GetPropertyValue<string>("Ticket"); }
            set { SetPropertyValue("Ticket", value); }
        }

        public string TrackingEntry
        {
            get { return GetPropertyValue<string>("TrackingEntry"); }
            set { SetPropertyValue("TrackingEntry", value); }
        }


        void SetData()
        {
            if (this.AuditedObject != null && this.AuditedObject.AuditDataItems != null)
            {
                var ticketQuery = Session.GetObjectsToSave(true).OfType<AriaAuditDataPersistent>().Where(AuditItem => AuditItem.OperationType == "CustomData" && AuditItem.OldValue == "Ticket");// this.AuditedObject.AuditDataItems.Where(auditItem => auditItem.OldValue == "Ticket");
                if (ticketQuery != null && ticketQuery.Count() > 0)
                    Ticket = ticketQuery.First().NewValue;

                var commentsQuery = Session.GetObjectsToSave(true).OfType<AriaAuditDataPersistent>().Where(AuditItem => AuditItem.OperationType == "CustomData" && AuditItem.OldValue == "Comments");//this.AuditedObject.AuditDataItems.Where(auditItem => auditItem.OperationType == "CustomData" && auditItem.OldValue == "Comments");
                if (commentsQuery != null && commentsQuery.Count() > 0)
                    Comments = commentsQuery.First().NewValue;

                var TrackingEntryQuery = Session.GetObjectsToSave(true).OfType<AriaAuditDataPersistent>().Where(AuditItem => AuditItem.OperationType == "CustomData" && AuditItem.OldValue == "TrackingEntry");//this.AuditedObject.AuditDataItems.Where(auditItem => auditItem.OperationType == "CustomData" && auditItem.OldValue == "Comments");
                if (TrackingEntryQuery != null && TrackingEntryQuery.Count() > 0)
                    TrackingEntry = TrackingEntryQuery.First().NewValue;
            }
        }

        protected override void OnSaving()
        {
            SetData();
            base.OnSaving();
        }
    }
}
