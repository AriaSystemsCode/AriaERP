﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------
using System;
using System.Collections.Generic;

using System.Linq;

using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    [DefaultClassOptions]
    [XafDefaultProperty("Name")]
    [RelatedEntity("Aria5-SystemAdmin-TicketDetail")]
    public partial class TicketDetail : DevExpress.Persistent.BaseImpl.BaseObject
    {
        public TicketDetail(DevExpress.Xpo.Session session)
            : base(session)
        {

        }
        private System.Double _erpQ;
        private System.Double _edi;
        private System.Double _cwa;
        private System.Double _html5;
        private System.Double _pmoerpQ;
        private System.Double _pmoedi;
        private System.Double _pmocwa;
        private System.Double _pmohtml5;
        private TicketAHT _ticket;




        public System.Double QERP
        {
            get
            {
                return _erpQ;
            }
            set
            {
                SetPropertyValue("QERP", ref _erpQ, value);
            }
        }
        public System.Double EDI
        {
            get
            {
                return _edi;
            }
            set
            {
                SetPropertyValue("EDI", ref _edi, value);
            }
        }
        public System.Double CWA
        {
            get
            {
                return _cwa;
            }
            set
            {
                SetPropertyValue("CWA", ref _cwa, value);
            }
        }
        public System.Double Html5
        {
            get
            {
                return _html5;
            }
            set
            {
                SetPropertyValue("Html5", ref _html5, value);
            }
        }

        public System.Double QERPpmo
        {
            get
            {
                return _pmoerpQ;
            }
            set
            {
                SetPropertyValue("QERPpmo", ref _pmoerpQ, value);
            }
        }
        public System.Double EDIpmo
        {
            get
            {
                return _pmoedi;
            }
            set
            {
                SetPropertyValue("EDIpmo", ref _pmoedi, value);
            }
        }
        public System.Double CWApmo
        {
            get
            {
                return _pmocwa;
            }
            set
            {
                SetPropertyValue("CWApmo", ref _pmocwa, value);
            }
        }
        public System.Double Html5pmo
        {
            get
            {
                return _pmohtml5;
            }
            set
            {
                SetPropertyValue("Html5pmo", ref _pmohtml5, value);
            }
        }

       [DevExpress.Xpo.AssociationAttribute("TicketDetail-TicketAHT")]
        public TicketAHT Ticket
        {
            get { return _ticket; }
            set
            {
                SetPropertyValue("Ticket", ref _ticket, value);
            }
        }
    }
}