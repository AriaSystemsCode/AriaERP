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
using Aria5SystemAdmin.Module;

namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
    [DefaultClassOptions]
    [IsClient(true, false)]
    [RelatedEntity("Aria5-Windows8Xaml-TemporaryOrder")]
    public partial class TemporaryOrder : ClientBaseObject
    {
        public TemporaryOrder(Session session)
            : base(session)
        {

        }
        private ClientEntity _EntityOid;
        public ClientEntity EntityOid
        {
            get
            {
                return _EntityOid;
            }
            set
            {
                SetPropertyValue("EntityOid", ref _EntityOid, value);
            }
        }
        private System.Int16 _Quantity;
        public System.Int16 Quantity
        {
            get
            {
                return _Quantity;
            }
            set
            {
                SetPropertyValue("Quantity", ref _Quantity, value);
            }
        }

        private System.Double _PricePerQuantity;
        public System.Double PricePerQuantity
        {
            get
            {
                return _PricePerQuantity;
            }
            set
            {
                SetPropertyValue("PricePerQuantity", ref _PricePerQuantity, value);
            }
        }

        private System.String _ResourceId;
        [Size(100)]
        public System.String ResourceId
        {
            get
            {
                return _ResourceId;
            }
            set
            {
                SetPropertyValue("ResourceId", ref _ResourceId, value);
            }
        }

        private System.Double _TaxAmount;
        public System.Double TaxAmount
        {
            get
            {
                return _TaxAmount;
            }
            set
            {
                SetPropertyValue("TaxAmount", ref _TaxAmount, value);
            }
        }

        private System.Double _TaxPercentage;
        public System.Double TaxPercentage
        {
            get
            {
                return _TaxPercentage;
            }
            set
            {
                SetPropertyValue("TaxPercentage", ref _TaxPercentage, value);
            }
        }

        private System.String _Note;
        public System.String Note
        {
            get
            {
                return _Note;
            }
            set
            {
                SetPropertyValue("Note", ref _Note, value);
            }
        }

        // Sara.N,1 06-08-2016 Add new Fields in db[Start]
        private System.String _Status;
        [Size(35)]
        public System.String Status
        {
            get
            {
                return _Status;
            }
            set
            {
                SetPropertyValue("Status", ref _Status, value);
            }
        }

        private Guid _OrderOid;
        public Guid OrderOid
        {
            get
            {
                return _OrderOid;
            }
            set
            {
                SetPropertyValue("OrderOid", ref _OrderOid, value);
            }
        }

        private Guid _PermanentOrderLineOid;
        public Guid PermanentOrderLineOid
        {
            get
            {
                return _PermanentOrderLineOid;
            }
            set
            {
                SetPropertyValue("PermanentOrderLineOid", ref _PermanentOrderLineOid, value);
            }
        }
        // Sara.N,1 06-08-2016 Add new Fields in db[End]

    }
}
