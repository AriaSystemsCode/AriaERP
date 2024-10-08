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
using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module;

namespace Aria5.DevExpress.OneTouchAway.Module.BusinessObjects
{
    [DefaultClassOptions]
    [IsClient(true, false)]
    [RelatedEntity("Aria5-Windows8Xaml-Log")]


    public partial class Log : ClientBaseObject
    {
        public Log(Session session)
            : base(session)
        {

        }
        private System.Guid _RecordOID;
        public System.Guid RecordOID
        {
            get
            {
                return _RecordOID;
            }
            set
            {
                SetPropertyValue("RecordOID", ref _RecordOID, value);
            }
        }

        private ClientEntityType _EntityTypeOID;
        public ClientEntityType EntityTypeOID
        {
            get
            {
                return _EntityTypeOID;
            }
            set
            {
                SetPropertyValue("EntityTypeOID", ref _EntityTypeOID, value);
            }
        }

        private System.String _ModificationType;
        public System.String ModificationType
        {
            get
            {
                return _ModificationType;
            }
            set
            {
                SetPropertyValue("ModificationType", ref _ModificationType, value);
            }
        }
        private System.String _TableName;
        public System.String TableName
        {
            get
            {
                return _TableName;
            }
            set
            {
                SetPropertyValue("TableName", ref _TableName, value);
            }
        }
        private DateTime _LogDateTime;
        public DateTime LogDateTime
        {
            get
            {
                return _LogDateTime;
            }
            set
            {
                SetPropertyValue("LogDateTime", ref _LogDateTime, value);
            }
        }

        //private AriaSecuritySystemUser _UserOID;
        //public AriaSecuritySystemUser UserOID
        //{
        //    get
        //    {
        //        return _UserOID;
        //    }
        //    set
        //    {
        //        SetPropertyValue("UserOID", ref _UserOID, value);
        //    }
        //}
        private System.String _UserName;
        public System.String UserName
        {
            get
            {
                return _UserName;
            }
            set
            {
                SetPropertyValue("UserName", ref _UserName, value);
            }
        }

        private System.String _Async;
        public System.String Async
        {
            get
            {
                return _Async;
            }
            set
            {
                SetPropertyValue("Async", ref _Async, value);
            }
        }
        private System.Boolean _HandledBySync;
        public System.Boolean HandledBySync
        {
            get
            {
                return _HandledBySync;
            }
            set
            {
                SetPropertyValue("HandledBySync", ref _HandledBySync, value);
            }
        }

        private System.String _TransactionTimeStamp;
        public System.String TransactionTimeStamp
        {
            get
            {
                return _TransactionTimeStamp;
            }
            set
            {
                SetPropertyValue("TransactionTimeStamp", ref _TransactionTimeStamp, value);
            }
        }
    }
}
