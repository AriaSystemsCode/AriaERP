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
using System.ComponentModel;
using DevExpress.ExpressApp;
using DevExpress.Data.Filtering;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    [DefaultClassOptions]
    
    public partial class Reworkfeedback : DevExpress.Persistent.BaseImpl.BaseObject
    {
        public Reworkfeedback(DevExpress.Xpo.Session session)
            : base(session)
        {

        }
        private string _comment;
        [Size(5000)]
        public string Comment
        {
            get { return _comment; }
            set { _comment = value; }
        }
        private AriaSecuritySystemUser _addedby;
        public AriaSecuritySystemUser AddedBy
        {
            get
            {
                return _addedby;
            }
            set
            {
                _addedby = value;
            }
        }
        private Rework _rework;
        [Browsable(false)]
        [Association("Rework-Feedbacks")]
        public Rework Rework
        {
            get
            {
                return _rework;
            }
            set
            {
                _rework = value;
            }
        }
    }
}