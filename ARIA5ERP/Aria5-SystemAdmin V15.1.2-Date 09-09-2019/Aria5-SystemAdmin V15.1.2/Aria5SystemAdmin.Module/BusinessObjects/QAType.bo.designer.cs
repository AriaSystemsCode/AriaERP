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
    [RelatedEntity("Aria5-SystemAdmin-QAType")]
    public partial class QAType : DevExpress.Persistent.BaseImpl.BaseObject
    {
        public QAType(DevExpress.Xpo.Session session)
            : base(session)
        {

        }

        private System.String _Name;
        public System.String Name
        {
            get
            {
                return _Name;
            }
            set
            {
                SetPropertyValue("Name", ref _Name, value);
            }
        }

        private System.String _Description;
        public System.String Description
        {
            get
            {
                return _Description;
            }
            set
            {
                SetPropertyValue("Description", ref _Description, value);
            }
        }
    }
}
