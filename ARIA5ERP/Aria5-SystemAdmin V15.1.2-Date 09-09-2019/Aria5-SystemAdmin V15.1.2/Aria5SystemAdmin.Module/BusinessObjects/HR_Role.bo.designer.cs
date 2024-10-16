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
    [RelatedEntity("Aria5-SystemAdmin-HRRole")]
    public partial class HR_Role : DevExpress.Persistent.BaseImpl.BaseObject
    {
        public HR_Role(DevExpress.Xpo.Session session)
            : base(session)
        {

        }
        private string _name;

        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }

        private string _description;

        public string Description
        {
            get { return _description; }
            set { _description = value; }
        }
        [Association("HRRole-Activities")]
        public XPCollection<HRActivity> Activities
        {
            get
            {
                return GetCollection<HRActivity>("Activities");
            }
        }
        [Association("HRRole-Categories")]
        public XPCollection<HR_ActivityCategory> Categories
        {
            get
            {
                return GetCollection<HR_ActivityCategory>("Categories");
            }
        }
    }
}
