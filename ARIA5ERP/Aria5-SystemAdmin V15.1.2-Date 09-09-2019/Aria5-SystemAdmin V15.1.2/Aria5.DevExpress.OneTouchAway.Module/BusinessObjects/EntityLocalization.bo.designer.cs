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
    [RelatedEntity("Aria5-Windows8Xaml-EntityLocalization")]


    public partial class EntityLocalization : Localization
    {
        public EntityLocalization(Session session)
            : base(session)
        {

        }
        private ClientEntity _Entity;
        public ClientEntity Entity
        {
            get
            {
                return _Entity;
            }
            set
            {
                SetPropertyValue("Entity", ref _Entity, value);
            }
        }

       
    }
}