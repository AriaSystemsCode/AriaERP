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
    [RelatedEntity("Aria5-Windows8Xaml-EntityTypesRelationshipLocalization")]


    public partial class EntityTypesRelationshipLocalization : ClientBaseObject
    {
        public EntityTypesRelationshipLocalization(Session session)
            : base(session)
        {

        }

        private Language _Language;
        public Language Language
        {
            get
            {
                return _Language;
            }
            set
            {
                SetPropertyValue("Language", ref _Language, value);
            }
        }


        private System.String _LanguageId;
        public System.String LanguageId
        {
            get
            {
                return _LanguageId;
            }
            set
            {
                SetPropertyValue("LanguageId", ref _LanguageId, value);
            }
        }
        private System.String _EntityTypeId;
        public System.String EntityTypeId
        {
            get
            {
                return _EntityTypeId;
            }
            set
            {
                SetPropertyValue("EntityTypeId", ref _EntityTypeId, value);
            }
        }


        private ClientEntityTypesRelationship _EntityTypesRelationship;
        public ClientEntityTypesRelationship EntityTypesRelationship
        {
            get
            {
                return _EntityTypesRelationship;
            }
            set
            {
                SetPropertyValue("EntityTypesRelationship", ref _EntityTypesRelationship, value);
            }
        }


        private System.String _RelatedEntityTypeId;
        public System.String RelatedEntityTypeId
        {
            get
            {
                return _RelatedEntityTypeId;
            }
            set
            {
                SetPropertyValue("RelatedEntityTypeId", ref _RelatedEntityTypeId, value);
            }
        }

        private System.String _RelatedEntityTypeNameLocalized;
        public System.String RelatedEntityTypeNameLocalized
        {
            get
            {
                return _RelatedEntityTypeNameLocalized;
            }
            set
            {
                SetPropertyValue("RelatedEntityTypeNameLocalized", ref _RelatedEntityTypeNameLocalized, value);
            }
        }
    }
}