using System;
using System.ComponentModel;

using DevExpress.Xpo;
using DevExpress.Data.Filtering;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.ExpressApp.Editors;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    [DevExpress.Xpo.Custom("Caption", "Report Valid Entry")]
    [Persistent("Syrepuvr_ValidEntry")]
    public class ReportVariableValidEntry : XPLiteObject
    {
        public ReportVariableValidEntry(Session session)
            : base(session)
        {
            // This constructor is used when an object is loaded from a persistent storage.
            // Do not place any code here or place it only when the IsLoading property is false:
            // if (!IsLoading){
            //    It is now OK to place your initialization code here.
            // }
            // or as an alternative, move your initialization code into the AfterConstruction method.
        }

        [Key(AutoGenerate = true)]
        [Persistent()]
        [Browsable(false)]
        public int ID
        {
            get { return GetPropertyValue<int>("ID"); }
            set { SetPropertyValue("ID", value); }
        }

        [Association("ReportVariable-ValidEntries")]
        public ReportVariable Variable { get; set; }

        public string Name
        {
            get { return GetPropertyValue<string>("Name"); }
            set { SetPropertyValue("Name", value); }
        }

        [Size(100)]
        public string Code
        {
            get { return GetPropertyValue<string>("Code"); }
            set { SetPropertyValue("Code", value); }
        }

        public override void AfterConstruction()
        {
            base.AfterConstruction();
            // Place here your initialization code.
        }
    }

}