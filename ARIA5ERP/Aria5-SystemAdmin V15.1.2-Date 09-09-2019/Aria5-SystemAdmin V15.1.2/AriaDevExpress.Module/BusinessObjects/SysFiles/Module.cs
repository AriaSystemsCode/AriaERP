using System;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using System.ComponentModel;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    public class Module : XPLiteObject
    {
        [Key(false), Size(2)]
        public string ModuleName
        {
            get { return GetPropertyValue<string>("ModuleName"); }
            set { SetPropertyValue<string>("ModuleName", value); }
        }

        [Size(45)]
        public string ModuleDescription
        {
            get { return GetPropertyValue<string>("ModuleDescription"); }
            set { SetPropertyValue<string>("ModuleDescription", value); }
        }

        [Association("Module-File")]
        public XPCollection<File> Files
        {
            get
            {
                return GetCollection<File>("Files");
            }
        }

        [Association("Menu-Module")]
        public XPCollection<Menu> Menus
        {
            get
            {
                return GetCollection<Menu>("Menus");
            }
        }

        [Association("ReportCalledFrom-Modules"), Browsable(false)]
        public XPCollection<Report> ReportCalledFrom { get { return GetCollection<Report>("ReportCalledFrom"); } }


        protected override void OnSaved()
        {
            foreach (File file in Files)
            {
                file.Application = file.ReCalculateModules();

                if (BO.ValidateWithoutTicket(file, ContextIdentifier.Save))
                    Session.CommitTransaction();
            }

            foreach (Menu menu in Menus)
            {
                menu.cbarmodule = menu.ReCalculateModules();
                if (BO.ValidateWithoutTicket(menu, ContextIdentifier.Save))
                    Session.CommitTransaction();
            }
            base.OnSaved();
        }

        public override string ToString()
        {
            if (string.IsNullOrWhiteSpace(ModuleDescription))
                return ModuleName;
            else
                return ModuleName + " - " + ModuleDescription;
        }


        public Module(Session session) : base(session) { }
        public Module() : base(Session.DefaultSession) { }
        public override void AfterConstruction() { base.AfterConstruction(); }
    }
}
