   using System;
using System.ComponentModel;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.Persistent.Base;
using AriaDevExpress.Module.BusinessObjects.SysFiles;

namespace AriaDevExpress.Module.Controllers.SysFiles
{
    public partial class ReportVariablesSortController : ViewController
    {
        public ReportVariablesSortController()
        {
            InitializeComponent();
            RegisterActions(components);
        }

        Report CurrentReport;
        private void ReportVariableSortShowAction_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objSpace = Application.CreateObjectSpace();
            ReportVariablesSorterEmptyClass obj = new ReportVariablesSorterEmptyClass();
            CurrentReport = View.CurrentObject as Report;
            obj.SetReport(CurrentReport);
            DetailView targetView = Application.CreateDetailView(objSpace, obj);
            e.View = targetView;
        }

        //private void ReportVariableSortShowAction_Cancel(object sender, EventArgs e)
        //{  
        //    CurrentReport.variables.Session.DropChanges();
        //    CurrentReport.variables.Reload();
         
        //    if (((DetailView)View).ViewEditMode == DevExpress.ExpressApp.Editors.ViewEditMode.View)
        //    {
        //        CurrentReport.Save();
        //        CurrentReport.Session.CommitTransaction();
        //    }
        //}

        private void ReportVariableSortShowAction_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            if (((DetailView)View).ViewEditMode == DevExpress.ExpressApp.Editors.ViewEditMode.View)
            {
                CurrentReport.Save();
                CurrentReport.Session.CommitTransaction();
            }
        }
    }
}
