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
    public partial class ReportVariableBrowseFieldsController : ViewController
    {
        public ReportVariableBrowseFieldsController()
        {
            InitializeComponent();
            RegisterActions(components);
        }

        string browseFields;
        private void BrowseFieldsShowAction_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objSpace = Application.CreateObjectSpace();
            ReportVariableBrowseFieldSelector reportVariable = objSpace.CreateObject<ReportVariableBrowseFieldSelector>();
            reportVariable.SetVariable(View.CurrentObject as ReportVariable);
            DetailView targetView = Application.CreateDetailView(objSpace, reportVariable);
            targetView.ViewEditMode = DevExpress.ExpressApp.Editors.ViewEditMode.Edit;
            e.View = targetView;
        }

        private void BrowseFieldsShowAction_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {

        }

        private void BrowseFieldsShowAction_Cancel(object sender, EventArgs e)
        {
            ((ReportVariable)View.CurrentObject).BrowseFields = browseFields;
        }

    }
}
