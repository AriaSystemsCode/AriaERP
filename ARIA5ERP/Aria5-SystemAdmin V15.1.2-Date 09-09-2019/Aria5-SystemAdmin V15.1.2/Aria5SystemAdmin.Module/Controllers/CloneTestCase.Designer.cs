namespace Aria5SystemAdmin.Module.Controllers
{
    partial class CloneTestCase
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.SelectTestCases = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // SelectTestCases
            // 
            this.SelectTestCases.AcceptButtonCaption = null;
            this.SelectTestCases.CancelButtonCaption = null;
            this.SelectTestCases.Caption = "Select Test Cases";
            this.SelectTestCases.ConfirmationMessage = null;
            this.SelectTestCases.Id = "SelectTestCases";
            this.SelectTestCases.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TestRun);
            this.SelectTestCases.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.SelectTestCases.ToolTip = null;
            this.SelectTestCases.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.SelectTestCases.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectTestCases_CustomizePopupWindowParams);
            this.SelectTestCases.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectTestCases_Execute);
            // 
            // CloneTestCase
            // 
            this.Actions.Add(this.SelectTestCases);
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TestRun);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectTestCases;
    }
}
