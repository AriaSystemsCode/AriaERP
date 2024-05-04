namespace AriaDevExpress.Module.Controllers.SysFiles
{
    partial class ReportVariablesSortController
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
            this.ReportVariableSortShowAction = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // ReportVariableSortShowAction
            // 
            this.ReportVariableSortShowAction.AcceptButtonCaption = null;
            this.ReportVariableSortShowAction.ActionMeaning = DevExpress.ExpressApp.Actions.ActionMeaning.Accept;
            this.ReportVariableSortShowAction.CancelButtonCaption = null;
            this.ReportVariableSortShowAction.Caption = "Order Variables";
            this.ReportVariableSortShowAction.ConfirmationMessage = null;
            this.ReportVariableSortShowAction.Id = "ReportVariableSortShowAction";
            this.ReportVariableSortShowAction.ImageName = null;
            this.ReportVariableSortShowAction.Shortcut = null;
            this.ReportVariableSortShowAction.Tag = null;
            this.ReportVariableSortShowAction.TargetObjectsCriteria = null;
            this.ReportVariableSortShowAction.TargetObjectType = typeof(AriaDevExpress.Module.BusinessObjects.SysFiles.Report);
            this.ReportVariableSortShowAction.TargetViewId = null;
            this.ReportVariableSortShowAction.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.ReportVariableSortShowAction.ToolTip = null;
            this.ReportVariableSortShowAction.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.ReportVariableSortShowAction.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.ReportVariableSortShowAction_CustomizePopupWindowParams);
            this.ReportVariableSortShowAction.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.ReportVariableSortShowAction_Execute);
        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction ReportVariableSortShowAction;
    }
}
