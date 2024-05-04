using AriaDevExpress.Module.BusinessObjects.SysFiles;
namespace AriaDevExpress.Module.Controllers.SysFiles
{
    partial class ReportVariableBrowseFieldsController
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
            this.ReportVariableBrowseFieldsShowAction = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // ReportVariableBrowseFieldsShowAction
            // 
            this.ReportVariableBrowseFieldsShowAction.AcceptButtonCaption = null;
            this.ReportVariableBrowseFieldsShowAction.CancelButtonCaption = null;
            this.ReportVariableBrowseFieldsShowAction.Caption = "Browse Fields";
            this.ReportVariableBrowseFieldsShowAction.ConfirmationMessage = null;
            this.ReportVariableBrowseFieldsShowAction.Id = "ReportVariableBrowseFieldsShowAction";
            this.ReportVariableBrowseFieldsShowAction.ImageName = null;
            this.ReportVariableBrowseFieldsShowAction.Shortcut = null;
            this.ReportVariableBrowseFieldsShowAction.Tag = null;
            this.ReportVariableBrowseFieldsShowAction.TargetObjectsCriteria = null;
            this.ReportVariableBrowseFieldsShowAction.TargetObjectType = typeof(ReportVariable);
            this.ReportVariableBrowseFieldsShowAction.TargetViewId = null;
            this.ReportVariableBrowseFieldsShowAction.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.ReportVariableBrowseFieldsShowAction.ToolTip = null;
            this.ReportVariableBrowseFieldsShowAction.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.ReportVariableBrowseFieldsShowAction.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.BrowseFieldsShowAction_CustomizePopupWindowParams);
            this.ReportVariableBrowseFieldsShowAction.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.BrowseFieldsShowAction_Execute);
            this.ReportVariableBrowseFieldsShowAction.Cancel += new System.EventHandler(this.BrowseFieldsShowAction_Cancel);
            // 
            // ReportVariableBrowseFieldsController
            // 
            this.TargetObjectType = typeof(ReportVariable);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction ReportVariableBrowseFieldsShowAction;
    }
}
