using AriaDevExpress.Module.BusinessObjects.SysFiles;
namespace AriaDevExpress.Module.Controllers.SysFiles
{
    partial class AuditController
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
            this.ShowAriaAuditAction = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // ShowAriaAuditAction
            // 
            this.ShowAriaAuditAction.AcceptButtonCaption = null;
            this.ShowAriaAuditAction.ActionMeaning = DevExpress.ExpressApp.Actions.ActionMeaning.Accept;
            this.ShowAriaAuditAction.CancelButtonCaption = null;
            this.ShowAriaAuditAction.Caption = "Audit";
            this.ShowAriaAuditAction.ConfirmationMessage = null;
            this.ShowAriaAuditAction.Id = "ShowAriaAuditAction";
            this.ShowAriaAuditAction.ImageName = null;
            this.ShowAriaAuditAction.Shortcut = null;
            this.ShowAriaAuditAction.Tag = null;
            this.ShowAriaAuditAction.TargetObjectsCriteria = null;
            this.ShowAriaAuditAction.TargetViewId = null;
            this.ShowAriaAuditAction.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.ShowAriaAuditAction.ToolTip = null;
            this.ShowAriaAuditAction.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.ShowAriaAuditAction.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.ShowAriaAuditAction_CustomizePopupWindowParams);
            this.ShowAriaAuditAction.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.ShowAriaAuditAction_Execute);

                     // 
            // AuditController
            // 
            this.TargetObjectType = typeof(IAuditable);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction ShowAriaAuditAction;
    }
}
