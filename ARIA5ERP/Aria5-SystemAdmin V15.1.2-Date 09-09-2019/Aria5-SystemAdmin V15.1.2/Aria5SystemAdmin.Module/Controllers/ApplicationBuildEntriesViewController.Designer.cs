namespace Aria5SystemAdmin.Module.Controllers
{
    partial class ApplicationBuildEntriesViewController
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
            this.AppBuildLinkController = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // AppBuildLinkController
            // 
            this.AppBuildLinkController.AcceptButtonCaption = null;
            this.AppBuildLinkController.CancelButtonCaption = null;
            this.AppBuildLinkController.Caption = "Link";
            this.AppBuildLinkController.ConfirmationMessage = null;
            this.AppBuildLinkController.Id = "AppBuildLinkController";
            this.AppBuildLinkController.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.ApplicationBuildEntries_T);
            this.AppBuildLinkController.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.AppBuildLinkController.ToolTip = null;
            this.AppBuildLinkController.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.AppBuildLinkController.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.AppBuildLinkController_CustomizePopupWindowParams);
            this.AppBuildLinkController.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.AppBuildLinkController_Execute);
            // 
            // ApplicationBuildEntriesViewController1
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.ApplicationBuildEntries_T);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction AppBuildLinkController;
    }
}
