using AriaDevExpress.Module.BusinessObjects.SysFiles;
namespace AriaDevExpress.Module.Controllers.SysFiles
{
    partial class BrowseFieldsController
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
            this.BrowseFieldsShowAction = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // BrowseFieldsShowAction
            // 
            this.BrowseFieldsShowAction.AcceptButtonCaption = null;
            this.BrowseFieldsShowAction.CancelButtonCaption = null;
            this.BrowseFieldsShowAction.Caption = "Browse Fields";
            this.BrowseFieldsShowAction.ConfirmationMessage = null;
            this.BrowseFieldsShowAction.Id = "BrowseFieldsShowAction";
            this.BrowseFieldsShowAction.ImageName = null;
            this.BrowseFieldsShowAction.Shortcut = null;
            this.BrowseFieldsShowAction.Tag = null;
            this.BrowseFieldsShowAction.TargetObjectsCriteria = null;
            this.BrowseFieldsShowAction.TargetViewId = null;
            this.BrowseFieldsShowAction.ToolTip = null;
            this.BrowseFieldsShowAction.TypeOfView = null;
            this.BrowseFieldsShowAction.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.BrowseFieldsShowAction_CustomizePopupWindowParams);
            this.BrowseFieldsShowAction.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.BrowseFieldsShowAction_Execute);
            // 
            // BrowseFieldsController
            // 
            this.TargetObjectType = typeof(File);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction BrowseFieldsShowAction;
    }
}
