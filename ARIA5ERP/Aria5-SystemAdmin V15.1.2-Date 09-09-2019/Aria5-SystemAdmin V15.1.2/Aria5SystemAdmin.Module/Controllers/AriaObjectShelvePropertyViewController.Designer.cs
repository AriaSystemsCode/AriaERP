namespace Aria5SystemAdmin.Module.Controllers
{
    partial class AriaObjectShelvePropertyViewController
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
            this.SelectPropertiesToDelete = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            this.SelectPropertiesToModify = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // SelectPropertiesToDelete
            // 
            this.SelectPropertiesToDelete.AcceptButtonCaption = null;
            this.SelectPropertiesToDelete.CancelButtonCaption = null;
            this.SelectPropertiesToDelete.Caption = "Select Properties To Delete";
            this.SelectPropertiesToDelete.ConfirmationMessage = null;
            this.SelectPropertiesToDelete.Id = "AriaObjectShelvePropertySelectPropertiesToDelete";
            this.SelectPropertiesToDelete.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.SelectPropertiesToDelete.ToolTip = null;
            this.SelectPropertiesToDelete.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.SelectPropertiesToDelete.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectPropertiesToDelete_CustomizePopupWindowParams);
            this.SelectPropertiesToDelete.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectPropertiesToDelete_Execute);
            // 
            // SelectPropertiesToModify
            // 
            this.SelectPropertiesToModify.AcceptButtonCaption = null;
            this.SelectPropertiesToModify.CancelButtonCaption = null;
            this.SelectPropertiesToModify.Caption = "Select Properties To Modify";
            this.SelectPropertiesToModify.ConfirmationMessage = null;
            this.SelectPropertiesToModify.Id = "AriaObjectShelvePropertySelectPropertiesToModify";
            this.SelectPropertiesToModify.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.SelectPropertiesToModify.ToolTip = null;
            this.SelectPropertiesToModify.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.SelectPropertiesToModify.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectPropertiesToModify_CustomizePopupWindowParams);
            this.SelectPropertiesToModify.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectPropertiesToModify_Execute);
            // 
            // AriaObjectShelvePropertyViewController
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveProperty);
            this.ViewControlsCreated += new System.EventHandler(this.AriaObjectShelvePropertyViewController_ViewControlsCreated);
            this.AfterConstruction += new System.EventHandler(this.AriaObjectShelvePropertyViewController_AfterConstruction);
            this.Activated += new System.EventHandler(this.AriaObjectShelvePropertyViewController_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectPropertiesToModify;
        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectPropertiesToDelete;
    }
}
