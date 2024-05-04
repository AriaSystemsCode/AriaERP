namespace Aria5SystemAdmin.Module.Controllers
{
    partial class AriaObjectShelveMethodViewController
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
            this.SelectMethodsToDelete = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            this.SelectMethodsToModify = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // SelectMethodsToDelete
            // 
            this.SelectMethodsToDelete.AcceptButtonCaption = null;
            this.SelectMethodsToDelete.CancelButtonCaption = null;
            this.SelectMethodsToDelete.Caption = "Select Methods To Delete";
            this.SelectMethodsToDelete.ConfirmationMessage = null;
            this.SelectMethodsToDelete.Id = "AriaObjectShelveMethodSelectMethodsToDelete";
            this.SelectMethodsToDelete.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethod);
            this.SelectMethodsToDelete.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.SelectMethodsToDelete.ToolTip = null;
            this.SelectMethodsToDelete.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.SelectMethodsToDelete.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectMethodsToDelete_CustomizePopupWindowParams);
            this.SelectMethodsToDelete.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectMethodsToDelete_Execute);
            // 
            // SelectMethodsToModify
            // 
            this.SelectMethodsToModify.AcceptButtonCaption = null;
            this.SelectMethodsToModify.CancelButtonCaption = null;
            this.SelectMethodsToModify.Caption = "Select Methods To Modify";
            this.SelectMethodsToModify.ConfirmationMessage = null;
            this.SelectMethodsToModify.Id = "AriaObjectShelveMethodSelectMethodsToModify";
            this.SelectMethodsToModify.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethod);
            this.SelectMethodsToModify.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.SelectMethodsToModify.ToolTip = null;
            this.SelectMethodsToModify.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.SelectMethodsToModify.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectMethodsToModify_CustomizePopupWindowParams);
            this.SelectMethodsToModify.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectMethodsToModify_Execute);
            // 
            // AriaObjectShelveMethodViewController
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethod);
            this.TypeOfView = typeof(DevExpress.ExpressApp.View);
            this.Activated += new System.EventHandler(this.AriaObjectShelveMethodViewController_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectMethodsToDelete;
        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectMethodsToModify;
    }
}
