namespace Aria5SystemAdmin.Module.Controllers
{
    partial class AriaObjectShelveViewController
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
            this.SelectObjectsToModify = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            this.SelectObjectsToDelete = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // SelectObjectsToModify
            // 
            this.SelectObjectsToModify.AcceptButtonCaption = null;
            this.SelectObjectsToModify.CancelButtonCaption = null;
            this.SelectObjectsToModify.Caption = "Select Objects to Modify";
            this.SelectObjectsToModify.ConfirmationMessage = null;
            this.SelectObjectsToModify.Id = "SelectObjectsToModify";
            this.SelectObjectsToModify.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelve);
            this.SelectObjectsToModify.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.SelectObjectsToModify.ToolTip = null;
            this.SelectObjectsToModify.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.SelectObjectsToModify.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectObjectsToModify_CustomizePopupWindowParams);
            this.SelectObjectsToModify.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectObjectsToModify_Execute);
            // 
            // SelectObjectsToDelete
            // 
            this.SelectObjectsToDelete.AcceptButtonCaption = null;
            this.SelectObjectsToDelete.CancelButtonCaption = null;
            this.SelectObjectsToDelete.Caption = "Select Objects to Delete";
            this.SelectObjectsToDelete.ConfirmationMessage = null;
            this.SelectObjectsToDelete.Id = "AriaObjectShelveSelectObjectsToDelete";
            this.SelectObjectsToDelete.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelve);
            this.SelectObjectsToDelete.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.SelectObjectsToDelete.ToolTip = null;
            this.SelectObjectsToDelete.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.SelectObjectsToDelete.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectObjectsToDelete_CustomizePopupWindowParams);
            this.SelectObjectsToDelete.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectObjectsToDelete_Execute);
            // 
            // AriaObjectShelveViewController
            // 
            this.Actions.Add(this.SelectObjectsToModify);
            this.Actions.Add(this.SelectObjectsToDelete);
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelve);
            this.TypeOfView = typeof(DevExpress.ExpressApp.View);
            this.ViewControlsCreated += new System.EventHandler(this.AriaObjectShelveViewController_ViewControlsCreated);
            this.Activated += new System.EventHandler(this.AriaObjectShelveController_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectObjectsToModify;
        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectObjectsToDelete;
    }
}
