namespace Aria5SystemAdmin.Module.Controllers
{
    partial class AriaObjectShelveEventViewController
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
            this.SelectEventsToModify = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            this.SelectEventsToDelete = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // SelectEventsToModify
            // 
            this.SelectEventsToModify.AcceptButtonCaption = null;
            this.SelectEventsToModify.CancelButtonCaption = null;
            this.SelectEventsToModify.Caption = "Select Events To Modify";
            this.SelectEventsToModify.ConfirmationMessage = null;
            this.SelectEventsToModify.Id = "AriaObjectShelveEventSelectEventsToModify";
            this.SelectEventsToModify.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEvent);
            this.SelectEventsToModify.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.SelectEventsToModify.ToolTip = null;
            this.SelectEventsToModify.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.SelectEventsToModify.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectEventsToModify_CustomizePopupWindowParams);
            this.SelectEventsToModify.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectEventsToModify_Execute);
            // 
            // SelectEventsToDelete
            // 
            this.SelectEventsToDelete.AcceptButtonCaption = null;
            this.SelectEventsToDelete.CancelButtonCaption = null;
            this.SelectEventsToDelete.Caption = "Select Events To Delete";
            this.SelectEventsToDelete.ConfirmationMessage = null;
            this.SelectEventsToDelete.Id = "AriaObjectShelveEventSelectEventsToDelete";
            this.SelectEventsToDelete.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEvent);
            this.SelectEventsToDelete.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.SelectEventsToDelete.ToolTip = null;
            this.SelectEventsToDelete.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.SelectEventsToDelete.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectEventsToDelete_CustomizePopupWindowParams);
            this.SelectEventsToDelete.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectEventsToDelete_Execute);
            // 
            // AriaObjectShelveEventViewController
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEvent);
            this.Activated += new System.EventHandler(this.AriaObjectShelveEventViewController_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectEventsToModify;
        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectEventsToDelete;
    }
}
