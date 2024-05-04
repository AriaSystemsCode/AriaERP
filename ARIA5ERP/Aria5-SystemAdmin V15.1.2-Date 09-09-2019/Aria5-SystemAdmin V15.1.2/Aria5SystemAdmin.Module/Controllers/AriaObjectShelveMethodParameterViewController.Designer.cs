namespace Aria5SystemAdmin.Module.Controllers
{
    partial class AriaObjectShelveMethodParameterViewController
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
            this.SelectMethodParametersToModify = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            this.SelectMethodParametersToDelete = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // SelectMethodParametersToModify
            // 
            this.SelectMethodParametersToModify.AcceptButtonCaption = null;
            this.SelectMethodParametersToModify.CancelButtonCaption = null;
            this.SelectMethodParametersToModify.Caption = "Select Method Parameters To Modify";
            this.SelectMethodParametersToModify.ConfirmationMessage = null;
            this.SelectMethodParametersToModify.Id = "AriaObjectShelveMethodParameterSelectMethodParametersToModify";
            this.SelectMethodParametersToModify.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethodParameter);
            this.SelectMethodParametersToModify.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.SelectMethodParametersToModify.ToolTip = null;
            this.SelectMethodParametersToModify.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.SelectMethodParametersToModify.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectEventsToModify_CustomizePopupWindowParams);
            this.SelectMethodParametersToModify.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectEventsToModify_Execute);
            // 
            // SelectMethodParametersToDelete
            // 
            this.SelectMethodParametersToDelete.AcceptButtonCaption = null;
            this.SelectMethodParametersToDelete.CancelButtonCaption = null;
            this.SelectMethodParametersToDelete.Caption = "Select Method Parameters To Delete";
            this.SelectMethodParametersToDelete.ConfirmationMessage = null;
            this.SelectMethodParametersToDelete.Id = "AriaObjectShelveMethodParameterSelectMethodParametersToDelete";
            this.SelectMethodParametersToDelete.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethodParameter);
            this.SelectMethodParametersToDelete.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.SelectMethodParametersToDelete.ToolTip = null;
            this.SelectMethodParametersToDelete.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.SelectMethodParametersToDelete.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectEventsToDelete_CustomizePopupWindowParams);
            this.SelectMethodParametersToDelete.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectEventsToDelete_Execute);
            // 
            // AriaObjectShelveMethodParameterViewController
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveMethodParameter);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.Activated += new System.EventHandler(this.AriaObjectShelveMethodParameterViewController_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectMethodParametersToModify;
        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectMethodParametersToDelete;
    }
}
