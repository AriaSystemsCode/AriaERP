namespace Aria5SystemAdmin.Module.Controllers
{
    partial class AriaObjectShelveEventParameterViewController
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
            this.SelectEventParametersToModify = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            this.SelectEventParametersToDelete = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // SelectEventParametersToModify
            // 
            this.SelectEventParametersToModify.AcceptButtonCaption = null;
            this.SelectEventParametersToModify.CancelButtonCaption = null;
            this.SelectEventParametersToModify.Caption = "Select Event Parameters To Modify";
            this.SelectEventParametersToModify.ConfirmationMessage = null;
            this.SelectEventParametersToModify.Id = "AriaObjectShelveEventParameterSelectEventParametersToModify";
            this.SelectEventParametersToModify.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.SelectEventParametersToModify.ToolTip = null;
            this.SelectEventParametersToModify.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.SelectEventParametersToModify.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectEventParametersToModify_CustomizePopupWindowParams);
            this.SelectEventParametersToModify.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectEventParametersToModify_Execute);
            // 
            // SelectEventParametersToDelete
            // 
            this.SelectEventParametersToDelete.AcceptButtonCaption = null;
            this.SelectEventParametersToDelete.CancelButtonCaption = null;
            this.SelectEventParametersToDelete.Caption = "Select Event Parameters To Delete";
            this.SelectEventParametersToDelete.ConfirmationMessage = null;
            this.SelectEventParametersToDelete.Id = "AriaObjectShelveEventParameterSelectEventParametersToDelete";
            this.SelectEventParametersToDelete.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.SelectEventParametersToDelete.ToolTip = null;
            this.SelectEventParametersToDelete.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.SelectEventParametersToDelete.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.SelectEventParametersToDelete_CustomizePopupWindowParams);
            this.SelectEventParametersToDelete.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.SelectEventParametersToDelete_Execute);
            // 
            // AriaObjectShelveEventParameterViewController
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelveEventParameter);
            this.Activated += new System.EventHandler(this.AriaObjectShelveEventParameterViewController_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectEventParametersToModify;
        private DevExpress.ExpressApp.Actions.PopupWindowShowAction SelectEventParametersToDelete;
    }
}
