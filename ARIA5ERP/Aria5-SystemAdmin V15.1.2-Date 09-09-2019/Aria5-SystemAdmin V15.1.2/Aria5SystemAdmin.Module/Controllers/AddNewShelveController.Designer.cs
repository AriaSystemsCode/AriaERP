namespace Aria5SystemAdmin.Module.Controllers
{
    partial class AddNewShelveController
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
            this.NewWindow = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // NewWindow
            // 
            this.NewWindow.AcceptButtonCaption = null;
            this.NewWindow.CancelButtonCaption = null;
            this.NewWindow.Caption = "New Shelve";
            this.NewWindow.ConfirmationMessage = null;
            this.NewWindow.Id = "New Shelve";
            this.NewWindow.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelve);
            this.NewWindow.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.NewWindow.ToolTip = null;
            this.NewWindow.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.NewWindow.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.NewWindow_CustomizePopupWindowParams);
            this.NewWindow.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.NewWindow_Execute);
            // 
            // AddNewShelve
            // 
            this.TypeOfView = typeof(DevExpress.ExpressApp.View);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction NewWindow;

    }
}
