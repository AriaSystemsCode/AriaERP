namespace Aria5SystemAdmin.Module.Controllers
{
    partial class EntityOperatiPermissionWindowController
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
            this.NewPermission1 = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // NewPermission1
            // 
            this.NewPermission1.Caption = "New Permission";
            this.NewPermission1.ConfirmationMessage = null;
            this.NewPermission1.Id = "EntityOperationPermissionWindow_NewPermission";
            this.NewPermission1.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaSecuritySystemRole);
            this.NewPermission1.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.NewPermission1.ToolTip = null;
            this.NewPermission1.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.NewPermission1.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.NewPermission_Execute);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction NewPermission1;
    }
}
