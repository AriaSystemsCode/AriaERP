namespace Aria5SystemAdmin.Module.Controllers
{
    partial class EntityOperationPermissionController
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
            this.NewPermission = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // NewPermission
            // 
            this.NewPermission.Caption = "New Permission";
            this.NewPermission.ConfirmationMessage = null;
            this.NewPermission.Id = "EntityOperationPermission_NewPermission";
            this.NewPermission.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaSecuritySystemRole);
            this.NewPermission.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.NewPermission.ToolTip = null;
            this.NewPermission.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.NewPermission.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.NewPermission_Execute);
            // 
            // EntityOperationPermissionController
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaSecuritySystemRole);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction NewPermission;
    }
}
