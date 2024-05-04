namespace Aria5SystemAdmin.Module.Controllers
{
    partial class EntityOperationPermissionGeneratorController
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
            this.GeneratePermission = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.UpdateEntities = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // GeneratePermission
            // 
            this.GeneratePermission.Caption = "Generate Permission";
            this.GeneratePermission.ConfirmationMessage = null;
            this.GeneratePermission.Id = "EntityOperationPermissionGenerator_GeneratePermission";
            this.GeneratePermission.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.EntityOperationPermissionGenerator);
            this.GeneratePermission.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.GeneratePermission.ToolTip = null;
            this.GeneratePermission.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.GeneratePermission.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.GeneratePermission_Execute);
            // 
            // UpdateEntities
            // 
            this.UpdateEntities.Caption = "Update Entities";
            this.UpdateEntities.ConfirmationMessage = null;
            this.UpdateEntities.Id = "EntityOperationPermissionGenerator_UpdateEntities";
            this.UpdateEntities.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.EntityOperationPermissionGenerator);
            this.UpdateEntities.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.UpdateEntities.ToolTip = null;
            this.UpdateEntities.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.UpdateEntities.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.UpdateEntities_Execute);
            // 
            // EntityOperationPermissionGeneratorController
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.EntityOperationPermissionGenerator);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction GeneratePermission;
        private DevExpress.ExpressApp.Actions.SimpleAction UpdateEntities;
    }
}
