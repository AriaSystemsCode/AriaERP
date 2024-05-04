namespace Aria5SystemAdmin.Module.Controllers
{
    partial class ApplicationBuildViewController
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
            this.GenerateController = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.ApproveController = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.CompleteController = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // GenerateController
            // 
            this.GenerateController.Caption = "Generate Build";
            this.GenerateController.ConfirmationMessage = null;
            this.GenerateController.Id = "GenerateBuildAction";
            this.GenerateController.ToolTip = null;
            this.GenerateController.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.GenerateController_Execute_1);
            // 
            // ApproveController
            // 
            this.ApproveController.Caption = "Approve Build";
            this.ApproveController.ConfirmationMessage = null;
            this.ApproveController.Id = "ApproveAction";
            this.ApproveController.ToolTip = null;
            this.ApproveController.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.ApproveController_Execute);
            // 
            // CompleteController
            // 
            this.CompleteController.Caption = "Complete Build";
            this.CompleteController.ConfirmationMessage = null;
            this.CompleteController.Id = "CompleteBuildAction";
            this.CompleteController.ToolTip = null;
            this.CompleteController.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.CompleteController_Execute);
            // 
            // ApplicationBuildViewController
            // 
            this.Actions.Add(this.GenerateController);
            this.Actions.Add(this.ApproveController);
            this.Actions.Add(this.CompleteController);
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.ApplicationBuild_T);
            this.ViewControlsCreated += new System.EventHandler(this.ApplicationBuildViewController_ViewControlsCreated);
            this.Activated += new System.EventHandler(this.ApplicationBuildViewController_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction GenerateController;
        private DevExpress.ExpressApp.Actions.SimpleAction ApproveController;
        private DevExpress.ExpressApp.Actions.SimpleAction CompleteController;
    }
}
