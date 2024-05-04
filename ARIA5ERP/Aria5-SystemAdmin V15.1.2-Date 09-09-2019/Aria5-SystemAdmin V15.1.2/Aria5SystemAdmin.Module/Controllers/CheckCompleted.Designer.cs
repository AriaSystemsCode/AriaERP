namespace Aria5SystemAdmin.Module.Controllers
{
    partial class CheckCompleted
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
            this.check = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // check
            // 
            this.check.Caption = "check";
            this.check.ConfirmationMessage = null;
            this.check.Id = "check";
            this.check.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate);
            this.check.ToolTip = null;
            this.check.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.check_Execute);
            // 
            // CheckCompleted
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction check;
    }
}
