namespace Aria5SystemAdmin.Module.Controllers
{
    partial class TrackingTaskViewController
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
            this.GetRescources = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // GetRescources
            // 
            this.GetRescources.Caption = "Get Resources";
            this.GetRescources.ConfirmationMessage = null;
            this.GetRescources.Id = "TrackingTaskGetResources";
            this.GetRescources.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.GetRescources.ToolTip = null;
            this.GetRescources.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.GetRescources.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.GetRescources_Execute);
            // 
            // TrackingTaskViewController
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingTask);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction GetRescources;
    }
}
