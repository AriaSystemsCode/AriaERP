namespace Aria5SystemAdmin.Module.Controllers
{
    partial class TrackingEntryController
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
            this.TrackingEntry = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // TrackingEntry
            // 
            this.TrackingEntry.Caption = "Complete";
            this.TrackingEntry.ConfirmationMessage = null;
            this.TrackingEntry.Id = "TrackingEntryController";
            this.TrackingEntry.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry);
            this.TrackingEntry.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.TrackingEntry.ToolTip = null;
            this.TrackingEntry.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.TrackingEntry.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.TrackingEntry_Execute);
            // 
            // TrackingEntryController
            // 
            this.Activated += new System.EventHandler(this.TrackingEntryController_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction TrackingEntry;
    }
}
