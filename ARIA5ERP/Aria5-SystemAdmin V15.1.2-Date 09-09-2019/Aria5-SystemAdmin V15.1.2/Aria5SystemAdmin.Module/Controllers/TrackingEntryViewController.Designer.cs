namespace Aria5SystemAdmin.Module.Controllers
{
    partial class TrackingEntryViewController
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
            this.Complete = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.Cancel = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.CreateTestRun = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.CreateFix = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.StartThisTracking = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.ScheduleThisTracking = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            //MMT
            //Approve
            this.ApproveTracking = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.ApproveTracking.Caption = "Approve This Tracking";
            this.ApproveTracking.ConfirmationMessage = null;
            this.ApproveTracking.Id = "ApproveTrackingApproveTracking";
            this.ApproveTracking.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry);
            this.ApproveTracking.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.ApproveTracking.ToolTip = null;
            this.ApproveTracking.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.ApproveTracking.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.ApproveTracking_Execute);
            //MMT
            // 
            // Complete
            // 
            this.Complete.Caption = "Complete";
            this.Complete.ConfirmationMessage = null;
            this.Complete.Id = "CompleteController";
            this.Complete.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry);
            this.Complete.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.Complete.ToolTip = null;
            this.Complete.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.Complete.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.Complete_Execute);
            // 
            // Cancel
            // 
            this.Cancel.Caption = "Cancel";
            this.Cancel.ConfirmationMessage = null;
            this.Cancel.Id = "CancelAction";
            this.Cancel.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry);
            this.Cancel.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.Cancel.ToolTip = null;
            this.Cancel.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.Cancel.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.Cancel_Execute);
            // 
            // CreateTestRun
            // 
            this.CreateTestRun.Caption = "Create Test Run For Tracking";
            this.CreateTestRun.ConfirmationMessage = null;
            this.CreateTestRun.Id = "CreateTestRunForTracking";
            this.CreateTestRun.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry);
            this.CreateTestRun.TargetViewNesting = DevExpress.ExpressApp.Nesting.Root;
            this.CreateTestRun.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.CreateTestRun.ToolTip = null;
            this.CreateTestRun.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.CreateTestRun.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.CreateTestRunForTracking_Execute);
            // 
            // CreateFix
            // 
            this.CreateFix.Caption = "Create Fix";
            this.CreateFix.ConfirmationMessage = null;
            this.CreateFix.Id = "CreateFix";
            this.CreateFix.TargetObjectsCriteria = "Status != \'New\'";
            this.CreateFix.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry);
            this.CreateFix.TargetViewNesting = DevExpress.ExpressApp.Nesting.Root;
            this.CreateFix.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.CreateFix.ToolTip = null;
            this.CreateFix.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.CreateFix.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.CreateFix_Execute);
            // 
            // StartThisTracking
            // 
            this.StartThisTracking.Caption = "Start This Tracking";
            this.StartThisTracking.ConfirmationMessage = null;
            this.StartThisTracking.Id = "StartThisTracking";
            this.StartThisTracking.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry);
            this.StartThisTracking.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.StartThisTracking.ToolTip = null;
            this.StartThisTracking.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.StartThisTracking.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.StartThisTracking_Execute);
            // 
            // ScheduleThisTracking
            // 
            this.ScheduleThisTracking.Caption = "Schedule This Tracking";
            this.ScheduleThisTracking.ConfirmationMessage = null;
            this.ScheduleThisTracking.Id = "ScheduleThisTracking";
            this.ScheduleThisTracking.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry);
            this.ScheduleThisTracking.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.ScheduleThisTracking.ToolTip = null;
            this.ScheduleThisTracking.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.ScheduleThisTracking.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.ScheduleThisTracking_Execute);
            // 
            // TrackingEntryViewController
            // 
            this.Actions.Add(this.Complete);
            this.Actions.Add(this.Cancel);
            this.Actions.Add(this.CreateTestRun);
            this.Actions.Add(this.CreateFix);
            this.Actions.Add(this.StartThisTracking);
            this.Actions.Add(this.ScheduleThisTracking);
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TrackingEntry);
            this.ViewControlsCreated += new System.EventHandler(this.TrackingEntryViewController_ViewControlsCreated);
            this.Activated += new System.EventHandler(this.TrackingEntryViewController_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction Complete;
        private DevExpress.ExpressApp.Actions.SimpleAction Cancel;
        private DevExpress.ExpressApp.Actions.SimpleAction CreateTestRun;
        private DevExpress.ExpressApp.Actions.SimpleAction CreateFix;
        private DevExpress.ExpressApp.Actions.SimpleAction StartThisTracking;
        private DevExpress.ExpressApp.Actions.SimpleAction ScheduleThisTracking;
        //MMT
        private DevExpress.ExpressApp.Actions.SimpleAction ApproveTracking;
        //MMT
    }
}
