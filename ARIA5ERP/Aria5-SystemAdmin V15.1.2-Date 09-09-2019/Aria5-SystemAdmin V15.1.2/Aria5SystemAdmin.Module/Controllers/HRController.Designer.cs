namespace Aria5SystemAdmin.Module.Controllers
{
    partial class HRController
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
            DevExpress.ExpressApp.Actions.ChoiceActionItem choiceActionItem1 = new DevExpress.ExpressApp.Actions.ChoiceActionItem();
            DevExpress.ExpressApp.Actions.ChoiceActionItem choiceActionItem2 = new DevExpress.ExpressApp.Actions.ChoiceActionItem();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(HRController));
            this.ScheduleActivities = new DevExpress.ExpressApp.Actions.ParametrizedAction(this.components);
            this.AssignNewRevision = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.GetSettings = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.CompleteActivity = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.GetTheJobVacancyForm = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.changeapplicantstage = new DevExpress.ExpressApp.Actions.SingleChoiceAction(this.components);
            // 
            // ScheduleActivities
            // 
            this.ScheduleActivities.Caption = "Schedule Activities";
            this.ScheduleActivities.ConfirmationMessage = null;
            this.ScheduleActivities.Id = "ScheduleActivities";
            this.ScheduleActivities.NullValuePrompt = null;
            this.ScheduleActivities.ShortCaption = null;
            this.ScheduleActivities.TargetViewId = "HRJobVacancy_Applicants_ListView";
            this.ScheduleActivities.ToolTip = null;
            this.ScheduleActivities.TypeOfView = typeof(DevExpress.ExpressApp.View);
            this.ScheduleActivities.Execute += new DevExpress.ExpressApp.Actions.ParametrizedActionExecuteEventHandler(this.ScheduleActivities_Execute);
            // 
            // AssignNewRevision
            // 
            this.AssignNewRevision.Caption = "Assign New Revision";
            this.AssignNewRevision.ConfirmationMessage = null;
            this.AssignNewRevision.Id = "AssignNewJPRevision";
            this.AssignNewRevision.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.HRJobPosition);
            this.AssignNewRevision.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.AssignNewRevision.ToolTip = null;
            this.AssignNewRevision.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.AssignNewRevision.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.AssignNewRevision_Execute);
            // 
            // GetSettings
            // 
            this.GetSettings.Caption = "Get Settings";
            this.GetSettings.ConfirmationMessage = null;
            this.GetSettings.Id = "GetSettings";
            this.GetSettings.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.EntityTypeSettings);
            this.GetSettings.ToolTip = null;
            this.GetSettings.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.GetSettings_Execute);
            // 
            // CompleteActivity
            // 
            this.CompleteActivity.Caption = "Complete";
            this.CompleteActivity.ConfirmationMessage = null;
            this.CompleteActivity.Id = "CompleteActivity";
            this.CompleteActivity.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.HRActivity);
            this.CompleteActivity.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.CompleteActivity.ToolTip = null;
            this.CompleteActivity.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.CompleteActivity.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.CompleteActivity_Execute);
            // 
            // GetTheJobVacancyForm
            // 
            this.GetTheJobVacancyForm.Caption = "Get The Job Vacancy Form";
            this.GetTheJobVacancyForm.ConfirmationMessage = null;
            this.GetTheJobVacancyForm.Id = "GetTheJobVacancyForm";
            this.GetTheJobVacancyForm.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.HRJobVacancy);
            this.GetTheJobVacancyForm.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.GetTheJobVacancyForm.ToolTip = null;
            this.GetTheJobVacancyForm.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.GetTheJobVacancyForm.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.GetTheJobVacancyForm_Execute);
            // 
            // changeapplicantstage
            // 
            this.changeapplicantstage.Caption = "changeapplicantstage";
            this.changeapplicantstage.ConfirmationMessage = null;
            this.changeapplicantstage.Id = "changeapplicantstage";
            choiceActionItem1.Caption = "Passed";
            choiceActionItem1.Id = "Passed";
            choiceActionItem1.ImageName = null;
            choiceActionItem1.Shortcut = null;
            choiceActionItem1.ToolTip = null;
            choiceActionItem2.Caption = "Didn’t pass";
            choiceActionItem2.Id = "Didn’tpass";
            choiceActionItem2.ImageName = null;
            choiceActionItem2.Shortcut = null;
            choiceActionItem2.ToolTip = null;
            this.changeapplicantstage.Items.Add(choiceActionItem1);
            this.changeapplicantstage.Items.Add(choiceActionItem2);
            this.changeapplicantstage.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.HREmployee);
            this.changeapplicantstage.TargetViewId = resources.GetString("changeapplicantstage.TargetViewId");
            this.changeapplicantstage.ToolTip = null;
            this.changeapplicantstage.TypeOfView = typeof(DevExpress.ExpressApp.View);
            this.changeapplicantstage.Execute += new DevExpress.ExpressApp.Actions.SingleChoiceActionExecuteEventHandler(this.changeapplicantstage_Execute);
            // 
            // HRController
            // 
            this.Actions.Add(this.AssignNewRevision);
            this.Actions.Add(this.GetSettings);
            this.Actions.Add(this.CompleteActivity);
            this.Actions.Add(this.ScheduleActivities);
            this.Actions.Add(this.GetTheJobVacancyForm);
            this.Actions.Add(this.changeapplicantstage);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction AssignNewRevision;
        private DevExpress.ExpressApp.Actions.SimpleAction GetSettings;
        private DevExpress.ExpressApp.Actions.SimpleAction CompleteActivity;
        private DevExpress.ExpressApp.Actions.ParametrizedAction ScheduleActivities;
        private DevExpress.ExpressApp.Actions.SimpleAction GetTheJobVacancyForm;
        private DevExpress.ExpressApp.Actions.SingleChoiceAction changeapplicantstage;
    }
}
