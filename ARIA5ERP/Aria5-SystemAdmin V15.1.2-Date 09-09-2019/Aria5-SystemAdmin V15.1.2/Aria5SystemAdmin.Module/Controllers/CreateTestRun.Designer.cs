namespace Aria5SystemAdmin.Module.Controllers
{
    partial class CreateTestRun
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
            this.CreateTestRuna = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            this.CreateTestRunbasedontestcases = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.CreateTestcasebasedonsteps = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.TestCaseStepsPassed = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // CreateTestRuna
            // 
            this.CreateTestRuna.AcceptButtonCaption = null;
            this.CreateTestRuna.CancelButtonCaption = null;
            this.CreateTestRuna.Caption = "Create Integration Test Run";
            this.CreateTestRuna.Category = "Edit";
            this.CreateTestRuna.ConfirmationMessage = null;
            this.CreateTestRuna.Id = "CreateTestRun";
            this.CreateTestRuna.QuickAccess = true;
            this.CreateTestRuna.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.ProjectTemplate);
            this.CreateTestRuna.TargetViewNesting = DevExpress.ExpressApp.Nesting.Root;
            this.CreateTestRuna.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.CreateTestRuna.ToolTip = "Create Automatic Test Run";
            this.CreateTestRuna.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.CreateTestRuna.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.AssignedResource_CustomizePopupWindowParams);
            this.CreateTestRuna.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.AssignedResource_Execute);
            // 
            // CreateTestRunbasedontestcases
            // 
            this.CreateTestRunbasedontestcases.Caption = "Create Test Run";
            this.CreateTestRunbasedontestcases.ConfirmationMessage = null;
            this.CreateTestRunbasedontestcases.Id = "CreateTestRunbasedontestcases";
            this.CreateTestRunbasedontestcases.SelectionDependencyType = DevExpress.ExpressApp.Actions.SelectionDependencyType.RequireMultipleObjects;
            this.CreateTestRunbasedontestcases.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TestCase);
            this.CreateTestRunbasedontestcases.TargetViewNesting = DevExpress.ExpressApp.Nesting.Nested;
            this.CreateTestRunbasedontestcases.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.CreateTestRunbasedontestcases.ToolTip = null;
            this.CreateTestRunbasedontestcases.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.CreateTestRunbasedontestcases.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.CreateTestRunbasedontestcases_Execute);
            // 
            // CreateTestcasebasedonsteps
            // 
            this.CreateTestcasebasedonsteps.Caption = "Create Test Case";
            this.CreateTestcasebasedonsteps.ConfirmationMessage = null;
            this.CreateTestcasebasedonsteps.Id = "CreateTestcasebasedonsteps";
            this.CreateTestcasebasedonsteps.SelectionDependencyType = DevExpress.ExpressApp.Actions.SelectionDependencyType.RequireMultipleObjects;
            this.CreateTestcasebasedonsteps.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TestCaseSteps);
            this.CreateTestcasebasedonsteps.TargetViewNesting = DevExpress.ExpressApp.Nesting.Nested;
            this.CreateTestcasebasedonsteps.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.CreateTestcasebasedonsteps.ToolTip = null;
            this.CreateTestcasebasedonsteps.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.CreateTestcasebasedonsteps.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.CreateTestcasebasedonsteps_Execute);
            // 
            // TestCaseStepsPassed
            // 
            this.TestCaseStepsPassed.Caption = "Mark As Passed";
            this.TestCaseStepsPassed.ConfirmationMessage = null;
            this.TestCaseStepsPassed.Id = "TestCaseStepsPassed";
            this.TestCaseStepsPassed.SelectionDependencyType = DevExpress.ExpressApp.Actions.SelectionDependencyType.RequireMultipleObjects;
            this.TestCaseStepsPassed.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TestCaseSteps);
            this.TestCaseStepsPassed.TargetViewNesting = DevExpress.ExpressApp.Nesting.Nested;
            this.TestCaseStepsPassed.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.TestCaseStepsPassed.ToolTip = null;
            this.TestCaseStepsPassed.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.TestCaseStepsPassed.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.TestCaseStepsPassed_Execute);
            // 
            // CreateTestRun
            // 
            this.Actions.Add(this.CreateTestRuna);
            this.Actions.Add(this.CreateTestRunbasedontestcases);
            this.Actions.Add(this.CreateTestcasebasedonsteps);
            this.Actions.Add(this.TestCaseStepsPassed);
            this.TypeOfView = typeof(DevExpress.ExpressApp.View);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction CreateTestRuna;
        private DevExpress.ExpressApp.Actions.SimpleAction CreateTestRunbasedontestcases;
        private DevExpress.ExpressApp.Actions.SimpleAction CreateTestcasebasedonsteps;
        private DevExpress.ExpressApp.Actions.SimpleAction TestCaseStepsPassed;
    }
}
