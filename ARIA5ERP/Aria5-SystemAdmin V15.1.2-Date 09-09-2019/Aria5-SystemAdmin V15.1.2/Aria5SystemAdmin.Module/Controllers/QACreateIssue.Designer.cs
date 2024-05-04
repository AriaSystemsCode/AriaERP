namespace Aria5SystemAdmin.Module.Controllers
{
    partial class QACreateIssue
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
            this.AddIssue = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // AddIssue
            // 
            this.AddIssue.Caption = "Add Issue";
            this.AddIssue.Category = "View";
            this.AddIssue.ConfirmationMessage = null;
            this.AddIssue.Id = "AddIssue";
            this.AddIssue.ToolTip = null;
            this.AddIssue.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.AddIssueAction_Execute);
            // 
            // CreateIssue
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.QARisk);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction AddIssue;
    }
}
