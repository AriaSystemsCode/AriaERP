namespace Aria5SystemAdmin.Module.Controllers
{
    partial class ApprovalsController
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
            this.ScopeApproval = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.ProofOfConceptApproval = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.BaselineItemsApprovale = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // ScopeApproval
            // 
            this.ScopeApproval.Caption = "Approve";
            this.ScopeApproval.ConfirmationMessage = null;
            this.ScopeApproval.Id = "ScopeApproval";
            this.ScopeApproval.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.ProjectScope);
            this.ScopeApproval.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.ScopeApproval.ToolTip = null;
            this.ScopeApproval.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.ScopeApproval.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.ScopeApproval_Execute);
            // 
            // ProofOfConceptApproval
            // 
            this.ProofOfConceptApproval.Caption = "Approve";
            this.ProofOfConceptApproval.ConfirmationMessage = null;
            this.ProofOfConceptApproval.Id = "ProofOfConceptApproval";
            this.ProofOfConceptApproval.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.ProofOfConcept);
            this.ProofOfConceptApproval.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.ProofOfConceptApproval.ToolTip = null;
            this.ProofOfConceptApproval.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.ProofOfConceptApproval.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.ProofOfConceptApproval_Execute);
            // 
            // BaselineItemsApprovale
            // 
            this.BaselineItemsApprovale.Caption = "Approve";
            this.BaselineItemsApprovale.ConfirmationMessage = null;
            this.BaselineItemsApprovale.Id = "BaselineItemsApprovale";
            this.BaselineItemsApprovale.ToolTip = null;
            this.BaselineItemsApprovale.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.BaselineItemsApprovale_Execute);
            // 
            // ApprovalsController
            // 
            this.Actions.Add(this.ScopeApproval);
            this.Actions.Add(this.ProofOfConceptApproval);
            this.Actions.Add(this.BaselineItemsApprovale);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction ScopeApproval;
        private DevExpress.ExpressApp.Actions.SimpleAction ProofOfConceptApproval;
        private DevExpress.ExpressApp.Actions.SimpleAction BaselineItemsApprovale;
    }
}
