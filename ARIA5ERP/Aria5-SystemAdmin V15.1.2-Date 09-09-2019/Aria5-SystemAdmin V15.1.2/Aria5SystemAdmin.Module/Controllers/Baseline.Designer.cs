namespace Aria5SystemAdmin.Module.Controllers {
	partial class Baseline {
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		/// <summary> 
		/// Clean up any resources being used.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing) {
			if(disposing && (components != null)) {
				components.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Component Designer generated code

		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent() {
            this.components = new System.ComponentModel.Container();
            this.ImportRequirement = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // ImportRequirement
            // 
            this.ImportRequirement.Caption = "Import Requirement";
            this.ImportRequirement.ConfirmationMessage = null;
            this.ImportRequirement.Id = "ImportRequirement";
            this.ImportRequirement.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.Requirement);
            this.ImportRequirement.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.ImportRequirement.ToolTip = null;
            this.ImportRequirement.TypeOfView = typeof(DevExpress.ExpressApp.ListView);
            this.ImportRequirement.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.ImportRequirement_Execute);
            // 
            // Baseline
            // 
            this.Actions.Add(this.ImportRequirement);
            this.ViewControlsCreated += new System.EventHandler(this.Baseline_ViewControlsCreated);
            this.AfterConstruction += new System.EventHandler(this.Baseline_AfterConstruction);
            this.Activated += new System.EventHandler(this.Baseline_Activated);

		}

		#endregion

        private DevExpress.ExpressApp.Actions.SimpleAction ImportRequirement;

    }
}
