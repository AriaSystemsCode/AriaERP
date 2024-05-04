namespace Aria5SystemAdmin.Module.Controllers
{
    partial class GenerateFoxBuild
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
            this.Generatebuild = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // Generatebuild
            // 
            this.Generatebuild.Caption = "Generate Build";
            this.Generatebuild.ConfirmationMessage = "Reviewed all Tracking Entries Attached on the Build\r\n";
            this.Generatebuild.Id = "a5c5624f-8c22-480b-b152-4dfccee7614a";
            this.Generatebuild.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.ApplicationBuild_T);
            this.Generatebuild.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.Generatebuild.ToolTip = null;
            this.Generatebuild.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.Generatebuild.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.Generatebuild_Execute);
            // 
            // GenerateFoxBuild
            // 
            this.Actions.Add(this.Generatebuild);
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.ApplicationBuild_T);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction Generatebuild;
    }
}
