namespace Aria5SystemAdmin.Module.Controllers
{
    partial class DefectDensityController
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
            this.CalculateDD = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            this.getdefetfiles = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // CalculateDD
            // 
            this.CalculateDD.Caption = "Calculate DD";
            this.CalculateDD.ConfirmationMessage = null;
            this.CalculateDD.Id = "843037ef-192b-484e-a551-e5d3e5824e46";
            this.CalculateDD.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.DefectDensityReport);
            this.CalculateDD.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.CalculateDD.ToolTip = null;
            this.CalculateDD.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.CalculateDD.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.uploadfile_Execute);
            // 
            // getdefetfiles
            // 
            this.getdefetfiles.Caption = "GetFiles";
            this.getdefetfiles.ConfirmationMessage = null;
            this.getdefetfiles.Id = "getdefetfiles";
            this.getdefetfiles.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.DefectDensityFiles);
            this.getdefetfiles.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.getdefetfiles.ToolTip = null;
            this.getdefetfiles.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.getdefetfiles.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.getdefetfiles_Execute);
            // 
            // DefectDensityController
            // 
            this.Actions.Add(this.CalculateDD);
            this.Actions.Add(this.getdefetfiles);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction CalculateDD;
        private DevExpress.ExpressApp.Actions.SimpleAction getdefetfiles;
    }
}
