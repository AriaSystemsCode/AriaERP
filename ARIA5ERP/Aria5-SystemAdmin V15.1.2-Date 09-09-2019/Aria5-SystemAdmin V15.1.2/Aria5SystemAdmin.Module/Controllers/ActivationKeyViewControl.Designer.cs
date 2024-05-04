namespace Aria5SystemAdmin.Module.Controllers
{
    partial class ActivationKeyViewControl
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
            this.GenerateActivationKeyViewControl = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // GenerateActivationKeyViewControl
            this.GenerateActivationKeyViewControl.Caption = "Generate Activation Key ";
            this.GenerateActivationKeyViewControl.ConfirmationMessage = "Are you Sure you  want to Generate New  Activation Key?";
            this.GenerateActivationKeyViewControl.Id = "GenerateActivationKeyViewController";
            this.GenerateActivationKeyViewControl.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem);
            this.GenerateActivationKeyViewControl.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.GenerateActivationKeyViewControl.ToolTip = null;
            this.GenerateActivationKeyViewControl.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.GenerateActivationKeyViewControl.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.GenerateActivationKeyViewControl_Execute);
            // 
            // ActivationKeyViewControl
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction GenerateActivationKeyViewControl;
    }
}
