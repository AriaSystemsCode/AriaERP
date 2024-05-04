namespace Aria5SystemAdmin.Module.Controllers
{
    partial class CustomClone
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
            this.CopyTestRun = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // CopyTestRun
            // 
            this.CopyTestRun.Caption = "Copy Test Run";
            this.CopyTestRun.ConfirmationMessage = null;
            this.CopyTestRun.Id = "CopyTestRun";
            this.CopyTestRun.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TestRun);
            this.CopyTestRun.ToolTip = null;
            this.CopyTestRun.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.CopyTestRun_Execute);
            // 
            // CustomClone
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.TestRun);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction CopyTestRun;

    }
}
