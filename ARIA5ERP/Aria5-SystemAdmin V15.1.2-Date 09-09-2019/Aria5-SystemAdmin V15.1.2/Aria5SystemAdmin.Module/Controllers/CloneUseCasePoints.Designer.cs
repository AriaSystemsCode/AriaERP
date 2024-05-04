namespace Aria5SystemAdmin.Module.Controllers
{
    partial class CloneUseCasePoints
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
            this.CloneThisTemplate = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // CloneThisTemplate
            // 
            this.CloneThisTemplate.Caption = "Clone this Template";
            this.CloneThisTemplate.ConfirmationMessage = null;
            this.CloneThisTemplate.Id = "Clone this Template";
            this.CloneThisTemplate.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.QAUseCasePoints);
            this.CloneThisTemplate.ToolTip = null;
            this.CloneThisTemplate.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.CloneThisTemplate_Execute);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction CloneThisTemplate;

    }
}
