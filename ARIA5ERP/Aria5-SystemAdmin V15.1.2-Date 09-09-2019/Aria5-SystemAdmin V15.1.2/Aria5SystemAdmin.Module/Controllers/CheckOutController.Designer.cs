namespace Aria5SystemAdmin.Module.Controllers
{
    partial class CheckOutController
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
            this.CheckedOutFiles = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // CheckedOutFiles
            // 
            this.CheckedOutFiles.Caption = "Checked Out Files";
            this.CheckedOutFiles.ConfirmationMessage = null;
            this.CheckedOutFiles.Id = "Checked Out Files";
            this.CheckedOutFiles.ToolTip = null;
            this.CheckedOutFiles.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.CheckedOutFiles_Execute);
            // 
            // CheckOut
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObjectShelve);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.ListView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.ListView);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction CheckedOutFiles;
    }
}
