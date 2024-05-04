namespace AriaDevExpress.Module.Web.Controllers.WebSite
{
    partial class PingSearchEngine_Button_ToolBar
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
            this.PingSerchEngine2 = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // PingSerchEngine2
            // 
            this.PingSerchEngine2.Caption = "Ping Search Engine";
            this.PingSerchEngine2.ConfirmationMessage = null;
            this.PingSerchEngine2.Id = "PingSerchEngine2";
            this.PingSerchEngine2.ImageName = null;
            this.PingSerchEngine2.Shortcut = null;
            this.PingSerchEngine2.Tag = null;
            this.PingSerchEngine2.TargetObjectsCriteria = null;
            this.PingSerchEngine2.TargetViewId = null;
            this.PingSerchEngine2.ToolTip = null;
            this.PingSerchEngine2.TypeOfView = null;
            this.PingSerchEngine2.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.PingSerchEngine2_Execute);
            // 
            // PingSearchEngine_Button_ToolBar
            // 
            this.Activated += new System.EventHandler(this.PingSearchEngine_Button_ToolBar_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction PingSerchEngine2;
    }
}
