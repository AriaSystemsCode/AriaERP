namespace AriaDevExpress.Module.Web.Controllers.OneTouchAway
{
    partial class RefreshSiteMap_Button_ToolBar
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
            this.OTAWAYRefreshSiteMap = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // OTAWAYRefreshSiteMap
            // 
            this.OTAWAYRefreshSiteMap.Caption = "Refresh Site Map";
            this.OTAWAYRefreshSiteMap.ConfirmationMessage = null;
            this.OTAWAYRefreshSiteMap.Id = "OTAWAYRefreshSiteMap";
            this.OTAWAYRefreshSiteMap.ImageName = null;
            this.OTAWAYRefreshSiteMap.Shortcut = null;
            this.OTAWAYRefreshSiteMap.Tag = null;
            this.OTAWAYRefreshSiteMap.TargetObjectsCriteria = null;
            this.OTAWAYRefreshSiteMap.TargetViewId = null;
            this.OTAWAYRefreshSiteMap.ToolTip = null;
            this.OTAWAYRefreshSiteMap.TypeOfView = null;
            this.OTAWAYRefreshSiteMap.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.RefreshSiteMap_Execute);
            // 
            // RefreshSiteMap_Button_ToolBar
            // 
            this.Activated += new System.EventHandler(this.RefreshSiteMap_Button_ToolBar_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction OTAWAYRefreshSiteMap;
    }
}
