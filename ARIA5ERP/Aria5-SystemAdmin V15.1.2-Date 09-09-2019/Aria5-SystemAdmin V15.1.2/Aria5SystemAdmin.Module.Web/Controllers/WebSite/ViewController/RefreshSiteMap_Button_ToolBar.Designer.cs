namespace AriaDevExpress.Module.Web.Controllers.WebSite
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
            this.RefreshSiteMap = new DevExpress.ExpressApp.Actions.SimpleAction(this.components);
            // 
            // RefreshSiteMap
            // 
            this.RefreshSiteMap.Caption = "Refresh Site Map";
            this.RefreshSiteMap.ConfirmationMessage = null;
            this.RefreshSiteMap.Id = "RefreshSiteMap";
            this.RefreshSiteMap.ImageName = null;
            this.RefreshSiteMap.Shortcut = null;
            this.RefreshSiteMap.Tag = null;
            this.RefreshSiteMap.TargetObjectsCriteria = null;
            this.RefreshSiteMap.TargetViewId = null;
            this.RefreshSiteMap.ToolTip = null;
            this.RefreshSiteMap.TypeOfView = null;
            this.RefreshSiteMap.Execute += new DevExpress.ExpressApp.Actions.SimpleActionExecuteEventHandler(this.RefreshSiteMap_Execute);
            // 
            // RefreshSiteMap_Button_ToolBar
            // 
            this.Activated += new System.EventHandler(this.RefreshSiteMap_Button_ToolBar_Activated);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.SimpleAction RefreshSiteMap;
    }
}
