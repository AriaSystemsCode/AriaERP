namespace Aria5SystemAdmin.Module
{
    partial class Aria5SystemAdminModule
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
            // 
            // Aria5SystemAdminModule
            // 
            this.AdditionalExportedTypes.Add(typeof(Aria5.DevExpress.MainSystem.Module.BusinessObjects.IdentifierSegment));
            this.AdditionalExportedTypes.Add(typeof(Aria5.DevExpress.MainSystem.Module.BusinessObjects.IdentifierSegmentFormat));
            this.AdditionalExportedTypes.Add(typeof(Aria5.DevExpress.MainSystem.Module.BusinessObjects.IdentifierSegmentValidEntry));
            this.AdditionalExportedTypes.Add(typeof(Aria5.DevExpress.MainSystem.Module.BusinessObjects.IdentifierStructure));
            this.RequiredModuleTypes.Add(typeof(DevExpress.ExpressApp.SystemModule.SystemModule));
            this.RequiredModuleTypes.Add(typeof(DevExpress.ExpressApp.ConditionalAppearance.ConditionalAppearanceModule));
            this.RequiredModuleTypes.Add(typeof(DevExpress.ExpressApp.Validation.ValidationModule));
            this.RequiredModuleTypes.Add(typeof(DevExpress.ExpressApp.ViewVariantsModule.ViewVariantsModule));
            this.RequiredModuleTypes.Add(typeof(DevExpress.ExpressApp.Security.SecurityModule));
            this.RequiredModuleTypes.Add(typeof(DevExpress.ExpressApp.Notifications.NotificationsModule));

        }

        #endregion
    }
}
