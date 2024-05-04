namespace Aria5SystemAdmin.Module.Web
{
    partial class Aria5SystemAdminAspNetModule
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
            // Aria5SystemAdminAspNetModule
            // 
            this.RequiredModuleTypes.Add(typeof(Aria5SystemAdmin.Module.Aria5SystemAdminModule));
            this.RequiredModuleTypes.Add(typeof(DevExpress.ExpressApp.Web.SystemModule.SystemAspNetModule));
            this.RequiredModuleTypes.Add(typeof(DevExpress.ExpressApp.TreeListEditors.Web.TreeListEditorsAspNetModule));
            this.RequiredModuleTypes.Add(typeof(AriaDevExpress.Module.AriaDevExpressModule));

        }

        #endregion
    }
}
