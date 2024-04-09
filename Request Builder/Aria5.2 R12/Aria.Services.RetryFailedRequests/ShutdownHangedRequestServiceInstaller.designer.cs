namespace Aria.Services.ShutdownHangedRequestService
{
    partial class ShutdownHangedRequestServiceInstaller
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
            this.ShutdownHangedRequestHandlerInstaller = new System.ServiceProcess.ServiceInstaller();
            this.ShutdownHangedRequestHandlerProcessInstaller = new System.ServiceProcess.ServiceProcessInstaller();
            // 
            // RequestHandlerInstaller
            // 
            this.ShutdownHangedRequestHandlerInstaller.Description = "Aria Shutdown Hanged Request Service";
            this.ShutdownHangedRequestHandlerInstaller.DisplayName = "Aria Shutdown Hanged Request Service";
            this.ShutdownHangedRequestHandlerInstaller.ServiceName = "Aria Shutdown Hanged Request Service";
            this.ShutdownHangedRequestHandlerInstaller.StartType = System.ServiceProcess.ServiceStartMode.Automatic;
            // 
            // RequestHandlerProcessInstaller
            // 
            this.ShutdownHangedRequestHandlerProcessInstaller.Account = System.ServiceProcess.ServiceAccount.LocalSystem;
            // 
            // AriaRequestHandlerInstaller
            // 
            this.Installers.AddRange(new System.Configuration.Install.Installer[] {
            this.ShutdownHangedRequestHandlerInstaller,
            this.ShutdownHangedRequestHandlerProcessInstaller});
        }

        #endregion

        public System.ServiceProcess.ServiceInstaller ShutdownHangedRequestHandlerInstaller;
        public System.ServiceProcess.ServiceProcessInstaller ShutdownHangedRequestHandlerProcessInstaller;

    }
}