namespace Aria.Services.RequestHandler
{
    partial class AriaRequestHandlerInstaller
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
            this.RequestHandlerInstaller = new System.ServiceProcess.ServiceInstaller();
            this.RequestHandlerProcessInstaller = new System.ServiceProcess.ServiceProcessInstaller();
            // 
            // RequestHandlerInstaller
            // 
            this.RequestHandlerInstaller.Description = _description;
            this.RequestHandlerInstaller.DisplayName = _displayName;
            this.RequestHandlerInstaller.ServiceName = _serviceName;
            this.RequestHandlerInstaller.StartType = _startType;            
            // 
            // RequestHandlerProcessInstaller
            // 
            this.RequestHandlerProcessInstaller.Account = _account;
            this.RequestHandlerProcessInstaller.Password = _password;
            this.RequestHandlerProcessInstaller.Username = _userName;
            // 
            // AriaRequestHandlerInstaller
            // 
            this.Installers.AddRange(new System.Configuration.Install.Installer[] {
            this.RequestHandlerInstaller,
            this.RequestHandlerProcessInstaller});

        }

        #endregion

        public System.ServiceProcess.ServiceInstaller RequestHandlerInstaller;
        public System.ServiceProcess.ServiceProcessInstaller RequestHandlerProcessInstaller;

    }
}