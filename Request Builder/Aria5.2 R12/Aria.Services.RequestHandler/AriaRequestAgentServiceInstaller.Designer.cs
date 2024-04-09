namespace Aria.Services.RequestHandler
{
    partial class AriaRequestAgentServiceInstaller
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
            this._requestAgentServiceInstaller = new System.ServiceProcess.ServiceInstaller();
            this._requestAgentServiceProcessInstaller = new System.ServiceProcess.ServiceProcessInstaller();
            // 
            // _requestAgentServiceInstaller
            // 
            this._requestAgentServiceInstaller.ServiceName = "Aria Request Agent Service";
            this._requestAgentServiceInstaller.StartType = System.ServiceProcess.ServiceStartMode.Automatic;
            // 
            // _requestAgentServiceProcessInstaller
            // 
            this._requestAgentServiceProcessInstaller.Account = System.ServiceProcess.ServiceAccount.LocalSystem;
            this._requestAgentServiceProcessInstaller.Password = null;
            this._requestAgentServiceProcessInstaller.Username = null;
            // 
            // AriaRequestAgentServiceInstaller
            // 
            this.Installers.AddRange(new System.Configuration.Install.Installer[] {
            this._requestAgentServiceInstaller,
            this._requestAgentServiceProcessInstaller});

        }

        #endregion

        private System.ServiceProcess.ServiceInstaller _requestAgentServiceInstaller;
        private System.ServiceProcess.ServiceProcessInstaller _requestAgentServiceProcessInstaller;
    }
}