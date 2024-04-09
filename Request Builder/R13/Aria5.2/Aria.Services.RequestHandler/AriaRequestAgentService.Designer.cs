namespace Aria.Services.RequestHandler
{
    partial class AriaRequestAgentService
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
            this._requestAgentTimer = new System.Timers.Timer();
            // 
            // _requestAgentTimer
            // 
            this._requestAgentTimer.Enabled = true;
            this._requestAgentTimer.Interval = 5000;
            this._requestAgentTimer.Elapsed += new System.Timers.ElapsedEventHandler(_requestAgentTimer_Elapsed);
            // 
            // AriaRequestAgentService
            // 
            this.ServiceName = "Aria Request Agent Service";

        }

        #endregion

        private System.Timers.Timer _requestAgentTimer;
    }
}
