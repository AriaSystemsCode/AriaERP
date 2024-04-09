namespace Aria.Services.RequestHandler
{
    partial class AriaRequestHandler
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
            this.RequestAgentTimer = new System.Timers.Timer();
            ((System.ComponentModel.ISupportInitialize)(this.RequestAgentTimer)).BeginInit();
            // 
            // RequestAgentTimer
            // 
            this.RequestAgentTimer.Enabled = true;            
            //SAB 12-25-2013 Fix RB performance issue [Start]
            //this.RequestAgentTimer.Interval = 60000;
            this.RequestAgentTimer.Interval = 10000;
            //SAB 12-25-2013 Fix RB performance issue [Start]
            this.RequestAgentTimer.Elapsed += new System.Timers.ElapsedEventHandler(this._requestAgentTimer_Elapsed);
            // 
            // AriaRequestAgent
            // 
            this.ServiceName = _serviceName;
            ((System.ComponentModel.ISupportInitialize)(this.RequestAgentTimer)).EndInit();
        }

        #endregion

        private System.Timers.Timer RequestAgentTimer;
    }
}
