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
        /// 
        
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
            //SAB 12-25-2013 Fix RB performance issue [End]
            this.RequestAgentTimer.Elapsed += new System.Timers.ElapsedEventHandler(this._requestAgentTimer_Elapsed);
            // 
            // AriaRequestAgent
            // 


            // mah 5/20/2014
            this.RequestHangTimer = new System.Timers.Timer();
            // tmi [start] T20140415.0001 this timer should be disabled as there is a separate service that is responsible for the same task
            //this.RequestHangTimer.Enabled = true;
            this.RequestHangTimer.Enabled = false;
            // tmi [end  ] T20140415.0001 
            this.RequestHangTimer.Interval = 60000;
            this.RequestHangTimer.Elapsed += new System.Timers.ElapsedEventHandler(this._requestHangTimer_Elapsed);
            // mah 5/20/2014

            this.ServiceName = _serviceName;
            ((System.ComponentModel.ISupportInitialize)(this.RequestAgentTimer)).EndInit();
        }

        #endregion

        private System.Timers.Timer RequestAgentTimer;
        private System.Timers.Timer RequestHangTimer;
    }
}
