namespace Aria.Services.ShutdownHangedRequestService
{
    partial class ShutdownHangedRequestService
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
            this.Timer = new System.Timers.Timer();
            ((System.ComponentModel.ISupportInitialize)(this.Timer)).BeginInit();
            this.Timer.Enabled = true;
            this.Timer.Interval = 60000;
            Timer.Elapsed += new System.Timers.ElapsedEventHandler(Timer_Elapsed);
            // 
            // AriaRequestAgent
            // 

            ((System.ComponentModel.ISupportInitialize)(this.Timer)).EndInit();

#if(DEBUG)
#else            
            this.ServiceName = "Aria Shutdown Hanged Request Service";
#endif
        }

        private System.Timers.Timer Timer;

        #endregion

    }
}
