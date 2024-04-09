using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.ServiceProcess;
using System.Text;
using Aria.EnterpriseServices.RequestHandler;
using System.Timers;

namespace Aria.Services.RequestHandler
{
    public partial class AriaRequestAgentService : ServiceBase
    {
        public AriaRequestAgentService()
        {
            InitializeComponent();
        }

        protected override void OnStart(string[] args)
        {
            EventLog.WriteEntry("Service Started");

            AriaRequestAgent requestAgent = new AriaRequestAgent();
            
            try
            {
                requestAgent.GenerateOnComputerStartupRequests();
            }
            catch (Exception ex)
            {
                EventLog.WriteEntry(ex.Message);
            }

            try
            {
                //requestAgent.ExecuteOneTimeOnlyRequests();
            }
            catch (Exception ex)
            {
                EventLog.WriteEntry(ex.Message);
            }

            this._requestAgentTimer.Start();
        }

        protected override void OnStop()
        {
            this._requestAgentTimer.Stop();
            
            EventLog.WriteEntry("Service Stoped");
        }

        private void _requestAgentTimer_Elapsed(object sender, ElapsedEventArgs e)
        {
            EventLog.WriteEntry("Timer Elapsed");

            AriaRequestAgent requestAgent = new AriaRequestAgent();
            
            try
            {                
                requestAgent.GenerateScheduleRequests();                
            }
            catch (Exception ex)
            {
                EventLog.WriteEntry(ex.Message);
            }

            try
            {
                //requestAgent.ExecuteOneTimeOnlyRequests();
            }
            catch (Exception ex)
            {
                EventLog.WriteEntry(ex.Message);
            }
        }        
    }
}
