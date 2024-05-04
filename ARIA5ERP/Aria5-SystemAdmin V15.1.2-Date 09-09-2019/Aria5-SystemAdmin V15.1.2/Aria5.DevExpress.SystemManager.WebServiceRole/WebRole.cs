using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.WindowsAzure;
using Microsoft.WindowsAzure.Diagnostics;
using Microsoft.WindowsAzure.ServiceRuntime;

namespace Aria5.DevExpress.SystemManager.WebServiceRole
{
    public class WebRole : RoleEntryPoint
    {
        public override bool OnStart()
        {
            //Mina.B 2015-09-10 fix a bug in publish web service due to nul reference exeption [Begin]
            try
            {
                // To enable the AzureLocalStorageTraceListner, uncomment relevent section in the web.config  
                DiagnosticMonitorConfiguration diagnosticConfig = DiagnosticMonitor.GetDefaultInitialConfiguration();
                diagnosticConfig.Directories.ScheduledTransferPeriod = TimeSpan.FromMinutes(1);
                diagnosticConfig.Directories.DataSources.Add(AzureLocalStorageTraceListener.GetLogDirectory());

                // For information on handling configuration changes
                // see the MSDN topic at http://go.microsoft.com/fwlink/?LinkId=166357.
            }
            catch (Exception ex) { }
            //Mina.B 2015-09-10 fix a bug in publish web service due to nul reference exeption [End]
            return base.OnStart();
        }
    }
}
