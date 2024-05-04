using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.WindowsAzure;
using Microsoft.WindowsAzure.Diagnostics;
using Microsoft.WindowsAzure.ServiceRuntime;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole
{
    public class WebRole : RoleEntryPoint
    {
        public override bool OnStart()
        {
            //Mina.B 2015-09-14 fix bug null reference exception [Begin]
            try
            {
                // To enable the AzureLocalStorageTraceListner, uncomment relevent section in the web.config  
                DiagnosticMonitorConfiguration diagnosticConfig = DiagnosticMonitor.GetDefaultInitialConfiguration();
                diagnosticConfig.Directories.ScheduledTransferPeriod = TimeSpan.FromMinutes(1);
                //diagnosticConfig.Directories.DataSources.Add(AzureLocalStorageTraceListener.GetLogDirectory());

                // For information on handling configuration changes
                // see the MSDN topic at http://go.microsoft.com/fwlink/?LinkId=166357.
            }
            catch (Exception) { }
            //Mina.B 2015-09-14 fix bug null reference exception [End]
            return base.OnStart();
        }
    }
}
