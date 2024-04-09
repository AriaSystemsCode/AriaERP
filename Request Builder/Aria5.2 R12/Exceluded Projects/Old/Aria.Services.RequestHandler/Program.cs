using System.Collections.Generic;
using System.ServiceProcess;
using System.Text;
using System;
using System.Diagnostics;

namespace Aria.Services.RequestHandler
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {

            EventLog.WriteEntry("Aria.Services.RequestHandler", "Test", EventLogEntryType.Information);
            try
            {
                ServiceBase[] ServicesToRun;

                // More than one user Service may run within the same process. To add
                // another service to this process, change the following line to
                // create a second service object. For example,

                ServicesToRun = new ServiceBase[] { new AriaRequestHandler() };

                ServiceBase.Run(ServicesToRun);
            }
            catch (Exception Ex)
            {
                EventLog.WriteEntry("Aria.Services.RequestHandler", Ex.Message.ToString(), EventLogEntryType.Information);
            }
        }
    }
}