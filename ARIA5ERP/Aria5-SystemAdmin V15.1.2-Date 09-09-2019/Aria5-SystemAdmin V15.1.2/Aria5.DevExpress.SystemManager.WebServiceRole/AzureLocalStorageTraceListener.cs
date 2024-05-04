using System;
using System.Diagnostics;
using System.IO;
using Microsoft.WindowsAzure.Diagnostics;
using Microsoft.WindowsAzure.ServiceRuntime;

namespace Aria5.DevExpress.SystemManager.WebServiceRole
{
    public class AzureLocalStorageTraceListener : XmlWriterTraceListener
    {
        public AzureLocalStorageTraceListener()
            : base(Path.Combine(AzureLocalStorageTraceListener.GetLogDirectory().Path, "Aria5.DevExpress.SystemManager.WebServiceRole.svclog"))
        {
        }

        public static DirectoryConfiguration GetLogDirectory()
        {
            DirectoryConfiguration directory = new DirectoryConfiguration();
            directory.Container = "wad-tracefiles";
            directory.DirectoryQuotaInMB = 10;
            directory.Path = RoleEnvironment.GetLocalResource("Aria5.DevExpress.SystemManager.WebServiceRole.svclog").RootPath;
            return directory;
        }
    }
}
