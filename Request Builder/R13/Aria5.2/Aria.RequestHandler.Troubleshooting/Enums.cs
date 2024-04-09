using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.RequestHandler.Troubleshooting
{
    //public enum Checks
    //{
    //    CheckFileVersionFoxProgram,
    //    CheckFileVersionFoxClass,
    //    CheckFileVersionFoxScreen,
    //    CheckFileVersionAssembly
    //}

    public static class AriaVars
    {
        public static string cAria4SharedPath = "";//@"D:\ARIA4XP\";
        public static string Aria4SharedPath
        {
            get { return cAria4SharedPath; }
            set
            {
                cAria4SharedPath = value;
                cReportsPath = cAria4SharedPath + @"Reports\";
                cSrvProgPath = cAria4SharedPath + @"SRVPRGs\";
                cScreensPath = cAria4SharedPath + @"Screens\";
                cClassesPath = cAria4SharedPath + @"Classes\";
                cSRVCLSSPath = cAria4SharedPath + @"SRVCLSS\";
            }
        }
        public static string cReportsPath = cAria4SharedPath + @"Reports\";
        public static string cSrvProgPath = cAria4SharedPath + @"SRVPRGs\";
        public static string cScreensPath = cAria4SharedPath + @"Screens\";
        public static string cClassesPath = cAria4SharedPath + @"Classes\";
        public static string cSRVCLSSPath = cAria4SharedPath + @"SRVCLSS\";
        public static string cAssemblyPath = Environment.GetEnvironmentVariable("SystemRoot") + @"\Assembly\GAC_MSIL\";
        
        public static string ServerName;
        public static DBLoginTypes ServerLoginType;
        public static string UserName;
        public static string Password;

    }

    public enum CheckResult
    {
        NotChecked,
        Succeed,
        Failed
    }

    public enum FileType
    {
        FoxProgram,
        FoxClass,
        FoxScreen,
        DLL
    }

    public enum DBLoginTypes
    {
        WindowAuthentication,
        SqlServerAuthentication
    }
}
