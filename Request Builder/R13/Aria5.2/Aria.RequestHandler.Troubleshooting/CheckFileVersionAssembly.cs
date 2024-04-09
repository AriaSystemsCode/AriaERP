using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using System.IO;

namespace Aria.RequestHandler.Troubleshooting
{
    public class CheckFileVersionAssembly : Check
    {
        private string _assemblyName;
        public string AssemblyName
        {
            get { return _assemblyName; }
            set { _assemblyName = value; }
        }

        private string _assemblyGUID;
        public string AssemblyGUID
        {
            get { return _assemblyGUID; }
            set { _assemblyGUID = value; }
        }

        private string _assemblyPath;
        public string AssemblyPath
        {
            get { return _assemblyPath; }
            set { _assemblyPath = value; }
        }

        private string _currentVersion;
        public string CurrentVersion
        {
            get { return _currentVersion; }
            set { _currentVersion = value; }
        }

        public CheckFileVersionAssembly(string AssemblyName, string AssemblyGUID, string CurrentVersion)
        {
            _assemblyName = AssemblyName;
            _assemblyGUID = AssemblyGUID;
            _assemblyPath = AriaVars.cAssemblyPath + "\\" + AssemblyName + ".DLL";
            _currentVersion = CurrentVersion;
            CheckAction = "Check File Version of " + AssemblyName + " (Current Version is " + CurrentVersion + " )";
            CheckResult = CheckResult.Failed;
        }

        public override void GetCheckResult()
        {
            if (ParentCheck != null && ParentCheck.CheckResult == CheckResult.Failed)
            {
                CheckResult = CheckResult.Failed;
                CheckError = "Parent check failed";
                return;
            }

            if (File.Exists(AssemblyPath))
            {
                FileVersionInfo assemblyFileVersionInfo = FileVersionInfo.GetVersionInfo(AssemblyPath);
                string fileVersion = assemblyFileVersionInfo.FileVersion;

                if (fileVersion == CurrentVersion)
                {
                    CheckResult = CheckResult.Succeed;                    


                }
                else
                {
                    CheckResult = CheckResult.Failed;
                    CheckError = "File Version is " + fileVersion + " while the Current File Version is " + CurrentVersion;
                }
            }
            else
            {
                CheckResult = CheckResult.Failed;
                CheckError = "File " + AssemblyPath + " is not found under GAC_MSIL folder";
            }
        }
    }
}
