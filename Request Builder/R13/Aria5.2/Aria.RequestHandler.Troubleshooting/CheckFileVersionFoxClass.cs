using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace Aria.RequestHandler.Troubleshooting
{
    public class CheckFileVersionFoxClass : Check
    {
        private string _className;
        public string ClassName
        {
            get { return _className; }
            set { _className = value; }
        }

        private string _classModule;
        public string ClassModule
        {
            get { return _classModule; }
            set { _classModule = value; }
        }

        private string _classPath;
        public string ClassPath
        {
            get { return _classPath; }
            set { _classPath = value; }
        }

        private string _currentVersion;
        public string CurrentVersion
        {
            get { return _currentVersion; }
            set { _currentVersion = value; }
        }

        public CheckFileVersionFoxClass(string ClassName, string ClassModule, string ClassPath, string CurrentVersion)
        {
            _className = ClassName;
            _classModule = ClassModule;
            _classPath = ClassPath;
            _currentVersion = CurrentVersion;
            CheckAction = "Check File Version of " + ClassName + " (Current Version is " + CurrentVersion + " )"; 
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
            else if (!File.Exists(AriaVars.cSRVCLSSPath + ClassPath))
            {
                CheckResult = CheckResult.Failed;
                CheckError = "File " + AriaVars.cSRVCLSSPath + ClassPath + " not found";
                return;
            }
            else if (Path.GetExtension(AriaVars.cSRVCLSSPath + ClassPath) != ".VCX")
            {
                CheckResult = CheckResult.Failed;
                CheckError = "File " + AriaVars.cSRVCLSSPath + ClassPath + " is not a Fox Class";
                return;
            }

            string cAppDataPath = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);

            VisualFoxpro.FoxApplication fox = new VisualFoxpro.FoxApplication();
            fox.DefaultFilePath = File.Exists(Directory.GetCurrentDirectory() + @"\GetFileVersion.FXP") ? Directory.GetCurrentDirectory() : AriaVars.cAssemblyPath;
            fox.DoCmd("DO GetFileVersion.FXP WITH '" + AriaVars.cSRVCLSSPath + ClassPath + "', '" + cAppDataPath + @"\Temp.txt'");
            fox.Quit();
            
            if (File.Exists(cAppDataPath + @"\Temp.txt"))
            {
                StreamReader reader = new StreamReader(cAppDataPath + @"\Temp.txt");
                string fileVersion = reader.ReadToEnd();
                reader.Close();

                if (fileVersion == CurrentVersion)
                {
                    CheckResult = CheckResult.Succeed;
                }
                else
                {
                    CheckResult = CheckResult.Failed;
                    CheckError = "File Version is " + fileVersion + " while the Current File Version is " + CurrentVersion;
                }

                File.Delete(cAppDataPath + @"\Temp.txt");
            }
            else
            {
                CheckResult = CheckResult.Failed;
                CheckError = "File Version is not available from this class";
            }
        }
    }
}
