using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace Aria.RequestHandler.Troubleshooting
{
    public class CheckFileVersionFoxServerProgram : Check
    {
        private string _programName;
        public string ProgramName
        {
            get { return _programName; }
            set { _programName = value; }
        }

        private string _programModule;
        public string ProgramModule
        {
            get { return _programModule; }
            set { _programModule = value; }
        }

        private string _programPath;
        public string ProgramPath
        {
            get { return _programPath; }
            set { _programPath = value; }
        }

        private string _currentVersion;
        public string CurrentVersion
        {
            get { return _currentVersion; }
            set { _currentVersion = value; }
        }

        public CheckFileVersionFoxServerProgram(string ProgramName, string ProgramModule, string ProgramPath, string CurrentVersion)
        {
            _programName = ProgramName;
            _programModule = ProgramModule;
            _programPath = ProgramPath;
            _currentVersion = CurrentVersion;
            CheckAction = "Check File Version of " + ProgramName + " (Current Version is " + CurrentVersion + " )";
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
            else if (!File.Exists(AriaVars.cSrvProgPath + ProgramPath))
            {
                CheckResult = CheckResult.Failed;
                CheckError = "File " + AriaVars.cSrvProgPath + ProgramPath + " not found";
                return;
            }
            else if (Path.GetExtension(AriaVars.cSrvProgPath + ProgramPath) != ".FXP")
            {
                CheckResult = CheckResult.Failed;
                CheckError = "File " + AriaVars.cSrvProgPath + ProgramPath + " is not a Fox Program";
                return;
            }

            string cAppDataPath = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);

            VisualFoxpro.FoxApplication fox = new VisualFoxpro.FoxApplication();
            fox.DefaultFilePath = File.Exists(Directory.GetCurrentDirectory() + @"\GetFileVersion.FXP") ? Directory.GetCurrentDirectory() : AriaVars.cAssemblyPath;
            fox.DoCmd("DO GetFileVersion.FXP WITH '" + AriaVars.cSrvProgPath + ProgramPath + "', '" + cAppDataPath + @"\Temp.txt'");
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
                CheckError = "File Version is not available from this program";
            }
        }
    }
}
