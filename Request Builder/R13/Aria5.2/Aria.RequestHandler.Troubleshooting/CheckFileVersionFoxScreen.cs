using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace Aria.RequestHandler.Troubleshooting
{
    public class CheckFileVersionFoxScreen : Check
    {
        private string _screenName;
        public string ScreenName
        {
            get { return _screenName; }
            set { _screenName = value; }
        }

        private string _screenModule;
        public string ScreenModule
        {
            get { return _screenModule; }
            set { _screenModule = value; }
        }

        private string _screenPath;
        public string ScreenPath
        {
            get { return _screenPath; }
            set { _screenPath = value; }
        }

        private string _currentVersion;
        public string CurrentVersion
        {
            get { return _currentVersion; }
            set { _currentVersion = value; }
        }

        public CheckFileVersionFoxScreen(string ScreenName, string ScreenModule, string ScreenPath, string CurrentVersion)
        {
            _screenName = ScreenName;
            _screenModule = ScreenModule;
            _screenPath = ScreenPath;
            _currentVersion = CurrentVersion;
            CheckAction = "Check File Version of " + ScreenName + " (Current Version is " + CurrentVersion + " )";
            CheckResult = CheckResult.NotChecked;
        }

        public override void GetCheckResult()
        {            
            if (ParentCheck != null && ParentCheck.CheckResult == CheckResult.Failed)
            {
                CheckResult = CheckResult.Failed;
                CheckError = "Parent check failed";
                return;
            }
            else if (!File.Exists(AriaVars.cScreensPath + ScreenPath))
            {
                CheckResult = CheckResult.Failed;
                CheckError = "File " + AriaVars.cScreensPath + ScreenPath + " not found";
                return;
            }
            else if (Path.GetExtension(AriaVars.cScreensPath + ScreenPath) != ".SCX")
            {
                CheckResult = CheckResult.Failed;
                CheckError = "File " + AriaVars.cScreensPath + ScreenPath + " is not a Fox Screen";
                return;
            }

            string cAppDataPath = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);

            VisualFoxpro.FoxApplication fox = new VisualFoxpro.FoxApplication();
            fox.DefaultFilePath = File.Exists(Directory.GetCurrentDirectory() + @"\GetFileVersion.FXP") ? Directory.GetCurrentDirectory() : AriaVars.cAssemblyPath;
            fox.DoCmd("DO GetFileVersion.FXP WITH '" + AriaVars.cScreensPath + ScreenPath + "', '" + cAppDataPath + @"\Temp.txt'");
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
                CheckError = "File Version is not available from this screen";
            }
        }
    }
}
