using Core.Utilites;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Security.AccessControl;
using System.Security.Principal;
using System.Reflection;

namespace Core.Business
{
    public class IO
    {
        public IO(Settings settings)
        {
            this.Settings = settings;
        }

        public Settings Settings { get; set; }

        public void CreateClientAriaFolder()
        {
            if (!Directory.Exists(Settings.ClientSharedDirectory))
                Directory.CreateDirectory(Settings.ClientSharedDirectory);
        }

        public void CreateClientSQLFolder()
        {
            if (!Directory.Exists(Settings.ClientSQLDirectory))
                Directory.CreateDirectory(Settings.ClientSQLDirectory);
        }

        public void Copy_AriaSource_ClientFolder()
        {
            string source = Settings.AriaSourceDirectoryPath.EndsWithBackSlash();
            string dest = Settings.ClientSharedDirectory;
            Extensions.DirectoryCopy(source + "Aria4XP", dest + "Aria4XP");
            if (Settings.ClientSelectedRoles.Contains("AriaEDI"))
                Extensions.DirectoryCopy(Settings.AriaSourceDirectoryPath + "ARIA3EDI", Settings.ClientSharedDirectory + "ARIA3EDI");
        }

        public void Copy_AriaMaster_ClientSQLFolder()
        {
            Extensions.DirectoryCopy(Settings.AriaMasterSourceDirectoryPath, Settings.ClientSQLDirectory);

            string mdfFile = Settings.ClientSQLDirectory.EndsWithBackSlash() + Settings.ClientMasterSqlInfo.DataBaseName + ".mdf";
            string ldfFile = Settings.ClientSQLDirectory.EndsWithBackSlash() + Settings.ClientMasterSqlInfo.DataBaseName + "_log.ldf";

            if (!File.Exists(mdfFile))
                File.Move(Settings.ClientSQLDirectory.EndsWithBackSlash() + "Aria.Master.mdf", mdfFile);

            if (!File.Exists(ldfFile))
                File.Move(Settings.ClientSQLDirectory.EndsWithBackSlash() + "Aria.Master_log.ldf", ldfFile);

            //Sql Service Usualy runs under "Network Service" account , so we need to give it access to the DB so it can attach it.
            DirectorySecurity sec = Directory.GetAccessControl(Settings.ClientSQLDirectory);
            SecurityIdentifier networkServiceUser = new SecurityIdentifier(WellKnownSidType.WorldSid, null);
            sec.AddAccessRule(new FileSystemAccessRule(networkServiceUser, FileSystemRights.FullControl, InheritanceFlags.ContainerInherit | InheritanceFlags.ObjectInherit, PropagationFlags.None, AccessControlType.Allow));
            Directory.SetAccessControl(Settings.ClientSQLDirectory, sec);
        }

        public void DeleteActKey()
        {
            string actkeyOldfile = Settings.DefaultActKeyPath + Settings.ActKeyFileName;
            if (File.Exists(actkeyOldfile))
                File.Delete(actkeyOldfile);
        }

        public void CopyActKey()
        {
            string actkeyfile = Settings.DefaultActKeyPath + Settings.ActKeyFileName;
            if (!File.Exists(actkeyfile))
                File.Copy(Settings.ActivationKeyFilePath, actkeyfile);
        }

        public void CreateClientSettingsXML()
        {
            string sourceXMLPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().CodeBase.Replace("file:///", "")).EndsWithBackSlash();
            sourceXMLPath += @"Resources\Client_Setting.XML";

            string xmlContent = File.ReadAllText(sourceXMLPath);
            xmlContent = string.Format(xmlContent, Settings.ClientCode);

            string targetFile = Settings.ClientSysFilesDirectory.EndsWithBackSlash() + "Client_Setting.XML";
            File.WriteAllText(targetFile, xmlContent);
        }

        public void CreateCompanyFiles(Company company)
        {
            string company99Path = Settings.SourceDBFsDirectory.EndsWithBackSlash() + "99\\";
            string newCompanyPath = Settings.ClientDBFsDirectory.EndsWithBackSlash() + company.Code + "\\";
            if (!Directory.Exists(newCompanyPath))
                Extensions.DirectoryCopy(company99Path, newCompanyPath);
        }

        public void CreateAllCompaniesFiles()
        {
            //Delete 99 folder and any other folder in dbfs before creating the companies
            //System.IO.DirectoryInfo dbfsFolderInfo = new DirectoryInfo(Settings.ClientDBFsDirectory.EndsWithBackSlash());
            //foreach (FileInfo file in dbfsFolderInfo.GetFiles())
            //    file.Delete();
            //foreach (DirectoryInfo dir in dbfsFolderInfo.GetDirectories())
            //    dir.Delete(true);


            foreach (Company comp in Settings.Companies)
            {
                CreateCompanyFiles(comp);
            }
        }
    }
}