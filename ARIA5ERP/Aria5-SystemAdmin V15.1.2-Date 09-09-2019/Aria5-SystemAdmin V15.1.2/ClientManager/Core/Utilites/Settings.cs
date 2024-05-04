using System;
using System.Collections.Generic;
using System.Data.Odbc;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Core.Utilites
{
    public class Settings
    {
        public string ClientCode { get; set; }

        public string ClientName { get; set; }

        public string ClientPhone { get; set; }

        public string SharedDirectoryPath { get; set; }

        public string SharedServerName { get; set; }

        public string SharedDirectoryLocalPath { get; set; }

        public string SqlDBDirectoryPath { get; set; }

        public string AriaSourceDirectoryPath { get; set; }

        public string AriaMasterSourceDirectoryPath { get; set; }

        public string ClientSharedDirectory
        {
            get
            {
                return SharedDirectoryPath.EndsWithBackSlash() + ClientCode + "SH\\";
            }
        }

        public string ClientSharedDirectoryLocalPath
        {
            get
            {
                return SharedDirectoryLocalPath.EndsWithBackSlash() + ClientCode + "SH\\";
            }
        }

        public string ClientDBFsDirectory
        {
            get
            {
                return ClientSharedDirectory.EndsWithBackSlash() + "Aria4XP\\DBFS\\";
            }
        }

        public string ClientSysFilesDirectory
        {
            get
            {
                return ClientSharedDirectory.EndsWithBackSlash() + "Aria4XP\\SysFiles\\";
            }
        }

        public string SourceDBFsDirectory
        {
            get
            {
                return AriaSourceDirectoryPath.EndsWithBackSlash() + "Aria4XP\\DBFS\\";
            }
        } 

        public string ClientSQLDirectory
        {
            get
            {
                return SqlDBDirectoryPath.EndsWithBackSlash() + ClientCode;
            }
        }

        public SQLInfo Aria4SqlInfo { get; set; }

        public SQLInfo ClientMasterSqlInfo { get; set; }

        public SQLInfo SystemMasterSqlInfo { get; set; }

        public OdbcConnection SysFilesConnection
        {
            get
            {
                return SqlHelper.GetFoxConnection(string.Format("{0}Aria4XP\\SYSFILES", ClientSharedDirectory.EndsWithBackSlash()));
            }
        }

        public List<Company> Companies { get; set; }

        public string ClientAria4DBName(string companyCode)
        {
            return string.Format("{0}_LDB{1}", ClientCode, companyCode);
        }

        /// <summary>
        /// X:\\Aria4XP\\
        /// </summary>
        public string Aria4MappedPath { get; set; }

        public List<Credential> ClientUsers { get; set; }

        public List<string> ClientSelectedRoles { get; set; }

        public string DomainAdminUserName { get; set; }

        public string DomainAdminPassword { get; set; }

        public string DomainName { get; set; }

        public string ActivationKeyFilePath { get; set; }

        public const string ActKeyFileName = "ACT_KEY.BIN";

        public string DefaultActKeyPath
        {
            get
            {
                return string.Format("{0}Aria4XP\\SYSFILES\\", ClientSharedDirectory).EndsWithBackSlash();
            }
        }

        public List<string> SelectedApps { get; set; }

        public List<Actions> NeededActions { get; set; }
    }
}