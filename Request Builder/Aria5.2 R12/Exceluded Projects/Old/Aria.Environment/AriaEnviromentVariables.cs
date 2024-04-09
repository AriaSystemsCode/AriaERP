// T20110803.0001 MAH June 13 2011
using System.Xml;
using System;
using System.Data.Odbc;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Text;
using System.Diagnostics;
using System.Windows.Forms;
using System.IO;
using System.Runtime.InteropServices;


namespace Aria.Environment
{
    public class AriaEnviromentVariables : MarshalByRefObject
    {
        [DllImport("mpr.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern int WNetGetConnection(
            [MarshalAs(UnmanagedType.LPTStr)] string localName,
            [MarshalAs(UnmanagedType.LPTStr)] StringBuilder remoteName,
            ref int length);

        private string _vfpConnectionStringTemplate = @"Driver={Microsoft Visual FoxPro Driver};sourcedb=<DataBaseLocation>;sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes";
        private string _SQLAuthenticatedConnectionStringTemplate = @"Data Source=<Server>;Initial Catalog=<DBName>;User ID=<UserID>;Password=<Password>;Trusted_Connection=no";
        private string _SQLWindowsAuthenticatedConnectionStringTemplate = @"Data Source=<Server>;Initial Catalog=<DBName>;Integrated Security=True";
        private string _SQLConnectionStringTemplate = "Driver={SQL Server};server=<Server>;DATABASE=<DBName>;UID=<UserID>;PWD=<Password>";

        private Dictionary<string, string> _cached27DataConnectionString = new Dictionary<string, string>();
        private Dictionary<string, string> _cached04DataConnectionString = new Dictionary<string, string>();

        private string _aria27SharedPath;
        public string Aria27SharedPath
        {
            get { return _aria27SharedPath; }
            set { _aria27SharedPath = value; }
        }

        private string _aria40SharedPath;
        public string Aria40SharedPath
        {
            get { return _aria40SharedPath; }
            set { _aria40SharedPath = value; }
        }

        private string _aria40ClientPath;
        public string Aria40ClientPath
        {
            get { return _aria40ClientPath; }
            set { _aria40ClientPath = value; }
        }

        private string _clientID = "";
        public string ClientID
        {
            get { return _clientID; }
            set
            {
                _clientID = value;

                if (!string.IsNullOrEmpty(_clientID))
                {
                    ReadClientSettings(_clientID);
                }

            }
        }

        private string _companyID = "";
        public string CompanyID
        {
            get { return _companyID; }
            set
            {
                _companyID = value;
                ReadCompanySettings(_companyID);
            }
        }

        private string _requestServerName = "";
        public string RequestServerName
        {
            get { return _requestServerName; }
            set { _requestServerName = value; }
        }

        private int _rpcPort = 0;
        public int RpcPort
        {
            get { return _rpcPort; }
            set { _rpcPort = value; }
        }

        private string _aria27SystemFilesPath = "";
        public string Aria27SystemFilesPath
        {
            get { return _aria27SystemFilesPath; }
            set { _aria27SystemFilesPath = value; }
        }

        private string _aria27SystemFilesConnectionString = "";
        public string Aria27SystemFilesConnectionString
        {
            get { return _aria27SystemFilesConnectionString; }
            set { _aria27SystemFilesConnectionString = value; }
        }

        private string _aria40SystemFilesPath = "";
        public string Aria40SystemFilesPath
        {
            get { return _aria40SystemFilesPath; }
            set { _aria40SystemFilesPath = value; }
        }


        private string _aria40SystemFilesConnectionString = "";
        public string Aria40SystemFilesConnectionString
        {
            get { return _aria40SystemFilesConnectionString; }
            set { _aria40SystemFilesConnectionString = value; }
        }

        private string _dbServerName = "";
        public string DbServerName
        {
            get { return _dbServerName; }
            set { _dbServerName = value; }
        }

        private DatabaseServerLoginTypes _dbServerLoginType = DatabaseServerLoginTypes.NotSet;
        public DatabaseServerLoginTypes DbServerLoginType
        {
            get { return _dbServerLoginType; }
            set { _dbServerLoginType = value; }
        }

        private string _dbServerUserName = "";
        public string DbServerUserName
        {
            get { return _dbServerUserName; }
            set { _dbServerUserName = value; }
        }

        private string _dbServerPassword = "";
        public string DbServerPassword
        {
            get { return _dbServerPassword; }
            set { _dbServerPassword = value; }
        }
        
        private string _aria50SystemFilesConnectionString = "";
        public string Aria50SystemFilesConnectionString
        {
            get { return _aria50SystemFilesConnectionString; }
            set { _aria50SystemFilesConnectionString = value; }
        }

        private string _aria50SystemFilesConnectionStringOdbc = "";
        public string Aria50SystemFilesConnectionStringOdbc
        {
            get { return _aria50SystemFilesConnectionStringOdbc; }
            set { _aria50SystemFilesConnectionStringOdbc = value; }
        }

        private string _aria50ClientSystemFilesConnectionString = "";
        public string Aria50ClientSystemFilesConnectionString
        {
            get { return _aria50ClientSystemFilesConnectionString; }
            set { _aria50ClientSystemFilesConnectionString = value; }
        }

        private string _aria50ClientSystemFilesConnectionStringOdbc = "";
        public string Aria50ClientSystemFilesConnectionStringOdbc
        {
            get { return _aria50ClientSystemFilesConnectionStringOdbc; }
            set { _aria50ClientSystemFilesConnectionStringOdbc = value; }
        }

        private string _openRowSetDbServerName = "";
        public string OpenRowSetDbServerName
        {
            get 
            { 
                return _openRowSetDbServerName; 
            }
            set 
            {
                string result = "";
                for(int i =0; i < value.Length; i++)
                {
                    if (Convert.ToByte(Convert.ToChar(value.Substring(i, 1))) != 0)
                    {
                        result += Convert.ToChar(value.Substring(i, 1)).ToString();
                    }
                    else
                    {
                        break;
                    }
                }
                
                _openRowSetDbServerName = result; 
            }
        }

        private string _openRowSetDbServerUserName = "";
        public string OpenRowSetDbServerUserName
        {
            get { return _openRowSetDbServerUserName; }
            set 
            {
                string result = "";
                for (int i = 0; i < value.Length; i++)
                {
                    if (Convert.ToByte(Convert.ToChar(value.Substring(i, 1))) != 0)
                    {
                        result += Convert.ToChar(value.Substring(i, 1)).ToString();
                    }
                    else
                    {
                        break;
                    }
                }

                _openRowSetDbServerUserName = result; 
            }
        }

        private DatabaseServerLoginTypes _openRowSetDbServerLoginType = DatabaseServerLoginTypes.NotSet;
        public DatabaseServerLoginTypes OpenRowSetDbServerLoginType
        {
            get { return _openRowSetDbServerLoginType; }
            set { _openRowSetDbServerLoginType = value; }
        }

        private string _openRowSetDbServerPassword = "";
        public string OpenRowSetDbServerPassword
        {
            get { return _openRowSetDbServerPassword; }
            set 
            {
                string result = "";
                for (int i = 0; i < value.Length; i++)
                {
                    if (Convert.ToByte(Convert.ToChar(value.Substring(i, 1))) != 0)
                    {
                        result += Convert.ToChar(value.Substring(i, 1)).ToString();
                    }
                    else
                    {
                        break;
                    }
                }

                _openRowSetDbServerPassword = result; 
            }
        }
        
        private string _ariaOpenRowSetConnectionString = "";
        public string AriaOpenRowSetConnectionString
        {
            get { return _ariaOpenRowSetConnectionString; }
            set { _ariaOpenRowSetConnectionString = value; }
        }

        private int _ariaSMTPPort;
        public int AriaSMTPPort
        {
            get { return _ariaSMTPPort; }
            set { _ariaSMTPPort = value; }
        }

        private string _ariaSMTPHost = "";
        public string AriaSMTPHost
        {
            get { return _ariaSMTPHost; }
            set { _ariaSMTPHost = value; }
        }

        private string _ariaSMTPUserName = "";
        public string AriaSMTPUserName
        {
            get { return _ariaSMTPUserName; }
            set { _ariaSMTPUserName = value; }
        }

        private string _ariaSMTPPassword = "";
        public string AriaSMTPPassword
        {
            get { return _ariaSMTPPassword; }
            set { _ariaSMTPPassword = value; }
        }

        private string _ariaSenderMail = "";
        public string AriaSenderMail
        {
            get { return _ariaSenderMail; }
            set { _ariaSenderMail = value; }
        }

        private string _ariaSenderName = "";
        public string AriaSenderName
        {
            get { return _ariaSenderName; }
            set { _ariaSenderName = value; }
        }

        private bool _ariaSsl;
        public bool AriaSsl
        {
            get { return _ariaSsl; }
            set { _ariaSsl = value; }
        }

        private int _ariaMaxRecordsPerAgent;
        public int AriaMaxRecordsPerAgent
        {
            get { return _ariaMaxRecordsPerAgent; }
            set { _ariaMaxRecordsPerAgent = value; }
        }
        

        public AriaEnviromentVariables()
        {
            ReadServerSettings();
        }

        public AriaEnviromentVariables(string clientId)
        {
            if (string.IsNullOrEmpty(clientId))
            {
                ReadServerSettings();
            }
            else
            {
                ReadServerSettings();

                _clientID = clientId;

                ReadClientSettings(clientId);
            }
        }

        public AriaEnviromentVariables(string clientId, string companyId)
        {
            ReadServerSettings();

            if (!string.IsNullOrEmpty(clientId))
            {
                _clientID = clientId;

                ReadClientSettings(clientId);
            }

            if (!string.IsNullOrEmpty(companyId))
            {
                _companyID = companyId;

                ReadCompanySettings(clientId);
            }

        }

        private void ReadServerSettings()
        {
            string xmlFileName = System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine);

            XmlDocument xmlDocument = new XmlDocument();

            xmlDocument.Load(xmlFileName);

            XmlElement documentElement = xmlDocument.DocumentElement;

            string configurationFilePath = "";

            for (int index = 0; index < xmlDocument.DocumentElement.ChildNodes.Count; index++)
            {
                if (documentElement.ChildNodes[index].Name == "ConfigurationFile")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "ConfigurationFilePath")
                        {
                            configurationFilePath = xmlNode.InnerText;

                            xmlDocument.Load(configurationFilePath);

                            documentElement = xmlDocument.DocumentElement;

                            break;
                        }
                    }
                }

                if (configurationFilePath != "")
                {
                    break;
                }
            }

            for (int index = 0; index < xmlDocument.DocumentElement.ChildNodes.Count; index++)
            {
                if (documentElement.ChildNodes[index].Name == "DatabaseSetup")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "ServerName")
                        {
                            _dbServerName = xmlNode.InnerText;
                        }
                        else if (xmlNode.Name == "ServerLoginType")
                        {
                            _dbServerLoginType = (DatabaseServerLoginTypes)Enum.Parse(typeof(DatabaseServerLoginTypes), xmlNode.InnerText);
                        }
                        else if (xmlNode.Name == "UserName")
                        {
                            _dbServerUserName = xmlNode.InnerText;
                        }
                        else if (xmlNode.Name == "Password")
                        {
                            _dbServerPassword = xmlNode.InnerText;
                        }
                    }

                    switch (_dbServerLoginType)
                    {
                        case DatabaseServerLoginTypes.WindowAuthentication:
                            _aria50SystemFilesConnectionString = _SQLWindowsAuthenticatedConnectionStringTemplate.Replace("<Server>", _dbServerName)
                                                                                                                 .Replace("<DBName>", "System.Master");
                            break;

                        case DatabaseServerLoginTypes.SqlServerAuthentication:
                            _aria50SystemFilesConnectionString = _SQLAuthenticatedConnectionStringTemplate.Replace("<Server>", _dbServerName)
                                                                                                          .Replace("<DBName>", "System.Master")
                                                                                                          .Replace("<UserID>", _dbServerUserName)
                                                                                                          .Replace("<Password>", _dbServerPassword);
                            break;
                    }

                    _aria50SystemFilesConnectionStringOdbc = _SQLConnectionStringTemplate.Replace("<Server>", _dbServerName)
                                                                                         .Replace("<DBName>", "System.Master")
                                                                                         .Replace("<UserID>", _dbServerUserName)
                                                                                         .Replace("<Password>", _dbServerPassword);
                }
                else if (documentElement.ChildNodes[index].Name == "FileServer")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "Aria27SharedPath")
                        {
                            _aria27SharedPath = xmlNode.InnerText.TrimEnd();
                            if (!_aria27SharedPath.EndsWith(@"\"))
                            {
                                _aria27SharedPath += @"\";
                            }
                        }
                        else if (xmlNode.Name == "Aria40SharedPath")
                        {
                            _aria40SharedPath = xmlNode.InnerText.TrimEnd();
                            if (!_aria40SharedPath.EndsWith(@"\"))
                            {
                                _aria40SharedPath += @"\";
                            }
                        }
                    }
                }
                else if (documentElement.ChildNodes[index].Name == "RequestServer")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "Port")
                        {
                            _rpcPort = Convert.ToInt32(xmlNode.InnerText);
                        }

                        if (xmlNode.Name == "MaxRecordsPerAgent")
                        {
                            _ariaMaxRecordsPerAgent = Convert.ToInt32(xmlNode.InnerText);
                        }
                    }
                }
                else if (documentElement.ChildNodes[index].Name == "OpenRowSetServer")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "ServerName")
                        {
                            _openRowSetDbServerName = xmlNode.InnerText;
                        }
                        else if (xmlNode.Name == "ServerLoginType")
                        {
                            _openRowSetDbServerLoginType = (DatabaseServerLoginTypes)Enum.Parse(typeof(DatabaseServerLoginTypes), xmlNode.InnerText);
                        }
                        else if (xmlNode.Name == "UserName")
                        {
                            _openRowSetDbServerUserName = xmlNode.InnerText;
                        }
                        else if (xmlNode.Name == "Password")
                        {
                            _openRowSetDbServerPassword = xmlNode.InnerText;
                        }
                    }

                    switch (_openRowSetDbServerLoginType)
                    {
                        case DatabaseServerLoginTypes.WindowAuthentication:
                            _ariaOpenRowSetConnectionString = _SQLWindowsAuthenticatedConnectionStringTemplate.Replace("<Server>", _openRowSetDbServerName)
                                                                                                              .Replace("<DBName>", "master");
                            
                            break;

                        case DatabaseServerLoginTypes.SqlServerAuthentication:
                            _ariaOpenRowSetConnectionString = _SQLAuthenticatedConnectionStringTemplate.Replace("<Server>", _openRowSetDbServerName)
                                                                                                .Replace("<DBName>", "master")
                                                                                                .Replace("<UserID>", _openRowSetDbServerUserName)
                                                                                                .Replace("<Password>", _openRowSetDbServerPassword);
                            break;
                    }
                }
            }
        }

        private void ReadClientSettings(string clientId)
        {
            SqlConnection aria5Conn = new SqlConnection(Aria50SystemFilesConnectionString);
            aria5Conn.Open();

            SqlCommand clientsCmd = new SqlCommand("select ARIA27SYS, ARIA40SYS, CCONSERVER, CCONDBNAME, CCONUSERID, CCONPASWRD, REQSERVER " +
                                                   "from CLIENTS where CCLIENTID = @ClientID", aria5Conn);

            clientsCmd.Parameters.Add(new SqlParameter("ClientID", clientId));

            DataTable clientRow = new DataTable();
            clientRow.Load(clientsCmd.ExecuteReader());

            _aria27SystemFilesPath = clientRow.Rows[0]["ARIA27SYS"].ToString().TrimEnd();
            _aria27SystemFilesConnectionString = _vfpConnectionStringTemplate.Replace("<DataBaseLocation>", _aria27SystemFilesPath);

            _aria40SystemFilesPath = clientRow.Rows[0]["ARIA40SYS"].ToString().TrimEnd();
            _aria40SystemFilesConnectionString = _vfpConnectionStringTemplate.Replace("<DataBaseLocation>", _aria40SystemFilesPath);

            if (string.IsNullOrEmpty(clientRow.Rows[0]["CCONUSERID"].ToString()) || clientRow.Rows[0]["CCONUSERID"] == DBNull.Value)
            {
                _aria50ClientSystemFilesConnectionString = _SQLWindowsAuthenticatedConnectionStringTemplate
                                                                            .Replace("<Server>", clientRow.Rows[0]["CCONSERVER"].ToString())
                                                                            .Replace("<DBName>", clientRow.Rows[0]["CCONDBNAME"].ToString());
            }
            else
            {
                _aria50ClientSystemFilesConnectionString = _SQLAuthenticatedConnectionStringTemplate
                                                                            .Replace("<Server>", clientRow.Rows[0]["CCONSERVER"].ToString())
                                                                            .Replace("<DBName>", clientRow.Rows[0]["CCONDBNAME"].ToString())
                                                                            .Replace("<UserID>", clientRow.Rows[0]["CCONUSERID"].ToString())
                                                                            .Replace("<Password>", clientRow.Rows[0]["CCONPASWRD"].ToString());
            }

            _aria50ClientSystemFilesConnectionStringOdbc = _SQLConnectionStringTemplate.Replace("<Server>", clientRow.Rows[0]["CCONSERVER"].ToString())
                                                                                       .Replace("<DBName>", clientRow.Rows[0]["CCONDBNAME"].ToString())
                                                                                       .Replace("<UserID>", clientRow.Rows[0]["CCONUSERID"].ToString())
                                                                                       .Replace("<Password>", clientRow.Rows[0]["CCONPASWRD"].ToString());

            _requestServerName = clientRow.Rows[0]["REQSERVER"].ToString();

            string path = _aria40SystemFilesPath.ToUpper().Trim();

            if(!path.EndsWith(@"\")) path = path + @"\";

            Aria40ClientPath = _aria40SystemFilesPath.ToUpper().Trim();

            Aria40ClientPath = Aria40ClientPath.Replace(@"\SQLDICTIONARY", "");

            aria5Conn.Close();
        }

        private void ReadCompanySettings(string companyId)
        {
            EventLog.WriteEntry("Aria.EnviromentVariables.Setup", GetAria27CompanyDataConnectionString(companyId), EventLogEntryType.Information);

            EventLog.WriteEntry("Aria.EnviromentVariables.Setup", "1", EventLogEntryType.Information);
            // Read company setups
            OdbcConnection aria27Conn = new OdbcConnection(GetAria27CompanyDataConnectionString(companyId));
            aria27Conn.Open();

            OdbcCommand setupCmd = new OdbcCommand("SELECT Cfld_name, mdata_def FROM Setups WHERE cFld_Name " +
                                                   "in('M_SMTPSRVR', 'M_SMTPPORT', 'M_SMTPUSER', 'M_SMTPPSS ', " +
                                                   "'M_SNDRNME ' ,'M_SSL     ','M_SMTPUSR2')", aria27Conn);

            EventLog.WriteEntry("Aria.EnviromentVariables.Setup", "2", EventLogEntryType.Information);

            DataTable setupDt = new DataTable();
            setupDt.Load(setupCmd.ExecuteReader());

            if (setupDt.Select("cFld_Name = 'M_SMTPSRVR'").Length > 0)
            {
                _ariaSMTPHost = setupDt.Select("cFld_Name = 'M_SMTPSRVR'")[0]["mdata_def"].ToString();
            }

            int temp = 0;
            if (setupDt.Select("cFld_Name = 'M_SMTPPORT'").Length > 0 &&
                int.TryParse(setupDt.Select("cFld_Name = 'M_SMTPPORT'")[0]["mdata_def"].ToString(), out temp))
            {
                _ariaSMTPPort = int.Parse(setupDt.Select("cFld_Name = 'M_SMTPPORT'")[0]["mdata_def"].ToString());
            }

            if (setupDt.Select("cFld_Name = 'M_SMTPUSER'").Length > 0)
            {
                _ariaSMTPUserName = setupDt.Select("cFld_Name = 'M_SMTPUSER'")[0]["mdata_def"].ToString();
            }

            if (setupDt.Select("cFld_Name = 'M_SMTPPSS'").Length > 0)
            {
                _ariaSMTPPassword = setupDt.Select("cFld_Name = 'M_SMTPPSS'")[0]["mdata_def"].ToString();
            }

            if (setupDt.Select("cFld_Name = 'M_SNDRNME'").Length > 0)
            {
                _ariaSenderName = setupDt.Select("cFld_Name = 'M_SNDRNME'")[0]["mdata_def"].ToString();
            }

            if (setupDt.Select("cFld_Name = 'M_SMTPUSR2'").Length > 0)
            {
                _ariaSenderMail = setupDt.Select("cFld_Name = 'M_SMTPUSR2'")[0]["mdata_def"].ToString();
            }

            string _ssl = "";
            if (setupDt.Select("cFld_Name = 'M_SSL'").Length > 0)
            {
                _ssl = setupDt.Select("cFld_Name = 'M_SSL'")[0]["mdata_def"].ToString().ToUpper().TrimEnd();
            }

            if (_ssl == ".T." || _ssl == "TRUE")
            {
                _ariaSsl = true;
            }
            else
            {
                _ariaSsl = false;
            }

            aria27Conn.Close();
            EventLog.WriteEntry("Aria.EnviromentVariables.Setup", "10", EventLogEntryType.Information);

        }
        
        public string GetAria27CompanyDataConnectionString(string companyID)
        {
            if (_cached27DataConnectionString.ContainsKey(companyID)) return _cached27DataConnectionString[companyID];

            OdbcConnection aria27Conn = new OdbcConnection(Aria27SystemFilesConnectionString);
            aria27Conn.Open();

            OdbcCommand command = new OdbcCommand("SELECT Ccom_ddir FROM Syccomp WHERE Ccomp_id = '" + companyID + "'", aria27Conn);

            DataTable row = new DataTable();
            row.Load(command.ExecuteReader());

            aria27Conn.Close();

            // SABER
            //string RealPath = "";
            //RealPath = row.Rows[0]["Ccom_ddir"].ToString();
            //if (RealPath.Length > 2 && RealPath[1] == ':')
            //{
            //    int errorout = 0;
            //    RealPath = GetUNCPath2(RealPath, out errorout);
            //    if (errorout != 0)
            //    {
            //        SqlConnection Conn = new SqlConnection(Aria50SystemFilesConnectionString);
            //        Conn.Open();
            //        SqlCommand Cmd = new SqlCommand("SELECT * from CLIENTS WHERE CCLIENTID='" + ClientID + "'", Conn);
            //        DataTable Dt = new DataTable();
            //        Dt.Load(Cmd.ExecuteReader());
            //        string cutstr = Dt.Rows[0]["CDATAPATH"].ToString().TrimEnd();
            //        cutstr = cutstr.Trim().ToUpper();
            //        RealPath = Path.Combine(cutstr, row.Rows[0]["Ccom_ddir"].ToString().Substring(3));
            //        Conn.Close();
            //    }
            //}

            string result = _vfpConnectionStringTemplate.Replace("<DataBaseLocation>", ResolveMappedDrive(row.Rows[0]["Ccom_ddir"].ToString().Trim()));

            _cached27DataConnectionString.Add(companyID, result);

            return result;
        }

        public string GetAria04CompanyDataConnectionString(string companyID)
        {
            if (_cached04DataConnectionString.ContainsKey(companyID)) return _cached04DataConnectionString[companyID];

            OdbcConnection aria27Conn = new OdbcConnection(Aria27SystemFilesConnectionString);
            aria27Conn.Open();

            OdbcCommand command = new OdbcCommand("SELECT CconServer, CconDbname, Cconuserid, Cconpaswrd " +
                                                  "FROM Syccomp WHERE Ccomp_id = '" + companyID + "'", aria27Conn);

            DataTable row = new DataTable();
            row.Load(command.ExecuteReader());

            string result = "";
            if (row.Rows[0]["Cconuserid"].ToString().Trim().CompareTo("") == 0)
            {
                result = _SQLWindowsAuthenticatedConnectionStringTemplate;
            }
            else
            {
                result = _SQLAuthenticatedConnectionStringTemplate;
            }

            result = result.Replace("<Server>", row.Rows[0]["CconServer"].ToString().Trim())
                           .Replace("<DBName>", row.Rows[0]["CconDbname"].ToString().Trim())
                           .Replace("<UserID>", row.Rows[0]["CconUserID"].ToString().Trim())
                           .Replace("<Password>", row.Rows[0]["CConpaswrd"].ToString().Trim());

            aria27Conn.Close();

            _cached04DataConnectionString.Add(companyID, result);

            return result;
        }

        public string GetAria04CompanyDataConnectionStringODBC(string companyID)
        {
            if (_cached04DataConnectionString.ContainsKey(companyID)) return _cached04DataConnectionString[companyID];

            OdbcConnection aria27Conn = new OdbcConnection(Aria27SystemFilesConnectionString);
            aria27Conn.Open();

            OdbcCommand command = new OdbcCommand("SELECT CconServer, CconDbname, Cconuserid, Cconpaswrd " +
                                                  "FROM Syccomp WHERE Ccomp_id = '" + companyID + "'", aria27Conn);

            DataTable row = new DataTable();
            row.Load(command.ExecuteReader());

            string result = _SQLConnectionStringTemplate;

            result = result.Replace("<Server>", row.Rows[0]["CconServer"].ToString().Trim())
                           .Replace("<DBName>", row.Rows[0]["CconDbname"].ToString().Trim())
                           .Replace("<UserID>", row.Rows[0]["CconUserID"].ToString().Trim())
                           .Replace("<Password>", row.Rows[0]["CConpaswrd"].ToString().Trim());

            aria27Conn.Close();

            _cached04DataConnectionString.Add(companyID, result);

            return result;
        }

        public string GetConnectionString(string companyName, AriaDatabaseTypes databaseType)
        {
            switch (databaseType)
            {
                case AriaDatabaseTypes.Aria27Data:
                    return GetAria27CompanyDataConnectionString(companyName);
                    break;

                case AriaDatabaseTypes.Aria27SystemFiles:
                    return Aria27SystemFilesConnectionString;
                    break;

                case AriaDatabaseTypes.Aria40Data:
                    return GetAria04CompanyDataConnectionString(companyName);
                    break;

                case AriaDatabaseTypes.Aria40SystemFiles:
                    return Aria40SystemFilesConnectionString;
                    break;

                case AriaDatabaseTypes.Aria50SystemFiles:
                    return Aria50SystemFilesConnectionString;
                    break;

                case AriaDatabaseTypes.Aria50ClientSystemFiles:
                    return Aria50ClientSystemFilesConnectionString;
                    break;

                case AriaDatabaseTypes.AriaOpenRowSet:
                    return AriaOpenRowSetConnectionString;
                    break;
                
                default:
                    return "";
            }
        }

        public string ResolveMappedDrive(string path)
        {
            OdbcConnection aria27Conn = new OdbcConnection(_aria27SystemFilesConnectionString);
            aria27Conn.Open();

            OdbcCommand sycinstCommand = new OdbcCommand("SELECT cinsysfdr from sycinst", aria27Conn);

            DataTable sycinst = new DataTable();
            sycinst.Load(sycinstCommand.ExecuteReader());

            string aria27SysFilePath = sycinst.Rows[0]["cinsysfdr"].ToString().Trim();
            if(!aria27SysFilePath.EndsWith(@"\")) aria27SysFilePath += @"\";
            
            string aria27SysFileUNCPath = _aria27SystemFilesPath.Trim();
            if (!aria27SysFileUNCPath.EndsWith(@"\")) aria27SysFileUNCPath += @"\";

            if (path.ToUpper().Trim().StartsWith(sycinst.Rows[0]["cinsysfdr"].ToString().Trim().Substring(0, 3).ToUpper()))
            {
                string x = aria27SysFileUNCPath.Substring(0, aria27SysFileUNCPath.Length - aria27SysFilePath.Length + 3);

                return path.Replace(sycinst.Rows[0]["cinsysfdr"].ToString().Trim().Substring(0, 3).ToUpper(), x)
                           .Replace(sycinst.Rows[0]["cinsysfdr"].ToString().Trim().Substring(0, 3).ToLower(), x);
            }
            else
            {
                return path;
            }
        }

        public string GetAbsolutePath(string path, bool zeroTerminal)
        {
            string result = "";

            if (zeroTerminal)
            {
                for (int i = 0; i < path.Length; i++)
                {
                    if (Convert.ToByte(Convert.ToChar(path.Substring(i, 1))) != 0)
                    {
                        result += Convert.ToChar(path.Substring(i, 1)).ToString();
                    }
                    else
                    {
                        break;
                    }
                }
            }
            else
            {
                result = path;
            }

            string originalPath = result.TrimEnd();

            try
            {
                StringBuilder sb = new StringBuilder(512);
                int size = sb.Capacity;

                // look for the {LETTER}: combination ...
                if (originalPath.Length > 2 && originalPath[1] == ':')
                {
                    // don't use char.IsLetter here - as that can be misleading
                    // the only valid drive letters are a-z && A-Z.
                    char c = originalPath[0];
                    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
                    {
                        int error = WNetGetConnection(originalPath.Substring(0, 2), sb, ref size);
                        
                        if (error == 0)
                        {
                            DirectoryInfo dir = new DirectoryInfo(originalPath);

                            string rsultPath = Path.GetFullPath(originalPath).Substring(Path.GetPathRoot(originalPath).Length);
                            return Path.Combine(sb.ToString().TrimEnd(), rsultPath);
                        }
                    }
                }

                return originalPath + (zeroTerminal ? '\0' : ' ').ToString();
            }
            catch (Exception)
            {
                return originalPath;
            }
        }

        // This method leaved becasue it's refrenced by current code
        public void ConnectionsRefresh()
        {
        }

        public void SaveSettings()
        {
            XmlDocument xmlDocument = new XmlDocument();
            
            xmlDocument.Load(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));

            XmlElement documentElement = xmlDocument.DocumentElement;

            for (int index = 0; index < documentElement.ChildNodes.Count; index++)
            {
                if (documentElement.ChildNodes[index].Name == "DatabaseSetup")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "ServerName")
                        {
                            xmlNode.InnerText = _dbServerName;
                        }
                        else if (xmlNode.Name == "ServerLoginType")
                        {
                            xmlNode.InnerText = _dbServerLoginType.ToString();
                        }
                        else if (xmlNode.Name == "UserName")
                        {
                            xmlNode.InnerText = _dbServerUserName;
                        }
                        else if (xmlNode.Name == "Password")
                        {
                            xmlNode.InnerText = _dbServerPassword;
                        }
                    }
                }

                if (documentElement.ChildNodes[index].Name == "OpenRowSetServer")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "ServerName")
                        {
                            xmlNode.InnerText = _openRowSetDbServerName;
                        }
                        else if (xmlNode.Name == "ServerLoginType")
                        {
                            xmlNode.InnerText = _openRowSetDbServerLoginType.ToString();
                        }
                        else if (xmlNode.Name == "UserName")
                        {
                            xmlNode.InnerText = _openRowSetDbServerUserName;
                        }
                        else if (xmlNode.Name == "Password")
                        {
                            xmlNode.InnerText = _openRowSetDbServerPassword;
                        }
                    }
                }

                if (documentElement.ChildNodes[index].Name == "FileServer")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "Aria40SharedPath")
                        {
                            xmlNode.InnerText = _aria40SharedPath;
                        }
                    }
                }

                if (documentElement.ChildNodes[index].Name == "RequestServer")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "MaxRecordsPerAgent")
                        {
                            xmlNode.InnerText = _ariaMaxRecordsPerAgent.ToString();
                        }
                    }
                }
            }

            xmlDocument.Save(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));
        }

        public bool IsValidOpenRowSetServer(string connection)
        {
            string serverName = connection.Split(';')[0];
            string uid = connection.Split(';')[1];
            string pwd = connection.Split(';')[2];
            try
            {
                string constring = @"Data Source=<Server>;Initial Catalog=<DBName>;User ID=<UserID>;Password=<Password>;Trusted_Connection=no".Replace("<Server>", serverName)
                                                      .Replace("<DBName>", "master")
                                                      .Replace("<UserID>", uid)
                                                      .Replace("<Password>", pwd);
                SqlConnection con = new SqlConnection(constring.TrimEnd());
                con.Open();

                SqlCommand com = new SqlCommand("SELECT SERVERPROPERTY('edition')", con);

                string result = com.ExecuteScalar().ToString();

                return !result.Contains("64");
            }
            catch (Exception ex)
            {
                return false;
            }
        }

        public void ExecuteSystemMasterCommand(string sqlCommand)
        {
            // calling from installshield put terminal zero in the end if the string

            string result = "";

            result = "";
            for (int i = 0; i < sqlCommand.Length; i++)
            {
                if (Convert.ToByte(Convert.ToChar(sqlCommand.Substring(i, 1))) != 0)
                {
                    result += Convert.ToChar(sqlCommand.Substring(i, 1)).ToString();
                }
                else
                {
                    break;
                }
            }
            sqlCommand = result;

            SqlConnection sqlConnection;
            sqlConnection = new SqlConnection(_aria50SystemFilesConnectionString);

            sqlConnection.Open();

            DataTable table = new DataTable();

            SqlCommand cmd = new SqlCommand(sqlCommand, sqlConnection);
            cmd.ExecuteNonQuery();
        }

        public void CreateClientSettings(string ClientId)
        {
            SqlConnection sqlConnection;
            sqlConnection = new SqlConnection(_aria50SystemFilesConnectionString);

            sqlConnection.Open();

            DataTable table = new DataTable();

            SqlCommand cmd = new SqlCommand("SELECT CCLIENTID, ARIA40SYS, REQSERVER FROM CLIENTS WHERE CCLIENTID = '" + ClientId + "'", sqlConnection);
            table.Load(cmd.ExecuteReader());

            if(table.Rows.Count > 0)
            {
                string xml = String.Format("<?xml version=\"1.0\" standalone=\"yes\"?>" +
                                            "<DocumentElement>" +
                                            "<ClientConfig>" +
                                            "<ClientID>{0}</ClientID>" +
                                            "<RemoteSrv>{1}</RemoteSrv>" +
                                            "<RemotePort>{2}</RemotePort>" + 
                                            "</ClientConfig>" +
                                            "</DocumentElement>", ClientId.Trim(), table.Rows[0]["REQSERVER"].ToString().Trim(), "1500");

                File.WriteAllText(Path.Combine(table.Rows[0]["ARIA40SYS"].ToString(), "Client_Setting.xml"), xml);
            }
        }
            
        public bool CanReadFromLocation(string path)
        {
            // calling from installshield put terminal zero in the end if the string
            string result = "";
            for (int i = 0; i < path.Length; i++)
            {
                if (Convert.ToByte(Convert.ToChar(path.Substring(i, 1))) != 0)
                {
                    result += Convert.ToChar(path.Substring(i, 1)).ToString();
                }
                else
                {
                    break;
                }
            }

            try
            {
                Directory.GetFiles(result);

                return true;
            }
            catch (Exception)
            {
                return false;
            }
        }

        public bool CanWriteToLocation(string path)
        {
            // calling from installshield put terminal zero in the end if the string
            string result = "";
            for (int i = 0; i < path.Length; i++)
            {
                if (Convert.ToByte(Convert.ToChar(path.Substring(i, 1))) != 0)
                {
                    result += Convert.ToChar(path.Substring(i, 1)).ToString();
                }
                else
                {
                    break;
                }
            }

            try
            {
                File.Create(Path.Combine(result, Path.GetRandomFileName()));

                return true;
            }
            catch (Exception)
            {
                return false;
            }
        }

        public bool OpenRowSetSqlServerCanReadFromLocation(string path)
        {
            SqlConnection sqlConnection;
            if (OpenRowSetDbServerLoginType == DatabaseServerLoginTypes.SqlServerAuthentication)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + OpenRowSetDbServerName + @";Initial Catalog=Master;User ID=" + OpenRowSetDbServerUserName + @";Password=" + OpenRowSetDbServerPassword + @";Trusted_Connection=no");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + OpenRowSetDbServerName + @";Initial Catalog=Master;Integrated Security=true");
            }

            sqlConnection.Open();

            DataTable table = new DataTable();

            SqlCommand cmd = new SqlCommand("master..xp_FILEEXIST", sqlConnection);
            cmd.CommandType = CommandType.StoredProcedure;
            cmd.Parameters.Add(new SqlParameter("File", path));
            DataTable result = new DataTable();
            result.Load(cmd.ExecuteReader());

            return result.Rows[0]["Parent Directory Exists"].ToString().Trim() == "1" &&
                   result.Rows[0]["File is a Directory"].ToString().Trim() == "1";
        }

        public override object InitializeLifetimeService()
        {
            return null;
        }
    }
}


//using System.Xml;
//using System;
//using System.Data.Odbc;
//using System.Collections;
//using System.Collections.Generic;
//using System.Data;
//using System.Data.SqlClient;
////[START]Ahmed Maher -Date: 2-12-2009 -Aria27Date may be X:\
//using System.Runtime.InteropServices;
//using System.Text;
//using System.IO;
////[END]


//namespace Aria.Environment
//{
//    /// <summary>
//    /// all information related configuration setting from XML file where this information related to database name and username and password, FoxPro paths, services name …. Etc.
//    /// </summary>
//    public class AriaEnviromentVariables
//    {
//        //[START]Ahmed Maher -Date: 2-12-2009 -Aria27Date may be X:\
//        [DllImport("mpr.dll", CharSet = CharSet.Unicode, SetLastError = true)]
//        public static extern int WNetGetConnection(
//            [MarshalAs(UnmanagedType.LPTStr)] string localName,
//            [MarshalAs(UnmanagedType.LPTStr)] StringBuilder remoteName,
//            ref int length);

//        /// <summary>
//        /// Used to get real path from mapped path.  
//        /// </summary>
//        /// <example>
//        /// <code>
//        /// AriaEnviromentVariables.GetUNCPath("X:\Aria27");
//        ///  -->Output :\\Aria-Maher\Aria27
//        /// </code>
//        /// </example>
//        /// <param name="originalPath">Mapped path</param>
//        /// <returns>return real path as string datatype</returns>
//        public static string GetUNCPath(string originalPath)
//        {
//            StringBuilder sb = new StringBuilder(512);
//            int size = sb.Capacity;

//            // look1 for the {LETTER}: combination ...
//            if (originalPath.Length > 2 && originalPath[1] == ':')
//            {
//                // don't use char.IsLetter here - as that can be misleading
//                // the only valid drive letters are a-z && A-Z.
//                char c = originalPath[0];
//                if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
//                {
//                    int error = WNetGetConnection(originalPath.Substring(0, 2),
//                        sb, ref size);
//                    if (error == 0)
//                    {
//                        //DirectoryInfo dir = new DirectoryInfo(originalPath);

//                        string path = Path.GetFullPath(originalPath)
//                            .Substring(Path.GetPathRoot(originalPath).Length);
//                        return Path.Combine(sb.ToString().TrimEnd(), path);

//                    }

//                }
//            }

//            return originalPath;
//        }
//        public static string GetUNCPath2(string originalPath, out int errorout)
//        {
//            StringBuilder sb = new StringBuilder(512);
//            int size = sb.Capacity;
//            errorout = 0;
//            // look1 for the {LETTER}: combination ...
//            if (originalPath.Length > 2 && originalPath[1] == ':')
//            {
//                // don't use char.IsLetter here - as that can be misleading
//                // the only valid drive letters are a-z && A-Z.
//                char c = originalPath[0];
//                if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
//                {
//                    int error = WNetGetConnection(originalPath.Substring(0, 2),
//                        sb, ref size);
//                    errorout = error;
//                    if (error == 0)
//                    {
//                        //DirectoryInfo dir = new DirectoryInfo(originalPath);

//                        string path = Path.GetFullPath(originalPath)
//                            .Substring(Path.GetPathRoot(originalPath).Length);
//                        return Path.Combine(sb.ToString().TrimEnd(), path);

//                    }

//                }
//            }

//            return originalPath;
//        }
//        //[END]

//        private Dictionary<string, string> _cached27DataConnectionString = new Dictionary<string, string>();
//        private Dictionary<string, string> _cached04DataConnectionString = new Dictionary<string, string>();
//        private int _smtpPort = 0;

//        private string _smtpServer = "";

//        private string _smtpUserName = "";

//        private string _smtpUserPassword = "";

//        private string _senderAddress = "";

//        private string _senderName = "";

//        //[START]Ahmed Maher -Date: 2-12-2009 -SSL Fix
//        private bool _ssl = true;

//        public bool Ssl
//        {
//            get { return _ssl; }
//            set { _ssl = value; }
//        }
//        //[END]
//        //[START]Ahmed Maher -Date: 23-03-2010 -To enable Custome Connection
//        private bool _customConnection = false;
//        /// <summary>
//        /// Get/Set Custom Connection where its enable us from add custom connection in configuration settings
//        /// and use it Rather than Generated Connection
//        /// </summary>
//        public bool CustomConnection
//        {
//            get { return _customConnection; }
//            set { _customConnection = value; }
//        }

//        private string _customAria50SystemFilesConnectionString = "";
//        /// <summary>
//        /// Get/Set Connection string of System.Master DB.
//        /// <c>  Note:Used by code only if CustomConnection is equal True.</c>
//        /// </summary>
//        public string CustomAria50SystemFilesConnectionString
//        {
//            get { return _customAria50SystemFilesConnectionString; }
//            set { _customAria50SystemFilesConnectionString = value; }
//        }

//        private string _customAria50ClientSystemFilesConnectionString = "";
//        /// <summary>
//        /// Get/Set Connection string of Aria.Master or "Client".Master DB.
//        /// <c>  Note:Used by code only if CustomConnection is equal True.</c>
//        /// </summary>
//        public string CustomAria50ClientSystemFilesConnectionString
//        {
//            get { return _customAria50ClientSystemFilesConnectionString; }
//            set { _customAria50ClientSystemFilesConnectionString = value; }
//        }
//        //[END]

//        private string _aria27SystemFilesPath = "";
//        public string Aria27SystemFilesPath
//        {
//            get { return _aria27SystemFilesPath; }
//            set { _aria27SystemFilesPath = value; }
//        }

//        private string _aria40SystemFilesPath = "";
//        public string Aria40SystemFilesPath
//        {
//            get { return _aria40SystemFilesPath; }
//            set { _aria40SystemFilesPath = value; }
//        }

//        private string _serverName = "";

//        private DatabaseServerLoginTypes _dbServerLoginType;

//        private string _dbServerUserName = "";

//        private string _dbServerPassword = "";

//        private string _clientID = "";
//        /// <summary>
//        /// Get/Set Clinet ID
//        /// </summary>
//        public string ClientID
//        {
//            get { return _clientID; }
//            set { _clientID = value; }
//        }

//        //[Start]Ahmed Maher -Date: 23/03/2010 -Aria Email
//        /// <summary>
//        /// Used to get Client id from XML sotred in X:\
//        /// </summary>
//        /// <returns>Returns Client id</returns>
//        public string GetClientID()
//        {
//            string XMLPath = GetUNCPath("X:\\");
//            XMLPath = XMLPath + "\\Client_Setting.XML";
//            DataSet DS = new DataSet();
//            DS.ReadXml(XMLPath);
//            if (DS.Tables[0].Rows.Count > 0)
//                return DS.Tables[0].Rows[0]["ClientID"].ToString();
//            return "";
//        }
//        private string _clientDatabaseConnectionString = "";
//        /// <summary>
//        /// Get Connection String specified to Client Maste DB <c>SQL connection</c>.
//        /// </summary>
//        public string ClientDatabaseConnectionString
//        {
//            get
//            {
//                if (_clientDatabaseConnectionString.Length <= 0)
//                {
//                    AriaClientDBConnection("SQL");
//                }
//                return _clientDatabaseConnectionString;
//            }
//        }

//        /// <summary>
//        /// Get Connection String specified to Client Maste DB <c>ODBC connection</c>.
//        /// </summary>
//        public string ClientDatabaseConnectionStringODBC
//        {
//            get
//            {
//                if (_clientDatabaseConnectionString.Length <= 0)
//                {
//                    AriaClientDBConnection("ODBC");
//                }
//                return _clientDatabaseConnectionString;
//            }
//        }

//        /// <summary>
//        /// Used to generate Connection string of Client DB stord in client table in System.Master
//        /// this connection can used to connect by ODBC or SQL Depend on type.
//        /// </summary>
//        /// <param name="ConnectionType">if connection type is SQL the result of connection related to .NET connection else if equal ODBC to enable from connect as ODBC.</param>
//        private void AriaClientDBConnection(string ConnectionType)
//        {
//            # region Get The Client ID
//            // T20100512.0026 Hassan.I 20-05-2010 [Begin]
//            string ClientID = "";
//            if (string.IsNullOrEmpty(this.ClientID) == true)
//            { ClientID = GetClientID(); }
//            else
//            { ClientID = this.ClientID.ToString().TrimEnd(); }
//            // T20100512.0026 Hassan.I 20-05-2010 [End]
//            #endregion

//            string ClientConn = "";
//            SqlConnection Conn = new SqlConnection(Aria50SystemFilesConnectionString);
//            Conn.Open();

//            SqlCommand Cmd = new SqlCommand("select * from CLIENTS where CCLIENTID='" + ClientID + "'", Conn);
//            DataTable Dt = new DataTable();
//            Dt.Load(Cmd.ExecuteReader());

//            // T20100512.0026 Hassan.I 20-05-2010 [Begin]

//            //if (ConnectionType == "SQL")
//            //    ClientConn = @"Data Source=" + Dt.Rows[0]["CCONSERVER"].ToString() + @";Initial Catalog=" + Dt.Rows[0]["CCONDBNAME"].ToString() + @";User ID=" + Dt.Rows[0]["CCONUSERID"].ToString() + @";Password=" + Dt.Rows[0]["CCONPASWRD"].ToString() + @";";
//            //if (ConnectionType == "ODBC")
//            //    ClientConn = "Driver={SQL Server};server=" + Dt.Rows[0]["CCONSERVER"].ToString() + ";DATABASE=" + Dt.Rows[0]["CCONDBNAME"].ToString() + @";UID=" + Dt.Rows[0]["CCONUSERID"].ToString() + @";PWD=" + Dt.Rows[0]["CCONPASWRD"].ToString();
//            //_clientDatabaseConnectionString = ClientConn;

//            if (ConnectionType == "SQL")
//            {
//                if (_dbServerLoginType == DatabaseServerLoginTypes.WindowAuthentication && string.IsNullOrEmpty(Dt.Rows[0]["CCONPASWRD"].ToString()) == true && string.IsNullOrEmpty(Dt.Rows[0]["CCONUSERID"].ToString()) == true)
//                { ClientConn = @"Data Source=" + Dt.Rows[0]["CCONSERVER"].ToString() + @";Initial Catalog=" + Dt.Rows[0]["CCONDBNAME"].ToString() + @";Integrated Security=True;"; }
//                else
//                { ClientConn = @"Data Source=" + Dt.Rows[0]["CCONSERVER"].ToString() + @";Initial Catalog=" + Dt.Rows[0]["CCONDBNAME"].ToString() + @";User ID=" + Dt.Rows[0]["CCONUSERID"].ToString() + @";Password=" + Dt.Rows[0]["CCONPASWRD"].ToString() + @";"; }
//            };

//            if (ConnectionType == "ODBC")
//            {
//                if (_dbServerLoginType == DatabaseServerLoginTypes.WindowAuthentication && string.IsNullOrEmpty(Dt.Rows[0]["CCONPASWRD"].ToString()) == true && string.IsNullOrEmpty(Dt.Rows[0]["CCONUSERID"].ToString()) == true)
//                { ClientConn = "Driver={SQL Server};server=" + Dt.Rows[0]["CCONSERVER"].ToString() + ";DATABASE=" + Dt.Rows[0]["CCONDBNAME"].ToString(); }
//                else
//                { ClientConn = "Driver={SQL Server};server=" + Dt.Rows[0]["CCONSERVER"].ToString() + ";DATABASE=" + Dt.Rows[0]["CCONDBNAME"].ToString() + @";UID=" + Dt.Rows[0]["CCONUSERID"].ToString() + @";PWD=" + Dt.Rows[0]["CCONPASWRD"].ToString(); }
//            };

//            _clientDatabaseConnectionString = ClientConn;

//            Conn.Close();
//            // T20100512.0026 Hassan.I 20-05-2010 [End]
//        }
//        //[END]

//        private void Init()
//        {
//            XmlDocument xmlDocument = new XmlDocument();

//            xmlDocument.Load(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));

//            XmlElement documentElement = xmlDocument.DocumentElement;

//            for (int index = 0; index < documentElement.ChildNodes.Count; index++)
//            {
//                if (documentElement.ChildNodes[index].Name == "EmailSettings")
//                {
//                    XmlNode xmlNode = null;

//                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
//                    {
//                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

//                        if (xmlNode.Name == "SmtpServer")
//                        {
//                            _smtpServer = xmlNode.InnerText;
//                        }
//                        else if (xmlNode.Name == "SmtpPort")
//                        {
//                            _smtpPort = int.Parse(xmlNode.InnerText);
//                        }
//                        else if (xmlNode.Name == "SmtpUserName")
//                        {
//                            _smtpUserName = xmlNode.InnerText;
//                        }
//                        else if (xmlNode.Name == "SmtpUserPassword")
//                        {
//                            _smtpUserPassword = xmlNode.InnerText;
//                        }
//                        else if (xmlNode.Name == "SenderAddress")
//                        {
//                            _senderAddress = xmlNode.InnerText;
//                        }
//                        else if (xmlNode.Name == "SenderName")
//                        {
//                            _senderName = xmlNode.InnerText;
//                        }
//                        //[START]Ahmed Maher -Date: 2-12-2009 -SSL Fix
//                        else if (xmlNode.Name == "SSL")
//                        {
//                            _ssl = Convert.ToBoolean(xmlNode.InnerText);
//                        }
//                        //[END]
//                    }
//                }
//                else if (documentElement.ChildNodes[index].Name == "DatabaseSetup")
//                {
//                    XmlNode xmlNode = null;

//                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
//                    {
//                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

//                        if (xmlNode.Name == "Aria27SystemFilesPath")
//                        {
//                            _aria27SystemFilesPath = xmlNode.InnerText;
//                        }
//                        else if (xmlNode.Name == "Aria40SystemFilesPath")
//                        {
//                            _aria40SystemFilesPath = xmlNode.InnerText;
//                        }
//                        else if (xmlNode.Name == "ServerName")
//                        {
//                            _serverName = xmlNode.InnerText;
//                        }
//                        else if (xmlNode.Name == "ServerLoginType")
//                        {
//                            _dbServerLoginType = (DatabaseServerLoginTypes)Enum.Parse(typeof(DatabaseServerLoginTypes), xmlNode.InnerText);
//                        }
//                        else if (xmlNode.Name == "UserName")
//                        {
//                            _dbServerUserName = xmlNode.InnerText;
//                        }
//                        else if (xmlNode.Name == "Password")
//                        {
//                            _dbServerPassword = xmlNode.InnerText;
//                        }
//                        //[START]Ahmed Maher -Date: 23-03-2010 -To enable Custome Connection
//                        else if (xmlNode.Name == "CustomConnection")
//                        {
//                            if (string.IsNullOrEmpty(xmlNode.InnerText))
//                                _customConnection = false;
//                            if (xmlNode.InnerText.ToLower() == "true" || xmlNode.InnerText.ToLower() == "false")
//                                _customConnection = Convert.ToBoolean(xmlNode.InnerText);
//                            else
//                                _customConnection = false;
//                        }
//                        else if (xmlNode.Name == "Aria50SystemFilesConnectionString")
//                        {
//                            _customAria50SystemFilesConnectionString = xmlNode.InnerText;
//                        }
//                        else if (xmlNode.Name == "Aria50ClientSystemFilesConnectionString")
//                        {
//                            _customAria50ClientSystemFilesConnectionString = xmlNode.InnerText;
//                        }
//                        //[END]
//                    }
//                }
//            }
//        }

//        public string GetAria27CompanyDataConnectionString(string companyID)
//        {

//            if (_cached27DataConnectionString.ContainsKey(companyID)) // checking if aleady cached
//                return _cached27DataConnectionString[companyID];

//            OdbcCommand command = new OdbcCommand();
//            command.CommandText = "SELECT Ccom_ddir FROM Syccomp WHERE Ccomp_id = '" + companyID + "'";
//            command.Connection = new OdbcConnection();
//            command.Connection.ConnectionString = _aria27SystemFilesConnectionString;
//            command.Connection.Open();
//            OdbcDataAdapter dataAdapter = new OdbcDataAdapter(command);
//            DataTable row = new DataTable();
//            dataAdapter.Fill(row);

//            #region SABER 07-15-2011
//            // SABER 07-15-2011 [Start]
//            ////[START]Ahmed Maher -Date: 2-12-2009 -Aria27Date may be X:\
//            //int errorout = 0;
//            //string RealPath = GetUNCPath2(row.Rows[0]["Ccom_ddir"].ToString(), out errorout);
//            //// T20100512.0026 Hassan 2010 05 23 [Begin]
//            //if (errorout != 0)
//            //{
//            //    string cutstr = Aria27SystemFilesPath;
//            //    cutstr = cutstr.Trim().ToUpper();
//            //    cutstr = cutstr.Replace("ARIA27\\SYSFILES\\", "");
//            //    RealPath = Path.Combine(cutstr, row.Rows[0]["Ccom_ddir"].ToString().Substring(3));
//            //};
//            //// T20100512.0026 Hassan 2010 05 23 [End]
            
//            ////string res = _aria27DataConnectionString.Replace("<DataBaseLocation>", row.Rows[0]["Ccom_ddir"].ToString());
//            //string res = _aria27DataConnectionString.Replace("<DataBaseLocation>", RealPath);
//            ////[END]

//            string RealPath = "";
//            RealPath = row.Rows[0]["Ccom_ddir"].ToString();
//            if (RealPath.Length > 2 && RealPath[1] == ':')
//            {                
//                int errorout = 0;            
//                RealPath = GetUNCPath2(RealPath, out errorout);                
//                if (errorout != 0)
//                {
//                    SqlConnection Conn = new SqlConnection(Aria50SystemFilesConnectionString);
//                    Conn.Open();
//                    SqlCommand Cmd = new SqlCommand("SELECT * from CLIENTS WHERE CCLIENTID='" + ClientID + "'", Conn);
//                    DataTable Dt = new DataTable();
//                    Dt.Load(Cmd.ExecuteReader());
//                    string cutstr = Dt.Rows[0]["CDATAPATH"].ToString().TrimEnd();
//                    cutstr = cutstr.Trim().ToUpper();                    
//                    RealPath = Path.Combine(cutstr, row.Rows[0]["Ccom_ddir"].ToString().Substring(3));
//                    Conn.Close();
//                }
//            }
//            // SABER 07-15-2011 [End]
//            #endregion

//            #region Modified Code By SABER 07-05-2011
//            //string RealPath;
//            //RealPath = row.Rows[0]["Ccom_ddir"].ToString();
//            //if (RealPath.Length > 2 && RealPath[1] == ':')
//            //{
//            //    DriveInfo drv = new DriveInfo(RealPath.Substring(0, 2));
//            //    if (drv.DriveType == DriveType.Network)
//            //    {
//            //        int errorout = 0;
//            //        RealPath = GetUNCPath2(RealPath, out errorout);
//            //        if (errorout != 0)
//            //        {
//            //            string cutstr = Aria27SystemFilesPath;
//            //            cutstr = cutstr.Trim().ToUpper();
//            //            cutstr = cutstr.Replace("ARIA27\\SYSFILES\\", "");
//            //            RealPath = Path.Combine(cutstr, row.Rows[0]["Ccom_ddir"].ToString().Substring(3));
//            //        };
//            //    }
//            //}
//            #endregion

//            string res = _aria27DataConnectionString.Replace("<DataBaseLocation>", RealPath);
//            _cached27DataConnectionString.Add(companyID, res); //caching 

//            //T20100512.0026 Hassan 2010 05 23 [Begin]
//            if (string.IsNullOrEmpty(ClientID) == false)
//            {
//                command.Connection.Close();
//                //MOH T20110103.0020 - Osprey Aria Upgrade - Cannot email documents from any screen [Start]
//                //command.CommandText = "SELECT Cfld_name, mdata_def FROM Setups WHERE cFld_Name = 'M_SMTPSRVR' or cFld_Name = 'M_SMTPPORT' or cFld_Name = 'M_SMTPUSER' or cFld_Name = 'M_SMTPPSS ' or cFld_Name = 'M_SNDRNME ' or cFld_Name = 'M_SSL     '";
//                command.CommandText = "SELECT Cfld_name, mdata_def FROM Setups WHERE cFld_Name in('M_SMTPSRVR','M_SMTPPORT','M_SMTPUSER','M_SMTPPSS ', 'M_SNDRNME ' ,'M_SSL     ','M_SMTPUSR2')";
//                //MOH T20110103.0020 - Osprey Aria Upgrade - Cannot email documents from any screen [End]
//                command.Connection = new OdbcConnection();
//                command.Connection.ConnectionString = res;
//                row.Rows.Clear();
//                dataAdapter.Fill(row);
//                if (row.Rows.Count == 0) throw new Exception("Couldn't Find Aria4 Email settings for company " + companyID);

//                _smtpServer = row.Select("cFld_Name = 'M_SMTPSRVR'")[0]["mdata_def"].ToString();
//                if (_smtpServer.Trim() == "") throw new Exception("Couldn't Find Aria4 Email settings (SMTP Server) for company " + companyID);
//                _ariaSMTPHost = _smtpServer;

//                string port = row.Select("cFld_Name = 'M_SMTPPORT'")[0]["mdata_def"].ToString();
//                if (port.Trim() == "") throw new Exception("Couldn't Find Aria4 Email settings (SMTP Port) for company " + companyID);
//                _smtpPort = int.Parse(row.Select("cFld_Name = 'M_SMTPPORT'")[0]["mdata_def"].ToString());
//                _ariaSMTPPort = _smtpPort;

//                _smtpUserName = row.Select("cFld_Name = 'M_SMTPUSER'")[0]["mdata_def"].ToString();
//                if (_smtpUserName.Trim() == "") throw new Exception("Couldn't Find Aria4 Email settings (SMTP UserName) for company " + companyID);
//                _ariaSMTPUserName = _smtpUserName;

//                _smtpUserPassword = row.Select("cFld_Name = 'M_SMTPPSS '")[0]["mdata_def"].ToString();
//                if (_smtpUserPassword.Trim() == "") throw new Exception("Couldn't Find Aria4 Email settings (SMTP Password) for company " + companyID);
//                _ariaSMTPPassword = _smtpUserPassword;

//                _senderName = row.Select("cFld_Name = 'M_SNDRNME '")[0]["mdata_def"].ToString();
//                _ariaSenderName = _senderName;

//                //MOH T20110103.0020 - Osprey Aria Upgrade - Cannot email documents from any screen [Start]
//                _senderAddress = row.Select("cFld_Name = 'M_SMTPUSR2'")[0]["mdata_def"].ToString();
//                _ariaSenderMail = _senderAddress;
//                //MOH T20110103.0020 - Osprey Aria Upgrade - Cannot email documents from any screen [End]

//                _ssl = false;
//                if ((row.Select("cFld_Name = 'M_SSL     '")[0]["mdata_def"].ToString().ToUpper().TrimEnd() == ".T.") || (row.Select("cFld_Name = 'M_SSL     '")[0]["mdata_def"].ToString().ToUpper().TrimEnd() == "TRUE"))
//                { _ssl = true; }
//                else
//                {
//                    if ((row.Select("cFld_Name = 'M_SSL     '")[0]["mdata_def"].ToString().ToUpper().TrimEnd() == ".F.") || (row.Select("cFld_Name = 'M_SSL     '")[0]["mdata_def"].ToString().ToUpper().TrimEnd() == "FALSE"))
//                    { _ssl = false; }
//                }
//            }
//            //T20100512.0026 Hassan 2010 05 23 [End]
//            return res;
//        }

//        public string GetAria04CompanyDataConnectionString(string companyID)
//        {
//            //[Modified]Ahmed Maher -Date: 10/03/2010 -tkt#T20100301.0004
//            //return _aria50SystemFilesConnectionString;
//            //[END]
//            if (_cached04DataConnectionString.ContainsKey(companyID)) // checking if cached
//                return _cached04DataConnectionString[companyID];

//            OdbcCommand command = new OdbcCommand();
//            command.CommandText = "SELECT CconServer,CconDbname,Cconuserid,Cconpaswrd FROM Syccomp WHERE Ccomp_id = '" + companyID + "'";
//            command.Connection = new OdbcConnection();
//            command.Connection.ConnectionString = _aria27SystemFilesConnectionString;
//            command.Connection.Open();
//            OdbcDataAdapter dataAdapter = new OdbcDataAdapter(command);
//            DataTable row = new DataTable();
//            dataAdapter.Fill(row);
//            string res = "";
//            if (row.Rows[0]["Cconuserid"].ToString().Trim().CompareTo("") == 0)
//            {
//                res = _aria04WindowsAuthenticatedDataConnectionString.Replace("<Server>", row.Rows[0]["CconServer"].ToString());
//                res = res.Replace("<DBName>", row.Rows[0]["CconDbname"].ToString());

//                _cached04DataConnectionString.Add(companyID, res); //caching
//                return res;
//            }

//            res = _aria04SQLAuthenticatedDataConnectionString.Replace("<Server>", row.Rows[0]["CconServer"].ToString());
//            res = res.Replace("<DBName>", row.Rows[0]["CconDbname"].ToString());
//            res = res.Replace("<UserID>", row.Rows[0]["Cconuserid"].ToString());
//            res = res.Replace("<Password>", row.Rows[0]["Cconpaswrd"].ToString());

//            _cached04DataConnectionString.Add(companyID, res); //caching

//            return res;
//        }

//        public AriaEnviromentVariables()
//        {

//            Init();

//            _ariaSMTPPort = _smtpPort;

//            _ariaSMTPHost = _smtpServer;

//            _ariaSMTPUserName = _smtpUserName;

//            _ariaSMTPPassword = _smtpUserPassword;

//            _ariaSenderMail = _senderAddress;

//            _ariaSenderName = _senderName;

//            if (_dbServerLoginType == DatabaseServerLoginTypes.SqlServerAuthentication)
//            {
//                //[Modified]Ahmed Maher -Date: 22/03/2010 -T20100312.0057 
//                //_aria50SystemFilesConnectionString = @"Data Source=" + _serverName + @";Initial Catalog=System.Master;User ID=" + _dbServerUserName + @";Password=" + _dbServerPassword + @";Trusted_Connection=True";
//                _aria50SystemFilesConnectionString = @"Data Source=" + _serverName + @";Initial Catalog=System.Master;User ID=" + _dbServerUserName + @";Password=" + _dbServerPassword + @";";
//                //[END]
//                _aria50SystemFilesConnectionStringOdbc = "Driver={SQL Server};server=" + this._serverName + ";DATABASE=System.Master;UID=" + this._dbServerUserName + ";PWD=" + this._dbServerPassword;
//                //[Modified]Ahmed Maher -Date: 22/03/2010 -T20100312.0057 
//                //_aria50ClientSystemFilesConnectionString = @"Data Source=" + _serverName + @";Initial Catalog=Aria.Master;User ID=" + _dbServerUserName + @";Password=" + _dbServerPassword + @";Trusted_Connection=True";
//                _aria50ClientSystemFilesConnectionString = @"Data Source=" + _serverName + @";Initial Catalog=Aria.Master;User ID=" + _dbServerUserName + @";Password=" + _dbServerPassword + @";";
//                //[END]
//                _aria50ClientSystemFilesConnectionStringOdbc = "Driver={SQL Server};server=" + _serverName + ";DATABASE=Aria.Master;UID=" + _dbServerUserName + @";PWD=" + _dbServerPassword;
//            }
//            else
//            {
//                _aria50SystemFilesConnectionString = @"Data Source=" + _serverName + @";Initial Catalog=System.Master;Integrated Security=True";
//                _aria50SystemFilesConnectionStringOdbc = "Driver={SQL Server};server=" + this._serverName + ";DATABASE=System.Master";
//                _aria50ClientSystemFilesConnectionString = @"Data Source=" + _serverName + @";Initial Catalog=Aria.Master;Integrated Security=True";
//                _aria50ClientSystemFilesConnectionStringOdbc = "Driver={SQL Server};server=" + _serverName + ";DATABASE=Aria.Master"; ;
//            }

//            //[START]Ahmed Maher -Date: 23-03-2010 -To enable Custome Connection
//            if (CustomConnection)
//            {
//                _aria50SystemFilesConnectionString = _customAria50SystemFilesConnectionString;
//                _aria50ClientSystemFilesConnectionString = _customAria50ClientSystemFilesConnectionString;
//            }
//            //[END]

//            _aria27SystemFilesConnectionString = @"Driver={Microsoft Visual FoxPro Driver};sourcedb=" + _aria27SystemFilesPath + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes";
//            _aria27DataConnectionString = @"Driver={Microsoft Visual FoxPro Driver};sourcedb=<DataBaseLocation>;sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes";

//            _aria40SystemFilesConnectionString = @"Driver={Microsoft Visual FoxPro Driver};sourcedb=" + _aria40SystemFilesPath + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes";

//            _aria04SQLAuthenticatedDataConnectionString = @"Data Source=<Server>;Initial Catalog=<DBName>;User ID=<UserID>;Password=<Password>;Trusted_Connection=no";
//            _aria04WindowsAuthenticatedDataConnectionString = @"Data Source=<Server>;Initial Catalog=<DBName>;Integrated Security=True";
//        }

//        private int _ariaSMTPPort;
//        public int AriaSMTPPort
//        {
//            get { return _ariaSMTPPort; }
//            set { _ariaSMTPPort = value; }
//        }

//        private string _ariaSMTPHost;
//        public string AriaSMTPHost
//        {
//            get { return _ariaSMTPHost; }
//            set { _ariaSMTPHost = value; }
//        }

//        private string _ariaSMTPUserName;
//        public string AriaSMTPUserName
//        {
//            get { return _ariaSMTPUserName; }
//            set { _ariaSMTPUserName = value; }
//        }

//        private string _ariaSMTPPassword;
//        public string AriaSMTPPassword
//        {
//            get { return _ariaSMTPPassword; }
//            set { _ariaSMTPPassword = value; }
//        }

//        private string _aria27DataConnectionString;
//        public string Aria27DataConnectionString
//        {
//            get { return _aria27DataConnectionString; }
//            set { _aria27DataConnectionString = value; }
//        }

//        private string _aria27SystemFilesConnectionString;
//        public string Aria27SystemFilesConnectionString
//        {
//            get { return _aria27SystemFilesConnectionString; }
//            set { _aria27SystemFilesConnectionString = value; }
//        }

//        private string _aria40SystemFilesConnectionString;
//        public string Aria40SystemFilesConnectionString
//        {
//            get { return _aria40SystemFilesConnectionString; }
//            set { _aria40SystemFilesConnectionString = value; }
//        }

//        private string _aria50SystemFilesConnectionString;
//        public string Aria50SystemFilesConnectionString
//        {
//            get { return _aria50SystemFilesConnectionString; }
//            set { _aria50SystemFilesConnectionString = value; }
//        }

//        private string _aria50SystemFilesConnectionStringOdbc;

//        public string Aria50SystemFilesConnectionStringOdbc
//        {
//            get { return _aria50SystemFilesConnectionStringOdbc; }
//            set { _aria50SystemFilesConnectionStringOdbc = value; }
//        }


//        private string _aria50ClientSystemFilesConnectionString;
//        public string Aria50ClientSystemFilesConnectionString
//        {
//            get { return _aria50ClientSystemFilesConnectionString; }
//            set { _aria50ClientSystemFilesConnectionString = value; }
//        }

//        private string _aria50ClientSystemFilesConnectionStringOdbc;
//        public string Aria50ClientSystemFilesConnectionStringOdbc
//        {
//            get { return _aria50ClientSystemFilesConnectionStringOdbc; }
//            set { _aria50ClientSystemFilesConnectionStringOdbc = value; }
//        }

//        private string _ariaSenderMail;
//        public string AriaSenderMail
//        {
//            get { return _ariaSenderMail; }
//            set { _ariaSenderMail = value; }
//        }

//        private string _ariaSenderName;
//        public string AriaSenderName
//        {
//            get { return _ariaSenderName; }
//            set { _ariaSenderName = value; }
//        }

//        private string _aria04SQLAuthenticatedDataConnectionString = "";
//        public string Aria04SQLAuthenticatedDataConnectionString
//        {
//            set { _aria04SQLAuthenticatedDataConnectionString = value; }
//            get { return _aria04SQLAuthenticatedDataConnectionString; }
//        }

//        private string _aria04WindowsAuthenticatedDataConnectionString = "";
//        public string Aria04WindowsAuthenticatedDataConnectionString
//        {
//            set { Aria04WindowsAuthenticatedDataConnectionString = value; }
//            get { return Aria04WindowsAuthenticatedDataConnectionString; }
//        }

//        public string GetConnectionString(string companyName, AriaDatabaseTypes databaseType)
//        {
//            switch (databaseType)
//            {
//                case AriaDatabaseTypes.Aria27Data:
//                    return GetAria27CompanyDataConnectionString(companyName);
//                    break;

//                case AriaDatabaseTypes.Aria27SystemFiles:
//                    return Aria27SystemFilesConnectionString;
//                    break;

//                case AriaDatabaseTypes.Aria40Data:
//                    return GetAria04CompanyDataConnectionString(companyName);
//                    break;

//                case AriaDatabaseTypes.Aria40SystemFiles:
//                    return Aria40SystemFilesConnectionString;
//                    break;

//                case AriaDatabaseTypes.Aria50SystemFiles:
//                    return Aria50SystemFilesConnectionString;
//                    break;

//                case AriaDatabaseTypes.Aria50ClientSystemFiles:
//                    return Aria50ClientSystemFilesConnectionString;
//                    break;
//            }

//            return "";
//        }
//        /// <summary>
//        /// Refresh pathes with W.R.T Client ID
//        /// T20100512.0026 Hassan 2010 05 23 [Begin]
//        /// </summary>
//        public void ConnectionsRefresh()
//        {

//            if (string.IsNullOrEmpty(ClientID) == false)
//            {

//                # region Fill connections to Client.master DB, from clients file from system master
//                //_aria50ClientSystemFilesConnectionString      && Cleint Master Connection string 
//                //_aria50ClientSystemFilesConnectionStringOdbc  && Cleint Master odbc Connection string 

//                AriaClientDBConnection("SQL");
//                _aria50ClientSystemFilesConnectionString = _clientDatabaseConnectionString;

//                AriaClientDBConnection("ODBC");
//                _aria50ClientSystemFilesConnectionStringOdbc = _clientDatabaseConnectionString;
//                # endregion


//                string ClientConn = "";
//                SqlConnection Conn = new SqlConnection(Aria50SystemFilesConnectionString);
//                Conn.Open();

//                SqlCommand Cmd = new SqlCommand("select * from CLIENTS where CCLIENTID='" + ClientID + "'", Conn);
//                DataTable Dt = new DataTable();
//                Dt.Load(Cmd.ExecuteReader());

//                //_aria27SystemFilesPath = Dt.Rows[0]["CDATAPATH"].ToString().TrimEnd()  + @"ARIA27\SYSFILES\";
//                _aria27SystemFilesPath = Dt.Rows[0]["ARIA27SYS"].ToString().TrimEnd();

//                //_aria40SystemFilesPath = Dt.Rows[0]["CDATAPATH"].ToString().TrimEnd() + @"ARIA4XP\SQLDICTIONARY\";
//                _aria40SystemFilesPath = Dt.Rows[0]["ARIA40SYS"].ToString().TrimEnd();

//                Conn.Close();


//                _aria27SystemFilesConnectionString =
//                    @"Driver={Microsoft Visual FoxPro Driver};sourcedb=" + _aria27SystemFilesPath + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes";

//                _aria40SystemFilesConnectionString =
//                  @"Driver={Microsoft Visual FoxPro Driver};sourcedb=" + _aria40SystemFilesPath + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes";

//                Conn.Close();
//            }

//        }

//    }
//}
// T20110803.0001 MAH June 13 2011 End