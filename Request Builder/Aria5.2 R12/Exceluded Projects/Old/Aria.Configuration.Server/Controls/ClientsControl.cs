using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using System.IO;
using Aria.Configuration.Server.Properties;
using Aria.Environment;

namespace Aria.Configuration.Server.Controls
{
    public partial class ClientsControl : UserControl
    {
        public event ConfigurationChangedHandler ConfigurationChanged;

        private bool _init = false;
        const string _upToDate = "Database is up to date";
        const string _notCreated = "Database is not created";
        const string _notUpdated = "Database is not updated";

        private Button _ownButton = null;
        public Button OwnButton
        {
            get { return _ownButton; }
            set { _ownButton = value; }
        }

        private AriaConfigurationStatusTypes _status;
        public AriaConfigurationStatusTypes Status
        {
            get { return _status; }
            set { _status = value; }
        }

        private ToolStripProgressBar _progressBar = null;
        public ToolStripProgressBar ProgressBar
        {
            get { return _progressBar; }
            set { _progressBar = value; }
        }

        private string _serverName = "";
        public string ServerName
        {
            get { return _serverName; }
            set { _serverName = value; }
        }

        private DatabaseServerLoginTypes _dbServerLoginType;
        public DatabaseServerLoginTypes DbServerLoginType
        {
            get { return _dbServerLoginType; }
            set { _dbServerLoginType = value; }
        }

        private string _userName = "";
        public string UserName
        {
            get { return _userName; }
            set { _userName = value; }
        }

        private string _password = "";
        public string Password
        {
            get { return _password; }
            set { _password = value; }
        }

        private string _openRowSetServerName = "";
        public string OpenRowSetServerName
        {
            get { return _openRowSetServerName; }
            set { _openRowSetServerName = value; }
        }

        private DatabaseServerLoginTypes _openRowSetDbServerLoginType;
        public DatabaseServerLoginTypes OpenRowSetDbServerLoginType
        {
            get { return _openRowSetDbServerLoginType; }
            set { _openRowSetDbServerLoginType = value; }
        }

        private string _openRowSetUserName = "";
        public string OpenRowSetUserName
        {
            get { return _openRowSetUserName; }
            set { _openRowSetUserName = value; }
        }

        private string _openRowSetPassword = "";
        public string OpenRowSetPassword
        {
            get { return _openRowSetPassword; }
            set { _openRowSetPassword = value; }
        }

        private string _aria4XPSharedPath = "";
        public string Aria4XPSharedPath
        {
            get { return _aria4XPSharedPath; }
            set { _aria4XPSharedPath = value; }
        }

        DataTable table;

        public ClientsControl()
        {
            InitializeComponent();

            dataGridViewClients.AutoGenerateColumns = false;

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
                            _serverName = xmlNode.InnerText;
                        }
                        else if (xmlNode.Name == "ServerLoginType")
                        {
                            _dbServerLoginType = (DatabaseServerLoginTypes)Enum.Parse(typeof(DatabaseServerLoginTypes), xmlNode.InnerText);
                        }
                        else if (xmlNode.Name == "UserName")
                        {
                            _userName = xmlNode.InnerText;
                        }
                        else if (xmlNode.Name == "Password")
                        {
                            _password = xmlNode.InnerText;
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
                            _openRowSetServerName = xmlNode.InnerText;
                        }
                        else if (xmlNode.Name == "ServerLoginType")
                        {
                            _openRowSetDbServerLoginType = (DatabaseServerLoginTypes)Enum.Parse(typeof(DatabaseServerLoginTypes), xmlNode.InnerText);
                        }
                        else if (xmlNode.Name == "UserName")
                        {
                            _openRowSetUserName = xmlNode.InnerText;
                        }
                        else if (xmlNode.Name == "Password")
                        {
                            _openRowSetPassword = xmlNode.InnerText;
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
                            _aria4XPSharedPath = xmlNode.InnerText;
                        }
                    }
                }
            }

            table = DatabaseUtilities.GetDataTable(_serverName, _dbServerLoginType, _userName, _password, "System.Master", "Select * From Clients");
            table.Columns.Add("Select", typeof(bool));
            table.Columns.Add("DBStatus");

            int target = DatabaseUtilities.GetTargetDatabaseVersion(_serverName, _dbServerLoginType, _userName, _password, "Client.Master");

            foreach (DataRow row in table.Rows)
            {
                string customizationFile = Path.Combine(Application.StartupPath, @"Customization\" + row["CCONDBNAME"].ToString() + ".xml");
                if (DatabaseUtilities.IsDatabaseExist(_serverName, _dbServerLoginType, _userName, _password, row["CCONDBNAME"].ToString()))
                {
                    if (DatabaseUtilities.GetDatabaseVersion(_serverName, _dbServerLoginType, _userName, _password, row["CCONDBNAME"].ToString()) < target ||
                        File.Exists(customizationFile))
                    {
                        row["DBStatus"] = _notUpdated;
                    }
                    else
                    {
                        row["DBStatus"] = _upToDate;
                    }
                }
                else
                {
                    row["DBStatus"] = "No Created";
                }

                if (row["REQSERVER"] != DBNull.Value && row["REQSERVER"].ToString().Trim().ToUpper() == SystemInformation.ComputerName.ToUpper())
                {
                    try
                    {
                        AriaEnviromentVariables env = new AriaEnviromentVariables();
                        string url = string.Format("tcp://{0}:{1}/{2}", row["REQSERVER"].ToString().Trim().ToUpper(), "1500", typeof(AriaEnviromentVariables).FullName);

                        AriaEnviromentVariables remoteAriaEnviromentVariables = (AriaEnviromentVariables)Activator.GetObject(typeof(AriaEnviromentVariables), url);

                        if ((row["Aria27Sys"] != DBNull.Value && row["Aria27Sys"].ToString().TrimEnd() != "" && !remoteAriaEnviromentVariables.CanReadFromLocation(row["Aria27Sys"].ToString().Trim().ToUpper().Replace("\\SYSFILES", ""))) ||
                            (Aria4XPSharedPath != null && Aria4XPSharedPath.Trim() != "" && !remoteAriaEnviromentVariables.CanReadFromLocation(Aria4XPSharedPath.Trim().ToUpper().Replace("\\SQLDICTIONARY", "")))
                          )
                        {
                            row["DBStatus"] += "; Request Handler service cannot access Aria folders.";
                        }
                    }
                    catch (Exception ex)
                    {
                    }

                    if (OpenRowSetServerName != null && OpenRowSetServerName.TrimEnd() != "" && row["Aria27Sys"] != DBNull.Value && row["Aria27Sys"].ToString().Trim() != "")
                    {
                        if (!DatabaseUtilities.CheckIfSQLServiceCanAccessLocation(OpenRowSetServerName, OpenRowSetDbServerLoginType, OpenRowSetUserName, OpenRowSetPassword, row["Aria27Sys"].ToString().Trim().ToUpper().Replace("\\SYSFILES", "")))
                        {
                            row["DBStatus"] += "; Mutliple Database service cannot access Aria folders.";
                        }
                    }
                }
            }
            dataGridViewClients.DataSource = table;

            dataGridViewClients.Columns["DBStatus"].Width = dataGridViewClients.Columns["DBStatus"].GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, true);

            dataGridViewClients.Refresh();
        }

        public ClientsControl(Button ownButton, ToolStripProgressBar progressBar)
            : this()
        {
            InitializeComponent();

            this.Anchor = ((AnchorStyles)((((AnchorStyles.Top | AnchorStyles.Bottom) | AnchorStyles.Left) | AnchorStyles.Right)));

            _ownButton = ownButton;

            _progressBar = progressBar;

            Init();
        }

        public void Init()
        {
            SetStatus();

            _init = true;
        }

        private void SetStatus()
        {
            _status = AriaConfigurationStatusTypes.Configured;

            errorProviderDatabaseNotUpdate.Clear();

            bool notUpated = false;

            foreach (DataRow row in table.Rows)
            {
                if (row["DBStatus"].ToString() != _upToDate)
                {
                    _status = AriaConfigurationStatusTypes.NotConfigured;
                }

                if (row["DBStatus"].ToString().Contains(_notUpdated) ||
                    row["DBStatus"].ToString().Contains(_notCreated))
                {
                    notUpated = true;
                }
            }

            switch (_status)
            {
                case AriaConfigurationStatusTypes.Configured:
                    _ownButton.Image = Resources.Pass;
                    break;

                case AriaConfigurationStatusTypes.NotConfigured:

                    if (notUpated)
                    {
                        errorProviderDatabaseNotUpdate.SetError(buttonCreateUpdateDatabase, "Database is not updated.");
                    }
                    _ownButton.Image = Resources.NotPass;
                    break;
            }

            if (ConfigurationChanged != null)
            {
                ConfigurationChanged();
            }
        }

        private void buttonCreateUpdateDatabase_Click(object sender, EventArgs e)
        {
            bool needToUpdate = false;
            int count = 0;

            foreach (DataRow row in table.Rows)
            {
                if ((row["DBStatus"].ToString().Contains(_notUpdated) || row["DBStatus"].ToString().Contains(_notCreated)) &&
                    (row["Select"] != DBNull.Value && Convert.ToBoolean(row["Select"])))
                {
                    needToUpdate = true;
                    count++;
                }
            }

            if (!needToUpdate)
            {
                MessageBox.Show("You have to select client to update or all clients are updated.", "Update", MessageBoxButtons.OK, MessageBoxIcon.Hand, MessageBoxDefaultButton.Button1);
                return;
            }

            _progressBar.Visible = true;
            _progressBar.Maximum = count + 1;
            _progressBar.Value = 0;

            foreach (DataRow row in table.Rows)
            {
                if ((row["DBStatus"].ToString().Contains(_notUpdated) || row["DBStatus"].ToString().Contains(_notCreated)) &&
                    (row["Select"] != DBNull.Value && Convert.ToBoolean(row["Select"])))
                {
                    if (row["DBStatus"].ToString() == "No Created")
                    {
                        DatabaseUtilities.CreateDatabase(_serverName, _dbServerLoginType, _userName, _password,
                                                            row["CCONDBNAME"].ToString(),
                                                            null);
                    }

                    DatabaseUtilities.UpdateDatabase(_serverName, _dbServerLoginType, _userName, _password,
                                                        row["CCONDBNAME"].ToString(),
                                                        "Client.Master",
                                                        null);

                    string customizationFile = Path.Combine(Application.StartupPath, @"Customization\" + row["CCONDBNAME"].ToString() + ".xml");
                    if (File.Exists(customizationFile))
                    {
                        DatabaseUtilities.AppendXmlToDatabase(_serverName, _dbServerLoginType, _userName, _password,
                                                              row["CCONDBNAME"].ToString(),
                                                              customizationFile,
                                                              null, 100);
                        File.Delete(customizationFile);
                    }

                    row["DBStatus"] = _upToDate;

                    _progressBar.Value++;
                }
            }

            _progressBar.Visible = false;

            SetStatus();
        }

        private void buttonAttach_Click(object sender, EventArgs e)
        {
            bool needToAttach = false;

            foreach (DataRow row in table.Rows)
            {
                if (row["Select"] != DBNull.Value && Convert.ToBoolean(row["Select"]) &&
                   (row["ReqServer"] == DBNull.Value || row["ReqServer"].ToString().Trim().ToUpper() != SystemInformation.ComputerName.Trim().ToUpper()))
                {
                    needToAttach = true;
                }
            }

            if (!needToAttach)
            {
                MessageBox.Show("You have to select client to assign or all clients are assigned.", "Update", MessageBoxButtons.OK, MessageBoxIcon.Hand, MessageBoxDefaultButton.Button1);
                return;
            }

            foreach (DataRow row in table.Rows)
            {
                if (row["Select"] != DBNull.Value && Convert.ToBoolean(row["Select"]) &&
                    (row["ReqServer"] == DBNull.Value || row["ReqServer"].ToString().Trim().ToUpper() != SystemInformation.ComputerName.Trim().ToUpper()))
                {
                    if (row["REQSERVER"] != DBNull.Value && row["REQSERVER"].ToString() != "")
                    {
                        string sqlDicPath = row["REQSERVER"].ToString().Trim();


                    }

                    row["REQSERVER"] = SystemInformation.ComputerName;

                    DatabaseUtilities.ExecuteCommand(_serverName, _dbServerLoginType, _userName, _password,
                         "System.Master",
                          "UPDATE Clients Set REQSERVER = '" + SystemInformation.ComputerName + "' WHERE CCLIENTID = '" + row["CCLIENTID"].ToString() + "'");

                    if (row["ARIA40SYS"] != DBNull.Value && row["ARIA40SYS"].ToString().Trim() != "")
                    {
                        string xml = String.Format("<?xml version=\"1.0\" standalone=\"yes\"?>" +
                                                    "<DocumentElement>" +
                                                    "<ClientConfig>" +
                                                    "<ClientID>{0}</ClientID>" +
                                                    "<RemoteSrv>{1}</RemoteSrv>" +
                                                    "<RemotePort>{2}</RemotePort>" +
                                                    "</ClientConfig>" +
                                                    "</DocumentElement>", row["CCLIENTID"].ToString().Trim(), table.Rows[0]["REQSERVER"].ToString().Trim(), "1500");

                        File.WriteAllText(Path.Combine(row["ARIA40SYS"].ToString(), "Client_Setting.xml"), xml);
                    }
                }
            }

            SetStatus();
        }

        private void buttonUnassign_Click(object sender, EventArgs e)
        {
            bool needToDettach = false;

            foreach (DataRow row in table.Rows)
            {
                if (row["Select"] != DBNull.Value && Convert.ToBoolean(row["Select"]) &&
                    (row["ReqServer"] != DBNull.Value && row["ReqServer"].ToString().Trim().ToUpper() == SystemInformation.ComputerName.Trim().ToUpper()))
                {
                    needToDettach = true;
                }
            }

            if (!needToDettach)
            {
                MessageBox.Show("You have to select client to unassign or all clients are unassigned.", "Update", MessageBoxButtons.OK, MessageBoxIcon.Hand, MessageBoxDefaultButton.Button1);
                return;
            }

            foreach (DataRow row in table.Rows)
            {
                if (row["Select"] != DBNull.Value && Convert.ToBoolean(row["Select"]) &&
                    (row["ReqServer"] != DBNull.Value && row["ReqServer"].ToString().Trim().ToUpper() == SystemInformation.ComputerName.Trim().ToUpper()))
                {
                    row["REQSERVER"] = "";

                    DatabaseUtilities.ExecuteCommand(_serverName, _dbServerLoginType, _userName, _password,
                         "System.Master",
                          "UPDATE Clients Set REQSERVER = '" + SystemInformation.ComputerName + "' WHERE CCLIENTID = ''");

                    if (row["ARIA40SYS"] != DBNull.Value && row["ARIA40SYS"].ToString().Trim() != "")
                    {
                        string xml = String.Format("<?xml version=\"1.0\" standalone=\"yes\"?>" +
                                                    "<DocumentElement>" +
                                                    "<ClientConfig>" +
                                                    "<ClientID>{0}</ClientID>" +
                                                    "<RemoteSrv>{1}</RemoteSrv>" +
                                                    "<RemotePort>{2}</RemotePort>" +
                                                    "</ClientConfig>" +
                                                    "</DocumentElement>", row["CCLIENTID"].ToString(), "", "");

                        File.WriteAllText(Path.Combine(row["ARIA40SYS"].ToString(), "Client_Setting.xml"), xml);
                    }
                }
            }

            SetStatus();
        }

        private void dataGridViewClients_CellPainting(object sender, DataGridViewCellPaintingEventArgs e)
        {
            if (e.ColumnIndex == 4)
            {
                if (e.RowIndex >= 0)
                {
                    if (table.Rows[e.RowIndex]["DBStatus"].ToString().TrimEnd() != _upToDate)
                    {
                        e.CellStyle.BackColor = Color.Red;
                    }
                }
            }
        }
    }
}