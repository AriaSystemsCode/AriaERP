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
        private string _instanceName = "";

        public ClientsControl()
        {
            InitializeComponent();

            dataGridViewClients.AutoGenerateColumns = false;

            XmlDocument xmlInstanceDocument = new XmlDocument();

            xmlInstanceDocument.Load(Path.Combine(Application.StartupPath, "instance settings.xml"));
            for (int index = 0; index < xmlInstanceDocument.ChildNodes.Count; index++)
            {
                if (xmlInstanceDocument.ChildNodes[index].Name.Trim().ToUpper() == "InstanceName".ToUpper())
                {
                    _instanceName = xmlInstanceDocument.ChildNodes[index].InnerText;
                }
            }

            if (_instanceName.Trim() != "")
            {
                _instanceName = "(" + _instanceName + ")";
            }

            XmlDocument xmlDocument = new XmlDocument();

            //xmlDocument.Load(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));
            xmlDocument.Load(Path.Combine(Application.StartupPath, "configuration settings.xml"));

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
            table.Columns.Add("Aria27Access");
            table.Columns.Add("Aria4XPAccess");
            table.Columns.Add("DatabaseCurrentVersion");
            table.Columns.Add("DatabaseTargetVersion");
            table.Columns.Add("CustomizationCurrentVersion");
            table.Columns.Add("CustomizationTargetVersion");

            foreach (DataRow row in table.Rows)
            {
                if (DatabaseUtilities.IsDatabaseExist(_serverName, _dbServerLoginType, _userName, _password, row["CCONDBNAME"].ToString()))
                {
                    int customizationTarget = DatabaseUtilities.GetTargetDatabaseVersion(_serverName, _dbServerLoginType, _userName, _password, "Client.Master", "Customization\\" + row["CCLIENTID"].ToString());
                    int target = DatabaseUtilities.GetTargetDatabaseVersion(_serverName, _dbServerLoginType, _userName, _password, "Client.Master", "");

                    row["DatabaseCurrentVersion"] = DatabaseUtilities.GetDatabaseVersion(_serverName, _dbServerLoginType, _userName, _password, row["CCONDBNAME"].ToString());

                    row["DatabaseTargetVersion"] = target.ToString();

                    row["CustomizationCurrentVersion"] = DatabaseUtilities.GetDatabaseCustomizationVersion(_serverName, _dbServerLoginType, _userName, _password, row["CCONDBNAME"].ToString());

                    row["CustomizationTargetVersion"] = customizationTarget.ToString();

                    if (DatabaseUtilities.GetDatabaseCustomizationVersion(_serverName, _dbServerLoginType, _userName, _password, row["CCONDBNAME"].ToString()) < customizationTarget ||
                        DatabaseUtilities.GetDatabaseVersion(_serverName, _dbServerLoginType, _userName, _password, row["CCONDBNAME"].ToString()) < target)
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
                    //SAB 03/19/2013 [Start]
                    //row["DBStatus"] = _notCreated;
                    row["DBStatus"] = "No Created";
                    //SAB 03/19/2013 [Start]
                }

                row["Aria27Access"] = "Allowed";
                row["Aria4XPAccess"] = "Allowed";

                if (row["REQSERVER"] != DBNull.Value && row["REQSERVER"].ToString().Trim().ToUpper() == SystemInformation.ComputerName.ToUpper())
                {
                    try
                    {
                        AriaEnviromentVariables env = new AriaEnviromentVariables();
                        string url = string.Format("tcp://{0}:{1}/{2}", row["REQSERVER"].ToString().Trim().ToUpper(), "1500", typeof(AriaEnviromentVariables).FullName + _instanceName);

                        AriaEnviromentVariables remoteAriaEnviromentVariables = (AriaEnviromentVariables)Activator.GetObject(typeof(AriaEnviromentVariables), url);

                        //SAB 03-17-2013 Media Modification [Start]
                        if ((row["Aria27Sys"] != DBNull.Value && row["Aria27Sys"].ToString().TrimEnd() != "" && !remoteAriaEnviromentVariables.CanReadFromLocation(row["Aria27Sys"].ToString().Trim().ToUpper().Replace("\\SYSFILES", ""))))
                        {
                            row["Aria27Access"] = "Not Allowed";
                        }
                        //SAB 03-17-2013 Media Modification [Start]

                        if (Aria4XPSharedPath != null && Aria4XPSharedPath.Trim() != "" && !remoteAriaEnviromentVariables.CanReadFromLocation(Aria4XPSharedPath.Trim().ToUpper().Replace("\\SYSFILES", "")))
                        {
                            row["Aria4XPAccess"] = "Not Allowed";
                        }
                    }
                    catch (Exception ex)
                    {
                    }

                    //SAB 03-17-2013 Media Modification [Start]
                    if (OpenRowSetServerName != null && OpenRowSetServerName.TrimEnd() != "" && row["Aria27Sys"] != DBNull.Value && row["Aria27Sys"].ToString().Trim() != "")
                    {
                        if (!DatabaseUtilities.CheckIfSQLServiceCanAccessLocation(OpenRowSetServerName, OpenRowSetDbServerLoginType, OpenRowSetUserName, OpenRowSetPassword, row["Aria27Sys"].ToString().Trim().ToUpper().Replace("\\SYSFILES", "")))
                        {
                            row["Aria27Access"] = "Not Allowed";
                        }
                    }
                    //SAB 03-17-2013 Media Modification [End]
                }
            }
            dataGridViewClients.DataSource = table;

            dataGridViewClients.Columns["DBStatus"].Width = dataGridViewClients.Columns["DBStatus"].GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, true);
            dataGridViewClients.Columns["Aria27Access"].Width = dataGridViewClients.Columns["Aria27Access"].GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, true);
            dataGridViewClients.Columns["Aria4XPAccess"].Width = dataGridViewClients.Columns["Aria4XPAccess"].GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, true);

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
                    notUpated = true;
                }

                if (row["DBStatus"].ToString() != _upToDate || row["Aria27Access"].ToString() != "Allowed" || row["Aria4XPAccess"].ToString() != "Allowed")
                {
                    _status = AriaConfigurationStatusTypes.NotConfigured;
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
                        errorProviderDatabaseNotUpdate.SetError(buttonCreateUpdateDatabase, "Cleints databases is not up to date.");
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
                    //SAB 03/19/2013 [Start]
                    //if (row["DBStatus"].ToString() == _notCreated)
                    if (row["DBStatus"].ToString() == "No Created")
                    //SAB 03/19/2013 [Start]
                    {
                        DatabaseUtilities.CreateDatabase(_serverName, _dbServerLoginType, _userName, _password,
                                                            row["CCONDBNAME"].ToString(),
                                                            null);
                    }

                    int target = DatabaseUtilities.GetTargetDatabaseVersion(_serverName, _dbServerLoginType, _userName, _password, "Client.Master", "");


                    if (DatabaseUtilities.GetDatabaseVersion(_serverName, _dbServerLoginType, _userName, _password, row["CCONDBNAME"].ToString()) < target)
                    {

                        DatabaseUtilities.UpdateDatabase(_serverName, _dbServerLoginType, _userName, _password,
                                                        row["CCONDBNAME"].ToString(),
                                                        "Client.Master",
                                                        null, null);
                    }

                    int customizationTarget = DatabaseUtilities.GetTargetDatabaseVersion(_serverName, _dbServerLoginType, _userName, _password, "Client.Master", "Customization\\" + row["CCLIENTID"].ToString());
                    if (DatabaseUtilities.GetDatabaseCustomizationVersion(_serverName, _dbServerLoginType, _userName, _password, row["CCONDBNAME"].ToString()) < customizationTarget)
                    {
                        DatabaseUtilities.UpdateDatabaseCustomization(_serverName, _dbServerLoginType, _userName, _password,
                                                            row["CCONDBNAME"].ToString(),
                                                            "Client.Master",
                                                            null, "Customization\\" + row["CCLIENTID"].ToString());
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
                        //SAB 04-03-2014 Add validation on the Execution Time [Start]
                        //string xml = String.Format("<?xml version=\"1.0\" standalone=\"yes\"?>" +
                        //                            "<DocumentElement>" +
                        //                            "<ClientConfig>" +
                        //                            "<ClientID>{0}</ClientID>" +
                        //                            "<InstanceName>{1}</InstanceName>" +
                        //                            "<RemoteSrv>{2}</RemoteSrv>" +
                        //                            "<RemotePort>{3}</RemotePort>" +
                        //                            "</ClientConfig>" +
                        //                            "</DocumentElement>", row["CCLIENTID"].ToString().Trim(), _instanceName, row["REQSERVER"].ToString().Trim(), "1500");
                        string xml = String.Format("<?xml version=\"1.0\" standalone=\"yes\"?>" +
                                                    "<DocumentElement>" +
                                                    "<ClientConfig>" +
                                                    "<ClientID>{0}</ClientID>" +
                                                    "<InstanceName>{1}</InstanceName>" +
                                                    "<RemoteSrv>{2}</RemoteSrv>" +
                                                    "<RemotePort>{3}</RemotePort>" +
                                                    "<ClientMaxRecordsPerReport>{4}</ClientMaxRecordsPerReport>" +
                                                    "<ClientMaxExecutionTimePerRequest>{5}</ClientMaxExecutionTimePerRequest>" +
                                                    "<ClientExceedLimitNotificationEmail>{6}</ClientExceedLimitNotificationEmail>" +
                                                    "</ClientConfig>" +
                                                    "</DocumentElement>", row["CCLIENTID"].ToString().Trim(), _instanceName, row["REQSERVER"].ToString().Trim(), "1500", "1000", "600", "");
                        //SAB 04-03-2014 Add validation on the Execution Time [End]

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
                        //SAB 04-03-2014 Add validation on the Execution Time [Start]
                        //string xml = String.Format("<?xml version=\"1.0\" standalone=\"yes\"?>" +
                        //                            "<DocumentElement>" +
                        //                            "<ClientConfig>" +
                        //                            "<ClientID>{0}</ClientID>" +
                        //                            "<InstanceName>{1}</InstanceName>" +
                        //                            "<RemoteSrv>{2}</RemoteSrv>" +
                        //                            "<RemotePort>{3}</RemotePort>" +
                        //                            "</ClientConfig>" +
                        //                            "</DocumentElement>", row["CCLIENTID"].ToString(), "", "", "");
                        string xml = String.Format("<?xml version=\"1.0\" standalone=\"yes\"?>" +
                                                    "<DocumentElement>" +
                                                    "<ClientConfig>" +
                                                    "<ClientID>{0}</ClientID>" +
                                                    "<InstanceName>{1}</InstanceName>" +
                                                    "<RemoteSrv>{2}</RemoteSrv>" +
                                                    "<RemotePort>{3}</RemotePort>" +
                                                    "<ClientMaxRecordsPerReport>{4}</ClientMaxRecordsPerReport>" +
                                                    "<ClientMaxExecutionTimePerRequest>{5}</ClientMaxExecutionTimePerRequest>" +
                                                    "<ClientExceedLimitNotificationEmail>{6}</ClientExceedLimitNotificationEmail>" +
                                                    "</ClientConfig>" +
                                                    "</DocumentElement>", row["CCLIENTID"].ToString().Trim(), "", "", "", "", "", "");
                        //SAB 04-03-2014 Add validation on the Execution Time [End]

                        File.WriteAllText(Path.Combine(row["ARIA40SYS"].ToString(), "Client_Setting.xml"), xml);
                    }
                }
            }

            SetStatus();
        }

        private void dataGridViewClients_CellPainting(object sender, DataGridViewCellPaintingEventArgs e)
        {
            if (e.ColumnIndex == 1)
            {
                if (e.RowIndex >= 0)
                {
                    if (table.Rows[e.RowIndex]["DBStatus"].ToString().TrimEnd() != _upToDate ||
                        table.Rows[e.RowIndex]["Aria27Access"].ToString().TrimEnd() != "Allowed" ||
                        table.Rows[e.RowIndex]["Aria4XPAccess"].ToString().TrimEnd() != "Allowed")
                    {
                        e.CellStyle.BackColor = Color.Red;
                    }
                }
            }

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

            if (e.ColumnIndex == 5)
            {
                if (e.RowIndex >= 0)
                {
                    if (table.Rows[e.RowIndex]["Aria27Access"].ToString().TrimEnd() != "Allowed")
                    {
                        e.CellStyle.BackColor = Color.Red;
                    }
                }
            }

            if (e.ColumnIndex == 6)
            {
                if (e.RowIndex >= 0)
                {
                    if (table.Rows[e.RowIndex]["Aria4XPAccess"].ToString().TrimEnd() != "Allowed")
                    {
                        e.CellStyle.BackColor = Color.Red;
                    }
                }
            }
        }

        // HES-Add the new limitation features on the UI 04/15/2014 client {START}
        private void udCMaxPerAgent_ValueChanged(object sender, EventArgs e)
        {
            if (_init) buttonCSave.Enabled = true;
        }

        private void udCMaxTime_ValueChanged(object sender, EventArgs e)
        {
            if (_init) buttonCSave.Enabled = true;
        }

        private void udCMaxRecPerReq_ValueChanged(object sender, EventArgs e)
        {
            if (_init) buttonCSave.Enabled = true;
        }

        private void txtCExcNotfEmail_TextChanged(object sender, EventArgs e)
        {
            if (_init) buttonCSave.Enabled = true;
        }

        private void buttonCSave_Click(object sender, EventArgs e)
        {
            bool needToUpdateMax = false;
            int count = 0;

            foreach (DataRow row in table.Rows)
            {
                if (row["Select"] != DBNull.Value && Convert.ToBoolean(row["Select"]))
                {
                    needToUpdateMax = true;
                    count++;
                }
            }

            if (!needToUpdateMax)
            {
                MessageBox.Show("You have to select client(s) to update the related Max settings.", "Update Max settings", MessageBoxButtons.OK, MessageBoxIcon.Hand, MessageBoxDefaultButton.Button1);
                return;
            }

            _progressBar.Visible = true;
            _progressBar.Maximum = count + 1;
            _progressBar.Value = 0;

            foreach (DataRow row in table.Rows)
            {
                if (row["Select"] != DBNull.Value && Convert.ToBoolean(row["Select"]))
                {
                    if (row["ARIA40SYS"] != DBNull.Value && row["ARIA40SYS"].ToString().Trim() != "")
                    {
                        string xml = String.Format("<?xml version=\"1.0\" standalone=\"yes\"?>" +
                                                    "<DocumentElement>" +
                                                    "<ClientConfig>" +
                                                    "<ClientID>{0}</ClientID>" +
                                                    "<InstanceName>{1}</InstanceName>" +
                                                    "<RemoteSrv>{2}</RemoteSrv>" +
                                                    "<RemotePort>{3}</RemotePort>" +
                                                    "<ClientMaxRecordsPerReport>{4}</ClientMaxRecordsPerReport>" +
                                                    "<ClientMaxExecutionTimePerRequest>{5}</ClientMaxExecutionTimePerRequest>" +
                                                    "<ClientExceedLimitNotificationEmail>{6}</ClientExceedLimitNotificationEmail>" +
                                                    "</ClientConfig>" +
                                                    "</DocumentElement>", row["CCLIENTID"].ToString().Trim(), _instanceName, row["REQSERVER"].ToString().Trim(), "1500", udCMaxRecPerReq.Value.ToString(), udCMaxTime.Value.ToString(), txtCExcNotfEmail.Text);

                        File.WriteAllText(Path.Combine(row["ARIA40SYS"].ToString(), "Client_Setting.xml"), xml);
                    }
                }
            }

            _progressBar.Visible = false;

            SetStatus();
        }

        private void dataGridViewClients_SelectionChanged(object sender, EventArgs e)
        {
        }

        private void dataGridViewClients_RowEnter(object sender, DataGridViewCellEventArgs e)
        {
            // HES-Add the new limitation features on the UI 04/15/2014 client {START}
            int count = 0;

            foreach (DataRow row in table.Rows)
            {
                if (row["Select"] != DBNull.Value && Convert.ToBoolean(row["Select"]))
                {
                    count++;
                }
            }

            if (count < 2 && table.Rows.Count > 0) 
            {
                XmlDocument xmlDocument = new XmlDocument();

                string clientPath = table.Rows[0]["ARIA40SYS"].ToString();
                xmlDocument.Load(Path.Combine(clientPath, "Client_Setting.xml"));

                XmlElement documentElement = xmlDocument.DocumentElement;

                for (int index = 0; index < documentElement.ChildNodes.Count; index++)
                {
                    if (documentElement.ChildNodes[index].Name == "ClientConfig")
                    {
                        XmlNode xmlNode = null;

                        for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                        {
                            xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                            if (xmlNode.Name == "ClientMaxRecordsPerAgent")
                            {
                                if (xmlNode.InnerText.Trim() == "")
                                {
                                    udCMaxPerAgent.Value = 0;
                                }
                                else
                                {
                                    udCMaxPerAgent.Value = Convert.ToInt32(xmlNode.InnerText);
                                }
                            }

                            if (xmlNode.Name == "ClientMaxRecordsPerReport")
                            {
                                if (xmlNode.InnerText.Trim() == "")
                                {
                                    udCMaxRecPerReq.Value = 0;
                                }
                                else
                                {
                                    udCMaxRecPerReq.Value = Convert.ToInt32(xmlNode.InnerText);
                                }
                            }

                            if (xmlNode.Name == "ClientMaxExecutionTimePerRequest")
                            {
                                if (xmlNode.InnerText.Trim() == "")
                                {
                                    udCMaxTime.Value = 0;
                                }
                                else
                                {
                                    udCMaxTime.Value = Convert.ToInt32(xmlNode.InnerText);
                                }
                            }
                            if (xmlNode.Name == "ClientExceedLimitNotificationEmail")
                            {
                                if (xmlNode.InnerText.Trim() == "")
                                {
                                    txtCExcNotfEmail.Text = "";
                                }
                                else
                                {
                                    txtCExcNotfEmail.Text = xmlNode.InnerText;
                                }
                            }
                        }
                    }
                }
            }
            else
            {
                if (count > 1)
                {
                    udCMaxRecPerReq.Value = 0;
                    udCMaxTime.Value = 0;
                    txtCExcNotfEmail.Text = "";
                    udCMaxPerAgent.Value = 0;
                }
            }
            // HES-Add the new limitation features on the UI 04/15/2014 client {END}
        }

        private void udCMaxTime_Leave(object sender, EventArgs e)
        {
            textBox1.Text = udCMaxTime.Value.ToString();
        }

        private void numericUpDown1_ValueChanged(object sender, EventArgs e)
        {
            MessageBox.Show(numericUpDown1.Value.ToString());
        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {

        }
    }
}