using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Data.SqlClient;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Collections;
using System.Xml;
using System.Diagnostics;
using System.Data.Odbc;
using System.Data.Sql;
using System.Text.RegularExpressions;
using Aria.Configuration.Server.Properties;
using System.Management;
using System.ServiceProcess;
using Microsoft.SqlServer.Management.Common;

namespace Aria.Configuration.Server.Controls
{
    /// <summary>
    /// This is User control used to handle all setting related database (SYSFILES path, SQLDICTIONARY path, SQL Server Connection).
    /// </summary>
    public partial class MutliDatabaseServer : UserControl
    {
        public event ConfigurationChangedHandler ConfigurationChanged;

        private bool _init = false;
        private bool _userNamePassChanged = false;
        private string _serviceName = "";

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

        public MutliDatabaseServer(Button ownButton, ToolStripProgressBar progressBar)
        {
            InitializeComponent();

            comboBoxLoginType.SelectedIndex = 1;
            comboBoxLoginType.Enabled = false;

            this.Anchor = ((AnchorStyles)((((AnchorStyles.Top | AnchorStyles.Bottom) | AnchorStyles.Left) | AnchorStyles.Right)));

            _ownButton = ownButton;

            _progressBar = progressBar;

            Init();
        }

        public void Init()
        {
            XmlDocument xmlDocument = new XmlDocument();

            xmlDocument.Load(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));

            XmlElement documentElement = xmlDocument.DocumentElement;

            for (int index = 0; index < documentElement.ChildNodes.Count; index++)
            {
                if (documentElement.ChildNodes[index].Name == "OpenRowSetServer")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "ServerName")
                        {
                            _serverName = xmlNode.InnerText;
                            comboBoxServerName.Text = _serverName;
                        }
                        else if (xmlNode.Name == "ServerLoginType")
                        {
                            _dbServerLoginType = (DatabaseServerLoginTypes)Enum.Parse(typeof(DatabaseServerLoginTypes), xmlNode.InnerText);
                            if (_dbServerLoginType == DatabaseServerLoginTypes.WindowAuthentication)
                            {
                                //comboBoxLoginType.SelectedIndex = 0;
                            }
                            else
                            {
                                comboBoxLoginType.SelectedIndex = 1;
                            }
                        }
                        else if (xmlNode.Name == "UserName")
                        {
                            _userName = xmlNode.InnerText;
                            textBoxUserName.Text = _userName;
                        }
                        else if (xmlNode.Name == "Password")
                        {
                            _password = xmlNode.InnerText;
                            textBoxPassword.Text = _password;
                        }
                    }
                }
            }

            SetStatus();

            _init = true;
        }

        private void SetStatus()
        {
            _status = AriaConfigurationStatusTypes.Configured;

            string error = "";

            errorProviderCannotCaonnect.Clear();

            if (DatabaseUtilities.IsServerExists(comboBoxServerName.Text,
                                     comboBoxLoginType.SelectedIndex == 0 ? DatabaseServerLoginTypes.WindowAuthentication : DatabaseServerLoginTypes.SqlServerAuthentication,
                                     textBoxUserName.Text,
                                     textBoxPassword.Text, ref error))
            {
                errorProviderCannotCaonnect.Clear();

                DataTable table = DatabaseUtilities.GetDataTable(comboBoxServerName.Text,
                                                                 comboBoxLoginType.SelectedIndex == 0 ? DatabaseServerLoginTypes.WindowAuthentication : DatabaseServerLoginTypes.SqlServerAuthentication,
                                                                 textBoxUserName.Text,
                                                                 textBoxPassword.Text,
                                                                 "Master", "SELECT IS_SRVROLEMEMBER('sysadmin', '" + textBoxUserName.Text + "') AS Result");

                if (table.Rows.Count == 0 || table.Rows[0]["Result"].ToString().Trim() != "1")
                {
                    _status = AriaConfigurationStatusTypes.NotConfigured;
                    errorProviderCannotCaonnect.SetError(textBoxUserName, "User must belong to server system administrator (sysadmin) role.");
                }

                if (DatabaseUtilities.Is64Server(comboBoxServerName.Text,
                                                 comboBoxLoginType.SelectedIndex == 0 ? DatabaseServerLoginTypes.WindowAuthentication : DatabaseServerLoginTypes.SqlServerAuthentication,
                                                 textBoxUserName.Text,
                                                 textBoxPassword.Text))
                {
                    _status = AriaConfigurationStatusTypes.NotConfigured;
                    errorProviderCannotCaonnect.SetError(comboBoxServerName, "SQL Server must be x86 edition.");
                }
            }
            else
            {
                _status = AriaConfigurationStatusTypes.NotConfigured;
                if (comboBoxServerName.Text.Trim().Length == 0)
                {
                    errorProviderCannotCaonnect.SetError(comboBoxServerName, "Server name is not selected.");
                }
                else
                {
                    errorProviderCannotCaonnect.SetError(comboBoxServerName, error);
                }
            }

            errorProviderInvalidFolder.Clear();

            switch (_status)
            {
                case AriaConfigurationStatusTypes.Configured:
                    //SqlConnection sqlConnection;
                    //if ((comboBoxLoginType.SelectedIndex == 0 ? DatabaseServerLoginTypes.WindowAuthentication : DatabaseServerLoginTypes.SqlServerAuthentication) == DatabaseServerLoginTypes.SqlServerAuthentication)
                    //{
                    //    sqlConnection = new SqlConnection(@"Data Source=" + comboBoxServerName.Text + @";Initial Catalog=" + "Master" + @";User ID=" + textBoxUserName.Text + @";Password=" + textBoxPassword.Text + @";Trusted_Connection=no");
                    //}
                    //else
                    //{
                    //    sqlConnection = new SqlConnection(@"Data Source=" + comboBoxServerName.Text + @";Initial Catalog=" + "Master" + @";Integrated Security=true");
                    //}

                    //Microsoft.SqlServer.Management.Smo.Server server = new Microsoft.SqlServer.Management.Smo.Server(new ServerConnection(sqlConnection));

                    //_serverName = server.ServiceName;

                    //string objPath = string.Format("Win32_Service.Name='{0}'", "MSSQL$" + _serverName);
                    //using (ManagementObject service = new ManagementObject(new ManagementPath(objPath)))
                    //{
                    //    string value = service.GetPropertyValue("StartMode").ToString();

                    //    if (value == "Auto")
                    //    {
                    //        value = "Automatic";
                    //    }

                    //    comboBoxStartType.SelectedItem = value;
                    //    if (service.GetPropertyValue("StartName").ToString().Trim() == "LocalSystem")
                    //    {
                    //        comboBoxAccountType.SelectedIndex = 0;
                    //        textBoxServiceUserName.Text = "";
                    //        textBoxServicePassword.Text = "".PadRight(15, '*');
                    //        textBoxServiceUserName.Enabled = false;
                    //        textBoxServicePassword.Enabled = false;
                    //    }
                    //    else
                    //    {
                    //        comboBoxAccountType.SelectedIndex = 1;
                    //        textBoxServiceUserName.Text = service.GetPropertyValue("StartName").ToString();
                    //        textBoxServicePassword.Text = "".PadRight(15, '*');
                    //        textBoxServiceUserName.Enabled = true;
                    //        textBoxServicePassword.Enabled = true;
                    //    }
                    //}

                    //panel1.Visible = true;
                    _ownButton.Image = Resources.Pass;
                    break;

                case AriaConfigurationStatusTypes.NotConfigured:
                    //panel1.Visible = false;
                    _ownButton.Image = Resources.NotPass;
                    break;
            }

            if (_init && buttonSave.Enabled && _status == AriaConfigurationStatusTypes.Configured)
            {
                if (MessageBox.Show("Do you want save the settings?", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question, MessageBoxDefaultButton.Button1) == DialogResult.Yes)
                {
                    buttonSave.Enabled = false;
                    Save();
                }
            }

            if (ConfigurationChanged != null)
            {
                ConfigurationChanged();
            }
        }

        private void comboBoxServerName_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (_init) buttonSave.Enabled = true;
        }

        private void comboBoxServerName_DropDown(object sender, EventArgs e)
        {
            if (comboBoxServerName.Items.Count != 0) return;

            ArrayList servers = DatabaseUtilities.GetServers();

            foreach (object server in servers)
            {
                comboBoxServerName.Items.Add(server.ToString());
            }
        }

        private void comboBoxLoginType_SelectedIndexChanged(object sender, EventArgs e)
        {
            textBoxUserName.Enabled = comboBoxLoginType.SelectedIndex == 1;
            textBoxPassword.Enabled = comboBoxLoginType.SelectedIndex == 1;

            if (_init) buttonSave.Enabled = true;
        }

        private void textBoxUserName_TextChanged(object sender, EventArgs e)
        {
            if (_init) buttonSave.Enabled = true;
        }

        private void textBoxPassword_TextChanged(object sender, EventArgs e)
        {
            if (_init) buttonSave.Enabled = true;
        }

        private void buttonTestConection_Click(object sender, EventArgs e)
        {
            string error = "";
            if (DatabaseUtilities.IsServerExists(comboBoxServerName.Text,
                                                 comboBoxLoginType.SelectedIndex == 0 ? DatabaseServerLoginTypes.WindowAuthentication : DatabaseServerLoginTypes.SqlServerAuthentication,
                                                 textBoxUserName.Text,
                                                 textBoxPassword.Text, ref error))
            {
                _status = AriaConfigurationStatusTypes.Configured;
                errorProviderCannotCaonnect.Clear();
                MessageBox.Show("TESTS COMPLETED SUCCESSFULLY!", "Connection", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }

            SetStatus();
        }

        private void textBoxAria4XPSharedPath_TextChanged(object sender, EventArgs e)
        {
            if (_init) buttonSave.Enabled = true;
        }

        private void Save()
        {
            SetStatus();

            if (Status == AriaConfigurationStatusTypes.Configured)
            {
                XmlDocument xmlDocument = new XmlDocument();

                xmlDocument.Load(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));

                XmlElement documentElement = xmlDocument.DocumentElement;

                for (int index = 0; index < documentElement.ChildNodes.Count; index++)
                {
                    if (documentElement.ChildNodes[index].Name == "OpenRowSetServer")
                    {
                        XmlNode xmlNode = null;

                        for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                        {
                            xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                            if (xmlNode.Name == "ServerName")
                            {
                                xmlNode.InnerText = comboBoxServerName.Text;
                            }
                            else if (xmlNode.Name == "ServerLoginType")
                            {
                                xmlNode.InnerText = Enum.Parse(typeof(DatabaseServerLoginTypes), comboBoxLoginType.Text).ToString();
                            }
                            else if (xmlNode.Name == "UserName")
                            {
                                xmlNode.InnerText = textBoxUserName.Text;
                            }
                            else if (xmlNode.Name == "Password")
                            {
                                xmlNode.InnerText = textBoxPassword.Text;
                            }
                        }
                    }
                }

                xmlDocument.Save(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));
                
                SqlConnection sqlConnection;
                sqlConnection = new SqlConnection(@"Data Source=" + comboBoxServerName.Text + @";Initial Catalog=master;User ID=" + textBoxUserName.Text + @";Password=" + textBoxPassword.Text + @";Trusted_Connection=no");

                sqlConnection.Open();

                string command = "sp_configure 'show advanced options', 1; \n" + 
                                    "RECONFIGURE; \n" +
                                    "GO \n" +
                                    "sp_configure 'Ad Hoc Distributed Queries', 1; \n" +
                                    "RECONFIGURE; \n" +
                                    "GO";

                Microsoft.SqlServer.Management.Smo.Server server = new Microsoft.SqlServer.Management.Smo.Server(new ServerConnection(sqlConnection));
                server.ConnectionContext.ExecuteNonQuery(command);

                buttonSave.Enabled = false;
            }
        }

        private void comboBoxStartType_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (_init)
            {
                buttonChangeAccount.Enabled = true;
            }
        }

        private void comboBoxAccountType_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (comboBoxAccountType.SelectedItem.ToString() == "User")
            {
                textBoxServiceUserName.Enabled = true;
                textBoxServicePassword.Enabled = true;
            }
            else
            {
                textBoxServiceUserName.Enabled = false;
                textBoxServicePassword.Enabled = false;
            }

            if (_init)
            {
                buttonChangeAccount.Enabled = true;
                _userNamePassChanged = true;
            }
        }

        private void textBoxServiceUserName_TextChanged(object sender, EventArgs e)
        {
            if (_init)
            {
                buttonChangeAccount.Enabled = true;
                _userNamePassChanged = true;
            }
        }

        private void textBoxServicePassword_TextChanged(object sender, EventArgs e)
        {
            if (_init)
            {
                buttonChangeAccount.Enabled = true;
                _userNamePassChanged = true;
            }
        }

        private void buttonChangeAccount_Click(object sender, EventArgs e)
        {
            string username = textBoxServiceUserName.Text;

            if (comboBoxStartType.SelectedIndex == 1)
            {
                MessageBox.Show("Cannot disable the service.", "Settings", MessageBoxButtons.OK, MessageBoxIcon.Hand);

                return;
            }

            if (comboBoxAccountType.SelectedIndex == 1 && _userNamePassChanged)
            {
                try
                {
                    if (!username.Contains("\\")) username = ".\\" + username;

                    using (new Impersonator(username.Split('\\')[1],
                                            username.Split('\\')[0],
                                            textBoxServicePassword.Text))
                    {

                    }
                }
                catch (Exception)
                {
                    MessageBox.Show("Invalid user name and password.", "Login", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    return;
                }
            }

            MessageBox.Show("The system will restart the service, this process may take one or more minutes. Please wait.", "Restart", MessageBoxButtons.OK, MessageBoxIcon.Information);

            ServiceController controller = new ServiceController("MSSQL$" + _serverName);

            if (controller.Status != ServiceControllerStatus.Stopped)
            {
                controller.Stop();
                controller.WaitForStatus(ServiceControllerStatus.Stopped, new TimeSpan(0, 0, 60));
            }

            ManagementBaseObject inParams = null;

            ManagementObject srvc = new ManagementObject("Win32_Service.Name='" + "MSSQL$" + _serverName + "'");

            inParams = srvc.GetMethodParameters("Change");

            inParams["StartMode"] = comboBoxStartType.SelectedItem.ToString();

            if (_userNamePassChanged)
            {
                if (comboBoxAccountType.SelectedIndex == 0)
                {
                    inParams["StartName"] = ".\\LocalSystem";
                }
                else
                {
                    inParams["StartName"] = username.Replace(".\\", SystemInformation.ComputerName.ToUpper() + "\\");
                    inParams["StartPassword"] = textBoxServicePassword.Text;
                }
            }

            ManagementBaseObject outParams = srvc.InvokeMethod("Change", inParams, null);

            object w = System.Convert.ToInt32(outParams.Properties["ReturnValue"].Value);

            try
            {
                controller.Start();
                controller.WaitForStatus(ServiceControllerStatus.Running, new TimeSpan(0, 0, 60));

                buttonChangeAccount.Enabled = false;
                _userNamePassChanged = false;

                System.Threading.Thread.Sleep(60000);
            }
            catch (Exception ex)
            {
                MessageBox.Show(String.Format("Cannot start the service due to the following error '{0}'. \n Note: Login as service right must assign to selected user (Administrators do not have this right by default).", ex.GetBaseException().Message), "Login", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

            SetStatus();
        }

        private void buttonSave_Click(object sender, EventArgs e)
        {
            Save();
        }

        private void button1_Click(object sender, EventArgs e)
        {
             System.Diagnostics.Process.Start(Path.Combine(Application.StartupPath, "SQLEXPR_x86_ENU.exe"), "");

        }
    }
}
