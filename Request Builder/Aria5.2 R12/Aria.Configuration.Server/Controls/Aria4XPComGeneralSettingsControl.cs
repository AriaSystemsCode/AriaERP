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
using Microsoft.SqlServer.Management.Smo;

namespace Aria.Configuration.Server.Controls
{
    /// <summary>
    /// This is User control used to handle all setting related database (SYSFILES path, SQLDICTIONARY path, SQL Server Connection).
    /// </summary>
    public partial class Aria4XPComGeneralSettingsControl : UserControl
    {
        private AriaConfigurationStatusTypes _status;
        public AriaConfigurationStatusTypes Status
        {
            get { return _status; }
            set { _status = value; }
        }

        private string _aria27SystemFilesPath = "";

        private string _aria40SystemFilesPath = "";

        private string _serverName = "";

        private string _databaseName = "";

        private DatabaseServerLoginTypes _dbServerLoginType;

        private string _dbServerUserName = "";

        private string _dbServerPassword = "";

        private bool _isDatabaseUpToDate = false;

        private ListViewItem _ownItem = null;

        private ToolStripProgressBar _progressBar = null;

        public Aria4XPComGeneralSettingsControl(ListViewItem ownItem, ToolStripProgressBar progressBar)
        {
            InitializeComponent();

            this.Anchor = ((AnchorStyles)((((AnchorStyles.Top | AnchorStyles.Bottom) | AnchorStyles.Left) | AnchorStyles.Right)));

            _ownItem = ownItem;

            _progressBar = progressBar;

            Init();
        }

        public void Init()
        {

            SetStatus();
        }


        private void SetStatus()
        {
            errorProvider1.Clear();

            if (!IsApplicationExists(label2.Text))
            {
                errorProvider1.SetError(label2, "The application name not found in the COM+ applications");

                _status = AriaConfigurationStatusTypes.NotConfigured;

                //_comComponentsButton.Enabled = false;

                //label2.Text = "";
            }
            else
            {
                _status = AriaConfigurationStatusTypes.Configured;

                //_comComponentsButton.Enabled = true;

            }
            if (_status == AriaConfigurationStatusTypes.NotConfigured ||
                    ComComponentsControl.Status == AriaConfigurationStatusTypes.NotConfigured)
            {
                _ownItem.ImageIndex = (int)AriaConfigurationStatusTypes.NotConfigured;
            }
            else
            {
                _ownItem.ImageIndex = (int)AriaConfigurationStatusTypes.Configured;
            }
        }

        private bool IsApplicationExists(string applicationName)
        {
            _progressBar.Value = 0;

            _progressBar.Visible = true;

            COMAdmin.COMAdminCatalog catalog = new COMAdmin.COMAdminCatalog();

            COMAdmin.COMAdminCatalogCollection applications = (COMAdmin.COMAdminCatalogCollection)catalog.GetCollection("Applications");

            applications.Populate();

            _progressBar.Maximum = applications.Count;

            COMAdmin.COMAdminCatalogObject application = null;

            for (int index = 0; index < applications.Count; index++)
            {
                _progressBar.Value++;

                application = (COMAdmin.COMAdminCatalogObject)applications.get_Item(index);

                if (applicationName == application.Name.ToString())
                {
                    _progressBar.Value = _progressBar.Maximum;

                    _progressBar.Visible = false;

                    return true;
                }
            }

            _progressBar.Value = _progressBar.Maximum;

            _progressBar.Visible = false;

            return false;
        }



        public ArrayList GetServers()
        {
            DataTable dataTable = SmoApplication.EnumAvailableSqlServers(false);

            ArrayList servers = new ArrayList();

            if (dataTable.Rows.Count > 0)
            {
                foreach (DataRow dataRow in dataTable.Rows)
                {
                    servers.Add(dataRow["Name"].ToString());
                }
            }

            return servers;
        }

        private DatabaseCollection GetDatabases(string serverName)
        {
            Microsoft.SqlServer.Management.Common.ServerConnection serverConnection = new Microsoft.SqlServer.Management.Common.ServerConnection(serverName);

            Microsoft.SqlServer.Management.Smo.Server server = new Microsoft.SqlServer.Management.Smo.Server(serverConnection);

            return server.Databases;
        }

        private bool IsServerExists(string serverName)
        {
            try
            {
                SqlConnection sqlConnection;
                if (_dbServerLoginType == DatabaseServerLoginTypes.SqlServerAuthentication)
                {
                    sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=Master;User ID=" + _dbServerUserName + @";Password=" + _dbServerPassword + @";Trusted_Connection=no");
                }
                else
                {
                    sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=Master;Integrated Security=true");
                }

                sqlConnection.Open();
                sqlConnection.Close();
                return true;
            }
            catch (Exception e)
            {
                return false;
            }
        }

        private bool IsDatabaseExists(string serverName, string databaseName)
        {
            try
            {
                SqlConnection sqlConnection;
                if (_dbServerLoginType == DatabaseServerLoginTypes.SqlServerAuthentication)
                {
                    sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + "Master" + @";User ID=" + _dbServerUserName + @";Password=" + _dbServerPassword + @";Trusted_Connection=no");
                }
                else
                {
                    sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + "Master" + @";Integrated Security=true");
                }

                sqlConnection.Open();

                SqlCommand s = new SqlCommand("SELECT * FROM sysdatabases WHERE [Name] = '" + databaseName + "'", sqlConnection);

                SqlDataAdapter sqlAdpater = new SqlDataAdapter(s);
                DataTable newTable = new DataTable();

                sqlAdpater.Fill(newTable);

                sqlConnection.Close();


                return newTable.Rows.Count > 0;
            }
            catch (Exception e)
            {
                return false;
            }
        }

    }
}
