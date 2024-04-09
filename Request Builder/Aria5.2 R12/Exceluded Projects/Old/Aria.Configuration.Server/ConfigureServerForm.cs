using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Aria.Configuration.Server.Controls;
using System.Xml;

namespace Aria.Configuration.Server
{
    public delegate void ConfigurationChangedHandler();

    public partial class ConfigureServerForm : Form
    {
        private AriaConfigurationStatusTypes _serverStatus;
        public AriaConfigurationStatusTypes ServerStatus
        {
            get { return _serverStatus; }
            set { _serverStatus = value; }
        }

        private DatabaseSetupControl databaseSetupControl = null;

        private RequestHandlerServiceControl requestHandlerServiceControl = null;

        private ClientsControl clientsControl = null;

        private Aria4XPSetupControl aria4XPSetupControl = null;

        public ConfigureServerForm()
        {
            InitializeComponent();

            Init();
        }

        private void Init()
        {
            XmlDocument xmlDocument = new XmlDocument();

            xmlDocument.Load(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));

            XmlElement documentElement = xmlDocument.DocumentElement;

            for (int index = 0; index < documentElement.ChildNodes.Count; index++)
            {
                if (documentElement.ChildNodes[index].Name == "FileServer")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "Aria40SharedPath")
                        {
                            if (xmlNode.InnerText.Trim() == "")
                            {
                                buttonAria4XPSettings.Visible = false;
                                buttonAria4XPSettings.Enabled = false;
                            }
                            else
                            {
                                aria4XPSetupControl = new Aria4XPSetupControl(buttonAria4XPSettings, toolStripProgressBar1);
                            }
                        }
                    }
                }
            }

            buttonDatabaseSetup_Click(null, null);
        }
    
        private void button1_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        void databaseSetupControl_ConfigurationChanged()
        {
            if (databaseSetupControl.Status == AriaConfigurationStatusTypes.Configured)
            {
                buttonRequestHandlerService.Enabled = true;
                requestHandlerServiceControl = new RequestHandlerServiceControl(buttonRequestHandlerService, toolStripProgressBar1);
                requestHandlerServiceControl.ConfigurationChanged += new ConfigurationChangedHandler(requestHandlerServiceControl_ConfigurationChanged);

                buttonClients.Enabled = true;
                clientsControl = new ClientsControl(buttonClients, toolStripProgressBar1);
            }
            else
            {
                buttonRequestHandlerService.Enabled = false;
            }
        }

        private void buttonDatabaseSetup_Click(object sender, EventArgs e)
        {
            this.panel2.Controls.Clear();
            if (databaseSetupControl == null)
            {
                databaseSetupControl = new DatabaseSetupControl(buttonDatabaseSetup, toolStripProgressBar1);
                databaseSetupControl.ConfigurationChanged += new ConfigurationChangedHandler(databaseSetupControl_ConfigurationChanged);
                databaseSetupControl_ConfigurationChanged();
            }
            this.panel2.Controls.Add(databaseSetupControl);
        }

        void requestHandlerServiceControl_ConfigurationChanged()
        {
        }

        private void buttonRequestHandlerService_Click(object sender, EventArgs e)
        {
            this.panel2.Controls.Clear();
            if (requestHandlerServiceControl == null)
            {
                requestHandlerServiceControl = new RequestHandlerServiceControl(buttonRequestHandlerService, toolStripProgressBar1);
                requestHandlerServiceControl.ConfigurationChanged += new ConfigurationChangedHandler(requestHandlerServiceControl_ConfigurationChanged);
            }
            this.panel2.Controls.Add(requestHandlerServiceControl);
        }

        private void buttonAria4XPSettings_Click(object sender, EventArgs e)
        {
            this.panel2.Controls.Clear();
            if (aria4XPSetupControl == null)
            {
                aria4XPSetupControl = new Aria4XPSetupControl(buttonAria4XPSettings, toolStripProgressBar1);
            }
            this.panel2.Controls.Add(aria4XPSetupControl);
        }

        private void buttonClients_Click(object sender, EventArgs e)
        {
            this.panel2.Controls.Clear();
            clientsControl = null;
            clientsControl = new ClientsControl(buttonClients, toolStripProgressBar1);
            this.panel2.Controls.Add(clientsControl);
        }
    }
}