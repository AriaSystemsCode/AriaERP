using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Xml;

namespace Aria.Configuration.Server.Controls
{
    public partial class ComGeneralSettingsControl : UserControl
    {
        private static AriaConfigurationStatusTypes _status;
        public static AriaConfigurationStatusTypes Status
        {
            get { return _status; }
            set { _status = value; }
        }

        private static string _applicationName = "";
        public static string ApplicationName
        {
            get { return ComGeneralSettingsControl._applicationName; }
            set { ComGeneralSettingsControl._applicationName = value; }
        }

        private static string _applicationId = "";
        public static string ApplicationId
        {
            get { return ComGeneralSettingsControl._applicationId; }
            set { ComGeneralSettingsControl._applicationId = value; }
        }
        
        private ComActivationType _activationType;

        private ListViewItem _ownItem = null;

        private ToolStripProgressBar _progressBar;

        private Button _comComponentsButton = null;

        public ComGeneralSettingsControl(ListViewItem ownItem, ToolStripProgressBar progressBar, Button comComponentsButton)
        {
            InitializeComponent();

            Anchor = ((AnchorStyles)((((AnchorStyles.Top | AnchorStyles.Bottom) | AnchorStyles.Left) | AnchorStyles.Right)));

            _ownItem = ownItem;

            _progressBar = progressBar;

            _comComponentsButton = comComponentsButton;

            Init();
        }

        private void radioButton4_CheckedChanged(object sender, EventArgs e)
        {
            _activationType = ComActivationType.Library;
        }

        private void radioButton3_CheckedChanged(object sender, EventArgs e)
        {
            _activationType = ComActivationType.Server;
        }

        private void Init()
        {
            XmlDocument xmlDocument = new XmlDocument();

            xmlDocument.Load(Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));

            XmlElement documentElement = xmlDocument.DocumentElement;

            for (int index = 0; index < documentElement.ChildNodes.Count; index++)
            {
                if (documentElement.ChildNodes[index].Name == "ComSettings")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "ApplicationName")
                        {
                            _applicationName = xmlNode.InnerText;

                            label3.Text = _applicationName;
                        }
                    }

                    break;
                }
            }

            SetStatus();

            if (errorProvider1.GetError(label3) == "")
            {
                FillApplicationSettings(_applicationName);
            }
        }

        public void RefreshControl()
        {
            SetStatus();

            if (errorProvider1.GetError(label3) == "")
            {
                FillApplicationSettings(_applicationName);
            }
        }

        private void FillApplicationSettings(string applicationName)
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

                if (application.Name.ToString() == applicationName)
                {
                    textBox4.Text = application.get_Value("Description").ToString();

                    label2.Text = application.get_Value("ID").ToString();

                    _applicationId = label2.Text;
                    
                    _activationType = (ComActivationType)Enum.Parse(typeof(ComActivationType), application.get_Value("Activation").ToString());

                    if (_activationType == ComActivationType.Library)
                    {
                        radioButton4.Checked = true;
                    }
                    else if (_activationType == ComActivationType.Server)
                    {
                        radioButton3.Checked = true;
                    }

                    break;
                }
            }

            _progressBar.Value = _progressBar.Maximum;

            _progressBar.Visible = false;
        }

        private void SetStatus()
        {
            errorProvider1.Clear();

            if (!IsApplicationExists(label3.Text))
            {
                errorProvider1.SetError(label3, "The application name not found in the COM+ applications");

                _status = AriaConfigurationStatusTypes.NotConfigured;

                _comComponentsButton.Enabled = false;

                toolStripButton3.Enabled = true;

                toolStripButton1.Enabled = false;

                label2.Text = "";
            }
            else
            {                
                _status = AriaConfigurationStatusTypes.Configured;

                _comComponentsButton.Enabled = true;
                
                toolStripButton3.Enabled = false;

                toolStripButton1.Enabled = true;
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

        private void toolStripButton3_Click(object sender, EventArgs e)
        {
            CreateApplication();

            toolStripButton1.Enabled = true;
        }

        private void toolStripButton1_Click(object sender, EventArgs e)
        {
            UpdateApplication();
        }

        private void CreateApplication()
        {
            _progressBar.Value = 0;

            _progressBar.Visible = true;

            _progressBar.Maximum = 8;

            COMAdmin.COMAdminCatalog catalog = new COMAdmin.COMAdminCatalog();

            _progressBar.Value++;

            COMAdmin.COMAdminCatalogCollection applications = (COMAdmin.COMAdminCatalogCollection)catalog.GetCollection("Applications");

            _progressBar.Value++;
            
            applications.Populate();

            _progressBar.Value++;
            
            COMAdmin.COMAdminCatalogObject application = (COMAdmin.COMAdminCatalogObject)applications.Add();

            _progressBar.Value++;
            
            application.set_Value("Name", _applicationName);

            _progressBar.Value++;
            
            application.set_Value("Description", textBox4.Text);

            _progressBar.Value++;

            application.set_Value("Activation", _activationType);

            _progressBar.Value++;

            applications.SaveChanges();

            _progressBar.Value++;

            SetStatus();

            if (errorProvider1.GetError(label3) == "")
            {
                FillApplicationSettings(_applicationName);
            }

            _progressBar.Value = _progressBar.Maximum;

            _progressBar.Visible = false;
        }

        private void UpdateApplication()
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

                if (application.get_Value("Name").ToString() == _applicationName)
                {
                    application.set_Value("Description", textBox4.Text);

                    application.set_Value("Activation", _activationType);

                    applications.SaveChanges();

                    break;
                }                
            }

            SetStatus();

            if (errorProvider1.GetError(label3) == "")
            {
                FillApplicationSettings(_applicationName);
            }

            _progressBar.Value = _progressBar.Maximum;

            _progressBar.Visible = false;
        }
    }
}
