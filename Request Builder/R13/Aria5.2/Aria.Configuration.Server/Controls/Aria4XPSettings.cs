using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using Aria.Configuration.Server.Properties;
using System.IO;
using System.Runtime.InteropServices;
using System.ServiceProcess;

namespace Aria.Configuration.Server.Controls
{
    public partial class Aria4XPSettings : UserControl
    {
        [DllImport("mpr.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern int WNetGetConnection(
            [MarshalAs(UnmanagedType.LPTStr)] string localName,
            [MarshalAs(UnmanagedType.LPTStr)] StringBuilder remoteName,
            ref int length);

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

        private string _aria4XPSharedPath = "";
        public string Aria4XPSharedPath
        {
            get { return _aria4XPSharedPath; }
            set { _aria4XPSharedPath = value; }
        }

        public Aria4XPSettings(Button ownButton, ToolStripProgressBar progressBar)
        {
            InitializeComponent();

            this.Anchor = ((AnchorStyles)((((AnchorStyles.Top | AnchorStyles.Bottom) | AnchorStyles.Left) | AnchorStyles.Right)));

            _ownButton = ownButton;

            _progressBar = progressBar;

            Init();
        }

        public void Init()
        {
            XmlDocument xmlDocument = new XmlDocument();

            //xmlDocument.Load(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));
            xmlDocument.Load(Path.Combine(Application.StartupPath, "configuration settings.xml"));

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
                            _aria4XPSharedPath = xmlNode.InnerText;
                            textBoxAria4XPSharedPath.Text = _aria4XPSharedPath;
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

            errorProviderInvalidFolder.Clear();

            if (!Directory.Exists(textBoxAria4XPSharedPath.Text))
            {
                _status = AriaConfigurationStatusTypes.NotConfigured;
                errorProviderInvalidFolder.SetError(textBoxAria4XPSharedPath, "Invalid folder");
            }

            switch (_status)
            {
                case AriaConfigurationStatusTypes.Configured:
                    _ownButton.Image = Resources.Pass;
                    break;

                case AriaConfigurationStatusTypes.NotConfigured:
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

        public string GetAbsolutePath(string path)
        {
            string originalPath = path.TrimEnd();

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

                return originalPath;
            }
            catch (Exception)
            {
                return originalPath;
            }
        }

        private void buttonBrowse_Click(object sender, EventArgs e)
        {
            if (_init) buttonSave.Enabled = true;

            DialogResult dialogResult = this.folderBrowserDialogAria4XPSharedPath.ShowDialog();

            if (dialogResult == DialogResult.OK || dialogResult == DialogResult.Yes)
            {
                this.textBoxAria4XPSharedPath.Text = GetAbsolutePath(this.folderBrowserDialogAria4XPSharedPath.SelectedPath);

                if (textBoxAria4XPSharedPath.Text.Substring(textBoxAria4XPSharedPath.Text.Length - 1) != "\\")
                    textBoxAria4XPSharedPath.Text = textBoxAria4XPSharedPath.Text + "\\";
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

                //xmlDocument.Load(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));
                xmlDocument.Load(Path.Combine(Application.StartupPath, "configuration settings.xml"));

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
                                xmlNode.InnerText = textBoxAria4XPSharedPath.Text;
                            }
                        }
                    }
                }

                //xmlDocument.Save(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));
                xmlDocument.Save(Path.Combine(Application.StartupPath, "configuration settings.xml"));

                buttonSave.Enabled = false;

                //SAB 03/28/2013 Enable Restart service each time the user save settings [Start]
                MessageBox.Show("The system will restart the service, this process may take one or more minutes. Please wait.", "Restart", MessageBoxButtons.OK, MessageBoxIcon.Information);
                try
                {
                    string _instanceName = "";
                    XmlDocument xmlInstanceDocument = new XmlDocument();
                    xmlInstanceDocument.Load(Path.Combine(Application.StartupPath, "instance settings.xml"));
                    for (int index = 0; index < xmlInstanceDocument.ChildNodes.Count; index++)
                    {
                        if (xmlInstanceDocument.ChildNodes[index].Name.Trim().ToUpper() == "InstanceName".ToUpper())
                        {
                            _instanceName = xmlInstanceDocument.ChildNodes[index].InnerText;
                        }
                    }
                    ServiceController controller = new ServiceController("AriaRequestHandler" + _instanceName);
                    if (controller.Status == ServiceControllerStatus.Running)
                    {
                        controller.Stop();
                        controller.WaitForStatus(ServiceControllerStatus.Stopped, new TimeSpan(0, 0, 60));
                    }
                    controller.Start();
                    controller.WaitForStatus(ServiceControllerStatus.Running, new TimeSpan(0, 0, 60));
                }
                catch (Exception)
                {

                }
                //SAB 03/28/2013 Enable Restart service each time the user save settings [End]
            }
        }
        

        private void buttonSave_Click(object sender, EventArgs e)
        {
            Save();
        }
    }
}
