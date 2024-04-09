using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using System.Diagnostics;
using System.ServiceProcess;
using NetFwTypeLib;
using System.Management;
using System.Security.Principal;
using System.IO;
using System.Security.AccessControl;
using Aria.Configuration.Server.Properties;


namespace Aria.Configuration.Server.Controls
{
    public partial class RequestHandlerServiceControl : UserControl
    {
        public event ConfigurationChangedHandler ConfigurationChanged;

        private bool _init = false;
        private bool _userNamePassChanged = false;

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

        private string _instanceName = "";

        public RequestHandlerServiceControl(Button ownButton, ToolStripProgressBar progressBar)
        {
            InitializeComponent();

            Anchor = ((AnchorStyles)((((AnchorStyles.Top | AnchorStyles.Bottom) | AnchorStyles.Left) | AnchorStyles.Right)));

            _ownButton = ownButton;

            _progressBar = progressBar;


            Init();

            labelServiceName.Text = "AriaRequestHandler" + _instanceName;

            if (_instanceName == "")
            {
                labelDisplayName.Text = "Aria Request Handler";
            }
            else
            {
                labelDisplayName.Text = "Aria Request Handler" + " (" + _instanceName + ")";
            }
        }

        private void SetStatus()
        {
            _status = AriaConfigurationStatusTypes.Configured;

            errorProviderFirewall.Clear();
            errorProviderServiceStop.Clear();

            string objPath = string.Format("Win32_Service.Name='{0}'", "AriaRequestHandler" + _instanceName);
            using (ManagementObject service = new ManagementObject(new ManagementPath(objPath)))
            {
                labelState.Text = service.GetPropertyValue("State").ToString();
                ServiceController controller = new ServiceController("AriaRequestHandler" + _instanceName);
                //try
                //{
                //    //if (!SecurityUtilities.IsApplicationAuthorized(service.Properties["PathName"].Value.ToString()))
                //    MessageBox.Show("a1");
                //    FirewallUtility.AddFirewallException("Admin", new ushort[] { 1500 }, null, service.Properties["PathName"].Value.ToString());
                //    MessageBox.Show("a2");
                //    //{
                //    //    _status = AriaConfigurationStatusTypes.NotConfigured;
                //    //    errorProviderFirewall.SetError(buttonEnableFirewall, "Firewall prevents prequest service from calling remotely.");
                //    //    buttonEnableFirewall.Enabled = true;
                //    //}
                //    //else
                //    //{
                //    //    buttonEnableFirewall.Enabled = false;
                //    //}
                //}
                //catch(Exception ex)
                //{
                //    MessageBox.Show(ex.Message);

                //    //buttonEnableFirewall.Visible = false;
                //}
            }

            if (labelState.Text != "Running")
            {
                errorProviderServiceStop.SetError(buttonRun, "Service is not started.");
                buttonRun.Enabled = true;
                _status = AriaConfigurationStatusTypes.NotConfigured;
            }
            else
            {
                buttonRun.Enabled = false;
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

            if (ConfigurationChanged != null)
            {
                ConfigurationChanged();
            }
        }

        private void Init()
        {
            XmlDocument xmlInstanceDocument = new XmlDocument();

            xmlInstanceDocument.Load(Path.Combine(Application.StartupPath, "instance settings.xml"));
            for (int index = 0; index < xmlInstanceDocument.ChildNodes.Count; index++)
            {
                if (xmlInstanceDocument.ChildNodes[index].Name.Trim().ToUpper() == "InstanceName".ToUpper())
                {
                    _instanceName = xmlInstanceDocument.ChildNodes[index].InnerText;
                }
            }

            XmlDocument xmlDocument = new XmlDocument();

            //xmlDocument.Load(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));
            xmlDocument.Load(Path.Combine(Application.StartupPath, "configuration settings.xml"));
            

            XmlElement documentElement = xmlDocument.DocumentElement;

            for (int index = 0; index < documentElement.ChildNodes.Count; index++)
            {
                if (documentElement.ChildNodes[index].Name == "RequestServer")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "MaxRecordsPerAgent")
                        {
                            if (xmlNode.InnerText.Trim() == "")
                            {
                                numericUpDown1.Value = 0;
                            }
                            else
                            {
                                numericUpDown1.Value = Convert.ToInt32(xmlNode.InnerText);
                            }
                        }

                        // HES-Add the new limitation features on the UI 04/15/2014 {START}
                        if (xmlNode.Name == "MaxRecordsPerReport")
                        {
                            if (xmlNode.InnerText.Trim() == "")
                            {
                                udMaxRecPerReq.Value = 0;
                            }
                            else
                            {
                                udMaxRecPerReq.Value = Convert.ToInt32(xmlNode.InnerText);
                            }
                        }

                        if (xmlNode.Name == "MaxExecutionTimePerRequest")
                        {
                            if (xmlNode.InnerText.Trim() == "")
                            {
                                udMaxTime.Value = 0;
                            }
                            else
                            {
                                udMaxTime.Value = Convert.ToInt32(xmlNode.InnerText);
                            }
                        }
                        if (xmlNode.Name == "ExceedLimitNotificationEmail")
                        {
                            if (xmlNode.InnerText.Trim() == "")
                            {
                                txtExcNotfEmail.Text = "";
                            }
                            else
                            {
                                txtExcNotfEmail.Text = xmlNode.InnerText;
                            }
                        }
                        // HES-Add the new limitation features on the UI 04/15/2014 {END}
                    }
                }
            }

            string objPath = string.Format("Win32_Service.Name='{0}'", "AriaRequestHandler" + _instanceName);
            using (ManagementObject service = new ManagementObject(new ManagementPath(objPath)))
            {
                labelState.Text = service.GetPropertyValue("State").ToString();
                string value = service.GetPropertyValue("StartMode").ToString();

                if (value == "Auto")
                {
                    value = "Automatic";
                }

                comboBoxStartType.SelectedItem = value;
                if (service.GetPropertyValue("StartName").ToString().Trim() == "LocalSystem")
                {
                    comboBoxAccountType.SelectedIndex = 0;
                    textBoxUserName.Text = "";
                    textBoxPassword.Text = "".PadRight(15, '*');
                    textBoxUserName.Enabled = false;
                    textBoxPassword.Enabled = false;
                }
                else
                {
                    comboBoxAccountType.SelectedIndex = 1;
                    textBoxUserName.Text = service.GetPropertyValue("StartName").ToString();
                    textBoxPassword.Text = "".PadRight(15, '*');
                    textBoxUserName.Enabled = true;
                    textBoxPassword.Enabled = true;
                }
            }

            SetStatus();

            _init = true;
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
                textBoxUserName.Enabled = true;
                textBoxPassword.Enabled = true;
            }
            else
            {
                textBoxUserName.Enabled = false;
                textBoxPassword.Enabled = false;
            }

            if (_init)
            {
                buttonChangeAccount.Enabled = true;
                _userNamePassChanged = true;
            }
        }


        private void textBoxUserName_TextChanged(object sender, EventArgs e)
        {
            if (_init)
            {
                buttonChangeAccount.Enabled = true;
                _userNamePassChanged = true;
            }
        }

        private void textBoxPassword_TextChanged(object sender, EventArgs e)
        {
            if (_init)
            {
                buttonChangeAccount.Enabled = true;
                _userNamePassChanged = true;
            }
        }

        private void buttonEnableFirewall_Click(object sender, EventArgs e)
        {
            //string objPath = string.Format("Win32_Service.Name='{0}'", "Aria Request Handler ");
            //using (ManagementObject service = new ManagementObject(new ManagementPath(objPath)))
            //{
            //    ServiceController controller = new ServiceController("Aria Request Handler");
            //    try
            //    {
            //        if (!SecurityUtilities.IsApplicationAuthorized(service.Properties["PathName"].Value.ToString()))
            //        {
            //            SecurityUtilities.AuthorizeApplication("Aria Request Handler", service.Properties["PathName"].Value.ToString());
            //        }
            //    }
            //    catch (Exception)
            //    {
            //    }
            //}

            //SetStatus();
        }

        private void buttonRun_Click(object sender, EventArgs e)
        {
            try
            {
                ServiceController controller = new ServiceController("AriaRequestHandler" + _instanceName);
                if (controller.Status != ServiceControllerStatus.Running)
                {
                    controller.Start();
                    controller.WaitForStatus(ServiceControllerStatus.Running, new TimeSpan(0, 0, 60));
                }
            }
            catch (Exception)
            {

            }

            SetStatus();
        }

        private void buttonStart_Click(object sender, EventArgs e)
        {
            ServiceController controller = new ServiceController("AriaRequestHandler" + _instanceName);
            if (controller.Status != ServiceControllerStatus.Running)
            {
                controller.Start();
                controller.WaitForStatus(ServiceControllerStatus.Running, new TimeSpan(0, 0, 60));
            }

            SetStatus();
        }

        string[] UserGroups(string domain, string domainUserName)
        {

            WindowsIdentity ident = new WindowsIdentity(@"Administrator");
            List<string> groups = new List<string>();
            foreach (IdentityReference g in ident.Groups)
            {
                groups.Add(g.Value);
            }
            return groups.ToArray();
        }

        string[] AllowedAccounts(string filePath)
        {
            List<string> accounts = new List<string>();
            FileInfo fInfo = new FileInfo(filePath);
            var fsec = fInfo.GetAccessControl();
            AuthorizationRuleCollection acl = fsec.GetAccessRules(true, true, typeof(System.Security.Principal.NTAccount));
            foreach (FileSystemAccessRule ace in acl)
            {
                accounts.Add(ace.IdentityReference.Value);
            }
            return accounts.ToArray();
        }

        private void buttonStop_Click(object sender, EventArgs e)
        {
            ServiceController controller = new ServiceController("AriaRequestHandler" + _instanceName);
            if (controller.Status != ServiceControllerStatus.Stopped)
            {
                controller.Stop();
                controller.WaitForStatus(ServiceControllerStatus.Stopped, new TimeSpan(0, 0, 60));
            }

            SetStatus();
        }

        private void buttonChangeAccount_Click(object sender, EventArgs e)
        {
            string username = textBoxUserName.Text;

            if (comboBoxAccountType.SelectedIndex == 1 && _userNamePassChanged)
            {
                try
                {
                    if (!username.Contains("\\")) username = ".\\" + username;

                    using (new Impersonator(username.Split('\\')[1],
                                            username.Split('\\')[0],
                                            textBoxPassword.Text))
                    {

                    }
                }
                catch (Exception)
                {
                    MessageBox.Show("Invalid user name and password.", "Login", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    return;
                }
            }

            MessageBox.Show("The system will restart the service, please wait.", "Restart", MessageBoxButtons.OK, MessageBoxIcon.Information);

            errorProviderCannotChangeAccount.Clear();

            ServiceController controller = new ServiceController("AriaRequestHandler" + _instanceName);
            if (controller.Status != ServiceControllerStatus.Stopped)
            {
                controller.Stop();
                controller.WaitForStatus(ServiceControllerStatus.Stopped, new TimeSpan(0, 0, 60));
            }

            ManagementBaseObject inParams = null;

            ManagementObject srvc = new ManagementObject("Win32_Service.Name='" + "AriaRequestHandler"  + _instanceName + "'");

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
                    inParams["StartPassword"] = textBoxPassword.Text;
                }
            }

            ManagementBaseObject outParams = srvc.InvokeMethod("Change", inParams, null);

            //object w = System.Convert.ToInt32(outParams.Properties["ReturnValue"].Value);

            try
            {
                controller.Start();
                controller.WaitForStatus(ServiceControllerStatus.Running, new TimeSpan(0, 0, 60));

                buttonChangeAccount.Enabled = false;
                _userNamePassChanged = false;
            }
            catch (Exception ex)
            {
                MessageBox.Show(String.Format("Cannot start the service due to the following error '{0}'. \n Note: Login as service right must assign to selected user (Administrators do not have this right by default).", ex.GetBaseException().Message), "Login", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

            SetStatus();
        }

        //private void timerStatus_Tick(object sender, EventArgs e)
        //{
        //    SetStatus();
        //}

        private void numericUpDown1_ValueChanged(object sender, EventArgs e)
        {
            if (_init) buttonSave.Enabled = true;
        }

        private void buttonSave_Click(object sender, EventArgs e)
        {
            XmlDocument xmlDocument = new XmlDocument();

            //xmlDocument.Load(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));
            xmlDocument.Load(Path.Combine(Application.StartupPath, "configuration settings.xml"));

            XmlElement documentElement = xmlDocument.DocumentElement;

            for (int index = 0; index < documentElement.ChildNodes.Count; index++)
            {
                if (documentElement.ChildNodes[index].Name == "RequestServer")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "MaxRecordsPerAgent")
                        {
                            xmlNode.InnerText = numericUpDown1.Value.ToString();
                        }

                        // HES-Add the new limitation features on the UI 04/15/2014 {START}
                        if (xmlNode.Name == "MaxRecordsPerReport")
                        {
                            xmlNode.InnerText = udMaxRecPerReq.Value.ToString();
                        }

                        if (xmlNode.Name == "MaxExecutionTimePerRequest")
                        {
                            xmlNode.InnerText = udMaxTime.Value.ToString();
                        }

                        if (xmlNode.Name == "ExceedLimitNotificationEmail")
                        {
                            xmlNode.InnerText = txtExcNotfEmail.Text;
                        }
                        // HES-Add the new limitation features on the UI 04/15/2014 {END}
                    }
                }
            }

            //xmlDocument.Save(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));
            xmlDocument.Save(Path.Combine(Application.StartupPath, "configuration settings.xml"));

            buttonSave.Enabled = false;
        }

        private void label15_Click(object sender, EventArgs e)
        {

        }

        private void label2_Click(object sender, EventArgs e)
        {

        }

        private void label10_Click(object sender, EventArgs e)
        {

        }

        // HES-Add the new limitation features on the UI 04/15/2014 {START}
        private void udMaxTime_ValueChanged(object sender, EventArgs e)
        {
            if (_init) buttonSave.Enabled = true;
        }

        private void udMaxRecPerReq_ValueChanged(object sender, EventArgs e)
        {
            if (_init) buttonSave.Enabled = true;
        }

        private void txtExcNotfEmail_TextChanged(object sender, EventArgs e)
        {
            if (_init) buttonSave.Enabled = true;
        }

        private void RequestHandlerServiceControl_Load(object sender, EventArgs e)
        {

        }
        // HES-Add the new limitation features on the UI 04/15/2014 {END}
    }
}
