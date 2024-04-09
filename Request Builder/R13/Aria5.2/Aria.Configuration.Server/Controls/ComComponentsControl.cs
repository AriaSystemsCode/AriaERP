using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Reflection;
using System.EnterpriseServices;
using System.Collections;

namespace Aria.Configuration.Server.Controls
{
    public partial class ComComponentsControl : UserControl
    {
        private static AriaConfigurationStatusTypes _status;
        public static AriaConfigurationStatusTypes Status
        {
            get { return _status; }
            set { _status = value; }
        }

        private ListViewItem _ownItem = null;

        private ToolStripProgressBar _progressBar;
            
        public ComComponentsControl(ListViewItem ownItem, ToolStripProgressBar progressBar)
        {
            InitializeComponent();

            Anchor = ((AnchorStyles)((((AnchorStyles.Top | AnchorStyles.Bottom) | AnchorStyles.Left) | AnchorStyles.Right)));

            _ownItem = ownItem;

            _progressBar = progressBar;

            toolStripComboBox1.ComboBox.SelectedIndex = 0;

            Init();
        }

        public void Init()
        {
            FillTable();
        }

        private ArrayList GetDllComponentTypes(string fullFileName)
        {
            try
            {
                Assembly assembly = Assembly.LoadFile(fullFileName);

                Type[] assemblyTypes = assembly.GetTypes();

                ArrayList dllComponentTypes = new ArrayList();

                foreach (Type type in assemblyTypes)
                {
                    if (typeof(ServicedComponent).IsAssignableFrom(type))
                    {
                        dllComponentTypes.Add(type);
                    }
                }

                return dllComponentTypes;
            }
            catch (Exception ex)
            {
                return null;
            }
        }

        private Dictionary<string, string> GetComponentInfo(Guid targetComponentGuid)
        {
            COMAdmin.COMAdminCatalog catalog = new COMAdmin.COMAdminCatalog();

            COMAdmin.COMAdminCatalogCollection applications = (COMAdmin.COMAdminCatalogCollection)catalog.GetCollection("Applications");

            try
            {
                applications.Populate();
            }
            catch (Exception e)
            {
                System.Windows.Forms.MessageBox.Show(e.InnerException.ToString());
            }

            Dictionary<string, string> componentInfoDictionary = new Dictionary<string, string>();

            foreach (COMAdmin.COMAdminCatalogObject application in applications)
            {
                if (application.Name.ToString() == ComGeneralSettingsControl.ApplicationName)
                {
                    COMAdmin.ICatalogCollection components = (COMAdmin.ICatalogCollection)applications.GetCollection("Components", application.Key);

                    components.Populate();

                    Guid componentGuid = Guid.Empty;

                    foreach (COMAdmin.ICatalogObject component in components)
                    {
                        componentGuid = new Guid(component.get_Value("CLSID").ToString());

                        if (componentGuid.Equals(targetComponentGuid))
                        {
                            componentInfoDictionary.Add("Installed", "Yes");
                            
                            componentInfoDictionary.Add("PoolSize", component.get_Value("MinPoolSize").ToString());

                            break;
                        }
                    }

                    break;
                }
            }

            return componentInfoDictionary;
        }

        private bool IsComponentInstalled(Guid targetComponentGuid)
        {
            COMAdmin.COMAdminCatalog catalog = new COMAdmin.COMAdminCatalog();

            COMAdmin.COMAdminCatalogCollection applications = (COMAdmin.COMAdminCatalogCollection)catalog.GetCollection("Applications");

            applications.Populate();

            foreach (COMAdmin.COMAdminCatalogObject application in applications)
            {
                if (application.Name.ToString() == ComGeneralSettingsControl.ApplicationName)
                {
                    COMAdmin.ICatalogCollection components = (COMAdmin.ICatalogCollection)applications.GetCollection("Components", application.Key);

                    components.Populate();

                    Guid componentGuid = Guid.Empty;

                    foreach (COMAdmin.ICatalogObject component in components)
                    {
                        componentGuid = new Guid(component.get_Value("CLSID").ToString());
                        
                        if (componentGuid.Equals(targetComponentGuid))
                        {
                            return true;
                        }
                    }

                    break;
                }
            }

            return false;
        }

        public void RefreshControl()
        {            
            FillTable();
        }

        public void SetStatus()
        {
            _status = AriaConfigurationStatusTypes.Configured;
                    
            for (int colIndex = 0; colIndex < listView1.Columns.Count; colIndex++)
            {
                if (listView1.Columns[colIndex].Text == "Installed")
                {
                    for (int itemIndex = 0; itemIndex < listView1.Items.Count; itemIndex++)
                    {
                        if (listView1.Items[itemIndex].SubItems.Count <= colIndex || listView1.Items[itemIndex].SubItems[colIndex].Text == "No")
                        {
                            _status = AriaConfigurationStatusTypes.NotConfigured;
                                                        
                            return;
                        }
                    }
                                        
                    break;
                }
            }

            if (_status == AriaConfigurationStatusTypes.NotConfigured ||
                    ComGeneralSettingsControl.Status == AriaConfigurationStatusTypes.NotConfigured)
            {
                _ownItem.ImageIndex = (int)AriaConfigurationStatusTypes.NotConfigured;
            }
            else
            {
                _ownItem.ImageIndex = (int)AriaConfigurationStatusTypes.Configured;                
            }
        }

        private void FillTable()
        {
            listView1.Items.Clear();

            string[] fileNames = Directory.GetFiles(Application.StartupPath + @"\GAC Assemblies\");

            for (int fileIndex = 0; fileIndex < fileNames.Length; fileIndex++)
            {
                if (!fileNames[fileIndex].EndsWith(".dll"))
                {
                    continue;
                }

                ArrayList dllComponentTypes = GetDllComponentTypes(fileNames[fileIndex]);

                if (dllComponentTypes != null)
                {
                    for (int index = 0; index < dllComponentTypes.Count; index++)
                    {
                        Dictionary<string, string> componentInfoDictionary = GetComponentInfo(((Type)dllComponentTypes[index]).GUID);

                        listView1.Items.Add(((Type)dllComponentTypes[index]).FullName);

                        if (componentInfoDictionary.Count > 0)
                        {
                            listView1.Items[listView1.Items.Count - 1].SubItems.Add("Yes");

                            listView1.Items[listView1.Items.Count - 1].SubItems.Add(componentInfoDictionary["PoolSize"]);
                        }
                        else
                        {
                            listView1.Items[listView1.Items.Count - 1].SubItems.Add("No");

                            listView1.Items[listView1.Items.Count - 1].SubItems.Add("");
                        }

                        listView1.Items[listView1.Items.Count - 1].SubItems.Add(((Type)dllComponentTypes[index]).GUID.ToString());

                        listView1.Items[listView1.Items.Count - 1].SubItems.Add(fileNames[fileIndex]);
                    }
                }
            }

            SetStatus();
        }

        private void RemoveComponent(Guid targetComponentGuid)
        {
            COMAdmin.COMAdminCatalog catalog = new COMAdmin.COMAdminCatalog();

            COMAdmin.COMAdminCatalogCollection applications = (COMAdmin.COMAdminCatalogCollection)catalog.GetCollection("Applications");

            applications.Populate();

            foreach (COMAdmin.COMAdminCatalogObject application in applications)
            {
                if (application.Name.ToString() == ComGeneralSettingsControl.ApplicationName)
                {
                    COMAdmin.ICatalogCollection components = (COMAdmin.ICatalogCollection)applications.GetCollection("Components", application.Key);

                    components.Populate();

                    Guid componentGuid = Guid.Empty;

                    COMAdmin.ICatalogObject component = null;

                    for (int index = 0; index < components.Count; index++)
                    {
                        component = (COMAdmin.ICatalogObject)components.get_Item(index);

                        componentGuid = new Guid(component.get_Value("CLSID").ToString());

                        if (componentGuid.Equals(targetComponentGuid))
                        {
                            components.Remove(index);

                            components.SaveChanges();

                            applications.SaveChanges();
                            
                            break;
                        }
                    }

                    break;
                }
            }
        }

        private void listView1_Leave(object sender, EventArgs e)
        {
            toolStripButton3.Enabled = false;

            toolStripButton4.Enabled = false;

            //toolStripTextBox1.Enabled = false;

            toolStripTextBox1.TextBox.Text = null;
        }

        private void toolStripButton1_Click(object sender, EventArgs e)
        {
            toolStripButton3.Enabled = false;

            toolStripButton4.Enabled = false;

            //toolStripTextBox1.Enabled = false;

            _progressBar.Value = 0;

            _progressBar.Maximum = listView1.Items.Count;

            _progressBar.Visible = true;
            
            COMAdmin.COMAdminCatalog catalog = new COMAdmin.COMAdminCatalog();

            ArrayList installedDlls = new ArrayList();

            for (int index = 0; index < listView1.Items.Count; index++)
            {
                _progressBar.Value++;

                if (!installedDlls.Contains(listView1.Items[index].SubItems[4].Text))
                {
                    installedDlls.Add(listView1.Items[index].SubItems[4].Text);

                    catalog.InstallComponent(ComGeneralSettingsControl.ApplicationName, listView1.Items[index].SubItems[4].Text, "", "");
                }

                //Set pool size
                if (toolStripComboBox1.ComboBox.SelectedItem.ToString() == "Yes")
                {
                    if (toolStripTextBox1.TextBox.Text == "" || toolStripTextBox1.TextBox.Text == null)
                    {
                        toolStripTextBox1.TextBox.Text = "0";
                    }

                    SetComponentMinPoolSize(ComGeneralSettingsControl.ApplicationId,
                                                        listView1.Items[index].SubItems[3].Text,
                                                            int.Parse(toolStripTextBox1.TextBox.Text));                    
                }
            }

            RefreshControl();

            _progressBar.Value = _progressBar.Maximum;

            _progressBar.Visible = false;

            toolStripTextBox1.TextBox.Text = null;            
        }

        private void toolStripButton2_Click(object sender, EventArgs e)
        {
            toolStripButton3.Enabled = false;

            toolStripButton4.Enabled = false;

            //toolStripTextBox1.Enabled = false;

            toolStripTextBox1.TextBox.Text = null;

            _progressBar.Value = 0;

            _progressBar.Maximum = listView1.Items.Count;

            _progressBar.Visible = true;
            
            for (int index = 0; index < listView1.Items.Count; index++)
            {
                _progressBar.Value++;

                RemoveComponent(new Guid(listView1.Items[index].SubItems[3].Text));
            }
            
            RefreshControl();

            _progressBar.Value = _progressBar.Maximum;

            _progressBar.Visible = false;
        }

        private void toolStripButton3_Click(object sender, EventArgs e)
        {
            toolStripButton3.Enabled = false;

            toolStripButton4.Enabled = false;

            //toolStripTextBox1.Enabled = false;

            _progressBar.Value = 0;

            _progressBar.Visible = true;

            ListViewItem item = listView1.SelectedItems[0];

            ArrayList relatedComponents = new ArrayList();

            _progressBar.Maximum = listView1.Items.Count;

            //Get all classes in the same dll which not installed
            for (int index = 0; index < listView1.Items.Count; index++)
            {
                _progressBar.Value++;

                if (listView1.Items[index].SubItems[4].Text.Equals(item.SubItems[4].Text) &&
                        !listView1.Items[index].SubItems[3].Text.Equals(item.SubItems[3].Text) &&
                            listView1.Items[index].SubItems[1].Text.Equals("No"))
                {
                    relatedComponents.Add(listView1.Items[index]);
                }
            }

            _progressBar.Maximum += relatedComponents.Count + 1;

            COMAdmin.COMAdminCatalog catalog = new COMAdmin.COMAdminCatalog();

            //Install the dll
            catalog.InstallComponent(ComGeneralSettingsControl.ApplicationName, item.SubItems[4].Text, "", "");
            
            //Set pool size
            if (toolStripComboBox1.ComboBox.SelectedItem.ToString() == "Yes")
            {
                if (toolStripTextBox1.TextBox.Text == "" || toolStripTextBox1.TextBox.Text == null)
                {
                    toolStripTextBox1.TextBox.Text = "0";
                }
                
                SetComponentMinPoolSize(ComGeneralSettingsControl.ApplicationId, item.SubItems[3].Text,
                                            int.Parse(toolStripTextBox1.TextBox.Text));

            }

            Guid guid = Guid.Empty;

            //Remove all related calsses
            for (int index = 0; index < relatedComponents.Count; index++)
            {
                _progressBar.Value++;

                guid = new Guid(((ListViewItem)relatedComponents[index]).SubItems[3].Text);

                RemoveComponent(guid);
            }

            RefreshControl();

            _progressBar.Value = _progressBar.Maximum;

            _progressBar.Visible = false;
            
            toolStripTextBox1.TextBox.Text = null;
        }

        private void SetComponentMinPoolSize(string applicationId, string componentCLSID, int poolSize)
        {
            COMAdmin.COMAdminCatalog catalog = new COMAdmin.COMAdminCatalog();

            COMAdmin.COMAdminCatalogCollection applications = (COMAdmin.COMAdminCatalogCollection)catalog.GetCollection("Applications");

            applications.Populate();

            COMAdmin.ICatalogCollection components = (COMAdmin.ICatalogCollection)applications.GetCollection("Components", applicationId);

            components.Populate();

            foreach (COMAdmin.ICatalogObject component in components)
            {
                if (component.get_Value("CLSID").ToString().ToLower() == "{" + componentCLSID.ToLower() + "}")
                {
                    component.set_Value("ObjectPoolingEnabled", true);
                    component.set_Value("MinPoolSize", poolSize);

                    components.SaveChanges();
                    applications.SaveChanges();

                    break;
                }
            }
        }

        private void toolStripButton4_Click(object sender, EventArgs e)
        {
            toolStripButton3.Enabled = false;

            toolStripButton4.Enabled = false;

            //toolStripTextBox1.Enabled = false;

            toolStripTextBox1.TextBox.Text = null;
            
            _progressBar.Value = 0;

            _progressBar.Maximum = 3;

            _progressBar.Visible = true;
            
            ListViewItem item = listView1.SelectedItems[0];

            _progressBar.Value++;

            Guid targetComponentGuid = new Guid(item.SubItems[3].Text);

            _progressBar.Value++;
            
            RemoveComponent(targetComponentGuid);
            
            RefreshControl();

            _progressBar.Value = _progressBar.Maximum;
            
            _progressBar.Visible = false;

            toolStripButton3.Enabled = false;
        }

        private void toolStripTextBox1_TextChanged(object sender, EventArgs e)
        {            
            ToolStripTextBox toolStripTextBox = (ToolStripTextBox)sender;

            int selectionStart = toolStripTextBox.SelectionStart;
            
            for (int index = 0; index < toolStripTextBox.Text.Length; index++)
            {
                if (toolStripTextBox.Text[index] < '0' ||
                        toolStripTextBox.Text[index] > '9')
                {
                    toolStripTextBox.Text = toolStripTextBox.Text.Remove(index, 1);
                    selectionStart--;
                }
            }

            toolStripTextBox.SelectionStart = selectionStart;
        }

        private void listView1_ItemSelectionChanged(object sender, ListViewItemSelectionChangedEventArgs e)
        {
            toolStripTextBox1.TextBox.Text = e.Item.SubItems[2].Text;
            
            toolStripButton3.Enabled = true;

            toolStripButton4.Enabled = true;

            //toolStripTextBox1.Enabled = true;
        }

        private void toolStripComboBox1_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (toolStripComboBox1.ComboBox.SelectedItem.ToString() == "Yes")
            {
                toolStripLabel1.Visible = true;
                toolStripTextBox1.Visible = true;
            }
            else
            {
                toolStripLabel1.Visible = false;
                toolStripTextBox1.Visible = false;            
            }
        }        
    }
}
