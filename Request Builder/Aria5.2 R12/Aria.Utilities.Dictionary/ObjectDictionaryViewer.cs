using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Collections;
using Aria.Data;
using Aria.Environment;
using Aria.DataTypes.ObjectDictionary;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.DataTypes.Settings;
using Aria.Xml;
using System.Data.SqlClient;


namespace Aria.Utilities.Dictionary
{
    /// <summary>
    /// That is windows form used to browse object dictionary (object store in System.Master Database).
    /// </summary>
    public partial class ObjectDictionaryViewer : Form
    {
        AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
        ArrayList ariaObjects = new ArrayList();
        string clientId = "";

        AriaEnviromentVariables EnviromentVariables = new AriaEnviromentVariables();

        private void FillCustomers()
        {
            DataTable Clients = new DataTable();

            SqlCommand clientsCom = new SqlCommand("SELECT * FROM CLIENTS ORDER BY CCLIENTNAME");
            clientsCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            clientsCom.Connection.Open();
            Clients.Load(clientsCom.ExecuteReader());
            clientsCom.Connection.Close();

            for (int i = 0; i < Clients.Rows.Count; i++)
            {
                ToolStripItem item = customerToolStripMenuItem.DropDown.Items.Add(Clients.Rows[i]["CCLIENTID"].ToString().TrimEnd());
                item.Click += new EventHandler(item_Click);
            }
        }

        void item_Click(object sender, EventArgs e)
        {
            ToolStripItem item = sender as ToolStripItem;
            clientId = item.Text;
            ReloadDictionary();
        }

        public ObjectDictionaryViewer()
        {
            InitializeComponent();

            treeView1.ImageList = imageList1;

            FillCustomers();
            
            ReloadDictionary();            
        }

        private void ReloadDictionary()
        {
            ariaObjects = objectDictionary.LoadAriaObjects(new AriaDbConnection("", ""), clientId);
            treeView1.Nodes.Clear();
            propertyGrid1.SelectedObject = null;
            TreeNode rootNode = new TreeNode("Aria 5.0", 0, 0);
            rootNode.Nodes.Add(new TreeNode("Packages", 0, 0));//parent tree
            treeView1.Nodes.Add(rootNode);
            LoadAriaObjectChildren(rootNode.Nodes[0], 0);
        }
        
        private void loadAriaObjectProperties(AriaObject ariaObject, AriaObjectRevision ariaObjectRevision, TreeNode parentNode)
        {   //get properties to object
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            ArrayList objectProperties = objectDictionary.LoadAriaObjectProperties(new AriaDbConnection("", ""), ariaObject.ObjectName, ariaObjectRevision.ObjectRevision, false, clientId);
            foreach (AriaObjectProperty property in objectProperties)
            {
                TreeNode node;
                if (property.PropertyType == Aria.DataTypes.AriaDataTypes.AriaField)
                {
                    if (((AriaFieldSettings)property.PropertySettings).IsPrimaryKey)
                    {
                        node = new TreeNode(property.PropertyName, 14, 14);
                        parentNode.Nodes.Add(node);
                        node.Tag = property;
                    }
                }
            }

            foreach (AriaObjectProperty property in objectProperties)
            {
                TreeNode node;
                if (property.PropertyType == Aria.DataTypes.AriaDataTypes.AriaField)
                {
                    if (!((AriaFieldSettings)property.PropertySettings).IsPrimaryKey)
                    {
                        node = new TreeNode(property.PropertyName, 5, 5);
                        parentNode.Nodes.Add(node);
                        node.Tag = property;
                    }
                }
                else
                {
                    node = new TreeNode(property.PropertyName, 8, 8);
                    parentNode.Nodes.Add(node);
                    node.Tag = property;
                }
            }
        }

        private void loadAriaObjectMethodParameters(AriaObject ariaObject, AriaObjectMethod objectMethhod, TreeNode parentNode)
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            ArrayList objectMethodParameters = objectDictionary.LoadAriaObjectMethodParameters(new AriaDbConnection("", ""), ariaObject.ObjectName, ariaObject.ActiveRevision, objectMethhod.MethodName, clientId);

            foreach (AriaObjectMethodParameter methodParameter in objectMethodParameters)
            {
                TreeNode node = new TreeNode(methodParameter.ParameterName, 2, 2);
                parentNode.Nodes.Add(node);
                node.Tag = methodParameter;
            }
        }

        private void loadAriaObjectMethods(AriaObject ariaObject, TreeNode parentNode)
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            ArrayList objectMethods = objectDictionary.LoadAriaObjectMethods(new AriaDbConnection("", ""), ariaObject.ObjectName, ariaObject.ActiveRevision, false, clientId);

            foreach (AriaObjectMethod objectMethhod in objectMethods)
            {
                TreeNode methodNode = new TreeNode(objectMethhod.MethodName, 4, 4);
                parentNode.Nodes.Add(methodNode);
                loadAriaObjectMethodParameters(ariaObject, objectMethhod, methodNode);
                methodNode.Tag = objectMethhod;
            }
        }

        private void loadAriaObjectEventParameters(AriaObject ariaObject, AriaObjectEvent objectEvent, TreeNode parentNode)
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            ArrayList objectEventParameters = objectDictionary.LoadAriaObjectEventParameters(new AriaDbConnection("", ""), ariaObject.ObjectName, ariaObject.ActiveRevision, objectEvent.EventName, clientId);

            foreach (AriaObjectEventParameter objectEventParameter in objectEventParameters)
            {
                TreeNode node = new TreeNode(objectEventParameter.ParameterName, 2, 2);
                parentNode.Nodes.Add(node);
                node.Tag = objectEventParameter;
            }
        }

        private void loadAriaObjectEvents(AriaObject ariaObject, TreeNode parentNode)
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            ArrayList objectEvents = objectDictionary.LoadAriaObjectEvents(new AriaDbConnection("", ""), ariaObject.ObjectName, ariaObject.ActiveRevision, false, clientId);

            foreach (AriaObjectEvent objectEvent in objectEvents)
            {
                TreeNode eventNode = new TreeNode(objectEvent.EventName, 3, 3);
                parentNode.Nodes.Add(eventNode);
                loadAriaObjectEventParameters(ariaObject, objectEvent, eventNode);
                eventNode.Tag = objectEvent;
            }
        }

        private void LoadAriaObjectChildren(TreeNode parentNode, int id)
        {
            
            foreach (AriaObject ariaObject in ariaObjects)
            {
                if (!ariaObject.ParentObjectID.Equals(id))
                    continue;

                try
                {
                    loadAriaObject(parentNode, ariaObject.ObjectID);
                }
                catch
                {
                }
            }
        }

        private void LoadAriaObjectChildren(TreeNode parentNode, int id, AriaObjectTypes objectType)
        {
            string nodeName;
            switch (objectType)
            {
                case AriaObjectTypes.Data:
                    nodeName = "DataObjects";
                    break;

                case AriaObjectTypes.RelatedData:
                    nodeName = "RelatedDataObjects";
                    break;

                case AriaObjectTypes.Report:
                    nodeName = "Reports";
                    break;

                case AriaObjectTypes.Server:
                    nodeName = "Servers";
                    break;

                case AriaObjectTypes.OptionGrid:
                    nodeName = "OptionGrids";
                    break;

                case AriaObjectTypes.Presenatation:
                    nodeName = "Presenatations";
                    break;

                case AriaObjectTypes.Package:
                    nodeName = "Packages";
                    break;

                case AriaObjectTypes.Framework:
                    nodeName = "Frameworks";
                    break;

                case AriaObjectTypes.Utility:
                    nodeName = "Utilities";
                    break;


                default:
                    throw new Exception("Unhandlled type");
            }

            bool exist = false;
            TreeNode childNode = new TreeNode(nodeName, 0, 0);

            foreach (AriaObject ariaObject in ariaObjects)
            {
                if (!ariaObject.ParentObjectID.Equals(id))
                    continue;

                if (ariaObject.ObjectType != objectType)
                    continue;

                if (!exist)
                {
                    exist = true;
                    parentNode.Nodes.Add(childNode);
                }

                loadAriaObject(childNode, ariaObject.ObjectID);
            }
        }

        private void loadAriaObject(TreeNode parent, int id)
        {//get type of object from xml 
            foreach (AriaObject ariaObject in ariaObjects)
            {
                if (!ariaObject.ObjectID.Equals(id))
                    continue;
                //get name of products aria27 aria4xp crm
                string name = ariaObject.ObjectName.Substring(ariaObject.ObjectName.LastIndexOf('.') + 1);

                TreeNode node;

                //put object in its place depend on name
                switch (ariaObject.ObjectType)
                {
                    case AriaObjectTypes.Data:
                        node = new TreeNode(name, 1, 1);
                        break;

                    case AriaObjectTypes.RelatedData:
                        node = new TreeNode(name, 7, 7);
                        break;

                    case AriaObjectTypes.Report:
                        node = new TreeNode(name, 9, 9);
                        break;

                    case AriaObjectTypes.Server:
                        node = new TreeNode(name, 11, 11);
                        break;

                    case AriaObjectTypes.OptionGrid:
                        node = new TreeNode(name, 10, 10);
                        break;

                    case AriaObjectTypes.Presenatation:
                        node = new TreeNode(name, 12, 12);
                        break;

                    case AriaObjectTypes.Package:
                        node = new TreeNode(name, 6, 6);
                        break;

                    default:
                        throw new Exception("Unhandlled type");
                }

                node.Tag = ariaObject;

                //add node
                parent.Nodes.Add(node);

                ArrayList revisions = objectDictionary.LoadAriaObjectRevisions(new AriaDbConnection("", ""), ariaObject.ObjectName, clientId);

                for (int index = 0; index < revisions.Count; index++)
                {
                    TreeNode revisionNode = new TreeNode(((AriaObjectRevision)revisions[index]).ObjectRevision, 13, 13);
                    revisionNode.Tag = revisions[index];
                    node.Nodes.Add(revisionNode);
                    loadAriaObjectProperties(ariaObject, (AriaObjectRevision)revisions[index], revisionNode);
                    loadAriaObjectEvents(ariaObject, revisionNode);
                    loadAriaObjectMethods(ariaObject, revisionNode);
                }

                LoadAriaObjectChildren(node, ariaObject.ObjectID, AriaObjectTypes.Package);
                LoadAriaObjectChildren(node, ariaObject.ObjectID, AriaObjectTypes.Data);
                LoadAriaObjectChildren(node, ariaObject.ObjectID, AriaObjectTypes.RelatedData);

                LoadAriaObjectChildren(node, ariaObject.ObjectID, AriaObjectTypes.Report);
                LoadAriaObjectChildren(node, ariaObject.ObjectID, AriaObjectTypes.Server);
                LoadAriaObjectChildren(node, ariaObject.ObjectID, AriaObjectTypes.OptionGrid);

                LoadAriaObjectChildren(node, ariaObject.ObjectID, AriaObjectTypes.Presenatation);

                LoadAriaObjectChildren(node, ariaObject.ObjectID, AriaObjectTypes.Framework);

                LoadAriaObjectChildren(node, ariaObject.ObjectID, AriaObjectTypes.Utility);
            }
        }

        private void treeView1_AfterSelect(object sender, TreeViewEventArgs e)
        {
            propertyGrid1.SelectedObject = e.Node.Tag;
        }

        public void GetAllNodes(TreeNode node, List<TreeNode> allNodes)
        {
            foreach (TreeNode childNode in node.Nodes)
            {
                if (childNode.Checked)
                {
                    allNodes.Add(childNode);
                }
                GetAllNodes(childNode, allNodes);
            }
        }

        private void generateFixToolStripMenuItem_Click(object sender, EventArgs e)
        {
            List<TreeNode> nodes = new List<TreeNode>();
            GetAllNodes(treeView1.Nodes[0], nodes);

            DataSet dataSet = new DataSet("Aria50 Dictionary");
            dataSet.Tables.Add(new DataTable("AriaObject"));
            dataSet.Tables["AriaObject"].Columns.Add("ParentObjectID$", typeof(string));
            dataSet.Tables["AriaObject"].Columns.Add("ObjectID!$#", typeof(string));
            dataSet.Tables["AriaObject"].Columns.Add("ObjectName", typeof(string));
            dataSet.Tables["AriaObject"].Columns.Add("ObjectDescription", typeof(string));
            dataSet.Tables["AriaObject"].Columns.Add("ObjectType", typeof(string));
            dataSet.Tables["AriaObject"].Columns.Add("ActiveRevision", typeof(string));

            dataSet.Tables.Add(new DataTable("AriaObjectRevision"));
            dataSet.Tables["AriaObjectRevision"].Columns.Add("ObjectID$#", typeof(string));
            dataSet.Tables["AriaObjectRevision"].Columns.Add("ObjectRevision#", typeof(string));
            dataSet.Tables["AriaObjectRevision"].Columns.Add("ObjectRevisionSettings", typeof(string));

            dataSet.Tables.Add(new DataTable("AriaObjectProperty"));
            dataSet.Tables["AriaObjectProperty"].Columns.Add("ObjectID$#", typeof(string));
            dataSet.Tables["AriaObjectProperty"].Columns.Add("ObjectRevision#", typeof(string));
            dataSet.Tables["AriaObjectProperty"].Columns.Add("PropertyName#", typeof(string));
            dataSet.Tables["AriaObjectProperty"].Columns.Add("PropertyDescription", typeof(string));
            dataSet.Tables["AriaObjectProperty"].Columns.Add("ModificationType", typeof(string));
            dataSet.Tables["AriaObjectProperty"].Columns.Add("PropertyType", typeof(string));
            dataSet.Tables["AriaObjectProperty"].Columns.Add("PropertySettings", typeof(string));

            dataSet.Tables.Add(new DataTable("AriaObjectMethod"));
            dataSet.Tables["AriaObjectMethod"].Columns.Add("ObjectID$#", typeof(string));
            dataSet.Tables["AriaObjectMethod"].Columns.Add("ObjectRevision#", typeof(string));
            dataSet.Tables["AriaObjectMethod"].Columns.Add("MethodName#", typeof(string));
            dataSet.Tables["AriaObjectMethod"].Columns.Add("ModificationType", typeof(string));
            dataSet.Tables["AriaObjectMethod"].Columns.Add("BusinessObjectParameterName", typeof(string));

            dataSet.Tables.Add(new DataTable("AriaObjectMethodParameter"));
            dataSet.Tables["AriaObjectMethodParameter"].Columns.Add("ObjectID$#", typeof(string));
            dataSet.Tables["AriaObjectMethodParameter"].Columns.Add("ObjectRevision#", typeof(string));
            dataSet.Tables["AriaObjectMethodParameter"].Columns.Add("MethodName#", typeof(string));
            dataSet.Tables["AriaObjectMethodParameter"].Columns.Add("ParameterNo#", typeof(string));
            dataSet.Tables["AriaObjectMethodParameter"].Columns.Add("ParameterName", typeof(string));
            dataSet.Tables["AriaObjectMethodParameter"].Columns.Add("ParameterType", typeof(string));
            dataSet.Tables["AriaObjectMethodParameter"].Columns.Add("ParameterSettings", typeof(string));

            dataSet.Tables.Add(new DataTable("AriaObjectEvent"));
            dataSet.Tables["AriaObjectEvent"].Columns.Add("ObjectID$#", typeof(string));
            dataSet.Tables["AriaObjectEvent"].Columns.Add("ObjectRevision#", typeof(string));
            dataSet.Tables["AriaObjectEvent"].Columns.Add("EventName#", typeof(string));
            dataSet.Tables["AriaObjectEvent"].Columns.Add("EventDescription#", typeof(string));
            dataSet.Tables["AriaObjectEvent"].Columns.Add("ModificationType", typeof(string));
            dataSet.Tables["AriaObjectEvent"].Columns.Add("BusinessObjectParameterName", typeof(string));

            dataSet.Tables.Add(new DataTable("AriaObjectEventParameter"));
            dataSet.Tables["AriaObjectEventParameter"].Columns.Add("ObjectID$#", typeof(string));
            dataSet.Tables["AriaObjectEventParameter"].Columns.Add("ObjectRevision#", typeof(string));
            dataSet.Tables["AriaObjectEventParameter"].Columns.Add("EventName#", typeof(string));
            dataSet.Tables["AriaObjectEventParameter"].Columns.Add("ParameterNo#", typeof(string));
            dataSet.Tables["AriaObjectEventParameter"].Columns.Add("ParameterName", typeof(string));
            dataSet.Tables["AriaObjectEventParameter"].Columns.Add("ParameterType", typeof(string));
            dataSet.Tables["AriaObjectEventParameter"].Columns.Add("ParameterSettings", typeof(string));

            AriaXmlSerializer ser = new AriaXmlSerializer();

            foreach (TreeNode node in nodes)
            {
                if (node.Tag is AriaObject)
                {
                    AriaObject newObject = node.Tag as AriaObject;
                    AriaObject parentObject = node.Parent.Parent.Tag as AriaObject;

                    string parentObjectID = "SELECT ObjectID FROM AriaObject WHERE ObjectName = '" + parentObject.ObjectName + "'";
                    string objectID = "SELECT ObjectID FROM AriaObject WHERE ObjectName = '" + newObject.ObjectName + "'";

                    dataSet.Tables["AriaObject"].Rows.Add(parentObjectID, objectID, newObject.ObjectName, newObject.ObjectDescription, newObject.ObjectType, newObject.ActiveRevision);
                }

                if (node.Tag is AriaObjectRevision)
                {
                    AriaObjectRevision newObjectRevision = node.Tag as AriaObjectRevision;
                    AriaObject parentObject = node.Parent.Tag as AriaObject;

                    string objectID = "SELECT ObjectID FROM AriaObject WHERE ObjectName = '" + parentObject.ObjectName + "'";

                    dataSet.Tables["AriaObjectRevision"].Rows.Add(objectID, newObjectRevision.ObjectRevision, ser.ConvertToXml(newObjectRevision.ObjectRevisionSettings));
                }

                if (node.Tag is AriaObjectProperty)
                {
                    AriaObjectProperty newObjectProperty = node.Tag as AriaObjectProperty;
                    AriaObject parentObject = node.Parent.Parent.Tag as AriaObject;

                    string objectID = "SELECT ObjectID FROM AriaObject WHERE ObjectName = '" + parentObject.ObjectName + "'";

                    dataSet.Tables["AriaObjectProperty"].Rows.Add(objectID, newObjectProperty.ObjectRevision, newObjectProperty.PropertyName, newObjectProperty.PropertyDescription, newObjectProperty.ModificationType, newObjectProperty.PropertyType, ser.ConvertToXml(newObjectProperty.PropertySettings));
                }

                if (node.Tag is AriaObjectMethod)
                {
                    AriaObjectMethod newObjectMethod = node.Tag as AriaObjectMethod;
                    AriaObject parentObject = node.Parent.Parent.Tag as AriaObject;

                    string objectID = "SELECT ObjectID FROM AriaObject WHERE ObjectName = '" + parentObject.ObjectName + "'";

                    dataSet.Tables["AriaObjectMethod"].Rows.Add(objectID, newObjectMethod.ObjectRevision, newObjectMethod.MethodName, newObjectMethod.ModificationType, newObjectMethod.BusinessObjectParameterName);

                }

                if (node.Tag is AriaObjectMethodParameter)
                {
                    AriaObjectMethodParameter newObjectMethodParameter = node.Tag as AriaObjectMethodParameter;
                    AriaObject parentObject = node.Parent.Parent.Parent.Tag as AriaObject;

                    string objectID = "SELECT ObjectID FROM AriaObject WHERE ObjectName = '" + parentObject.ObjectName + "'";

                    dataSet.Tables["AriaObjectMethodParameter"].Rows.Add(objectID, newObjectMethodParameter.ObjectRevision, newObjectMethodParameter.MethodName, newObjectMethodParameter.ParameterNo, newObjectMethodParameter.ParameterName, newObjectMethodParameter.ParameterType, ser.ConvertToXml(newObjectMethodParameter.ParameterSettings));
                }

                if (node.Tag is AriaObjectEvent)
                {
                    AriaObjectEvent newObjectEvent = node.Tag as AriaObjectEvent;
                    AriaObject parentObject = node.Parent.Parent.Tag as AriaObject;

                    string objectID = "SELECT ObjectID FROM AriaObject WHERE ObjectName = '" + parentObject.ObjectName + "'";

                    dataSet.Tables["AriaObjectEvent"].Rows.Add(objectID, newObjectEvent.ObjectRevision, newObjectEvent.EventName, newObjectEvent.EventDescription, newObjectEvent.ModificationType, newObjectEvent.BusinessObjectParameterName);
                }

                if (node.Tag is AriaObjectEventParameter)
                {
                    AriaObjectEventParameter newObjectEventParameter = node.Tag as AriaObjectEventParameter;
                    AriaObject parentObject = node.Parent.Parent.Parent.Tag as AriaObject;

                    string objectID = "SELECT ObjectID FROM AriaObject WHERE ObjectName = '" + parentObject.ObjectName + "'";

                    dataSet.Tables["AriaObjectEventParameter"].Rows.Add(objectID, newObjectEventParameter.ObjectRevision, newObjectEventParameter.EventName, newObjectEventParameter.ParameterNo, newObjectEventParameter.ParameterName, newObjectEventParameter.ParameterType, ser.ConvertToXml(newObjectEventParameter.ParameterSettings));
                }
            }

            SaveFileDialog dialog = new SaveFileDialog();
            if (dialog.ShowDialog() == DialogResult.OK)
            {
                dataSet.WriteXml(dialog.FileName);
            }
        }

        private void addStandardDataToolStripMenuItem_Click(object sender, EventArgs e)
        {
            FormAddFileToDictionary form = new FormAddFileToDictionary();
            if (form.ShowDialog() == DialogResult.OK)
            {
                ReloadDictionary();
            }
        }

        private void addStandardFieldToolStripMenuItem_Click(object sender, EventArgs e)
        {
            FormAddFieldToDictionary form = new FormAddFieldToDictionary();
            if (form.ShowDialog() == DialogResult.OK)
            {
                ReloadDictionary();
            }
        }

        private void addRelatedDataToolStripMenuItem_Click(object sender, EventArgs e)
        {
            FormAddRelatedFileToDictionary form = new FormAddRelatedFileToDictionary();
            if (form.ShowDialog() == DialogResult.OK)
            {
                ReloadDictionary();
            }
        }

        private void addStandardReportToolStripMenuItem_Click(object sender, EventArgs e)
        {
            FormAddReportToDictionary form = new FormAddReportToDictionary();
            if (form.ShowDialog() == DialogResult.OK)
            {
                ReloadDictionary();
            }
        }

        private void addCustomReportToolStripMenuItem_Click(object sender, EventArgs e)
        {
            FormAddCustomReportToDictionary form = new FormAddCustomReportToDictionary();
            if (form.ShowDialog() == DialogResult.OK)
            {
                ReloadDictionary();
            }

        }

        private void systemToolStripMenuItem_Click(object sender, EventArgs e)
        {
            clientId = "";
            ReloadDictionary();
        }

        private void addStandardProgramToolStripMenuItem_Click(object sender, EventArgs e)
        {
            FormAddProgramToDictionary form = new FormAddProgramToDictionary();
            if (form.ShowDialog() == DialogResult.OK)
            {
                ReloadDictionary();
            }
        }

        //SAB 12-29-2013 Create new version of the dictionary tool for the customers [Start]
        private void ObjectDictionaryViewer_Load(object sender, EventArgs e)
        {
            string Aria_Env = System.Environment.GetEnvironmentVariable("ARIA_ENVIRONMENT", EnvironmentVariableTarget.Machine);
            if (Aria_Env != "AriaInHouse")
            {
                foreach (ToolStripItem mnu in fixToolStripMenuItem.DropDownItems)
                {
                    if (mnu.Name == "addCustomReportToolStripMenuItem")
                    {
                        mnu.Visible = true;
                    }
                    else
                    {
                        mnu.Visible = false;
                    }
                }
            }

        }
        //SAB 12-29-2013 Create new version of the dictionary tool for the customers [End]
    }
}