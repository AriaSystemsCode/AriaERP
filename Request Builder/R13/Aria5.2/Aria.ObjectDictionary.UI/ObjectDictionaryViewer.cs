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

namespace Aria.ObjectDictionary.UI
{
    /// <summary>
    /// That is windows form used to browse object dictionary (object store in System.Master Database).
    /// </summary>
    public partial class ObjectDictionaryViewer : Form
    {
        //SAB 12-19-2013 Fix Dictionary Viewer Problem [Start]
        //AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
        AriaObjectDictionaryDBCentric objectDictionary;        
        //SAB 12-19-2013 Fix Dictionary Viewer Problem [End]

        ArrayList ariaObjects = new ArrayList();
        string clientId = "";
        
        public ObjectDictionaryViewer()
        {        
            InitializeComponent();

            treeView1.ImageList = imageList1;


            ariaObjects = objectDictionary.LoadAriaObjects(new AriaDbConnection("", ""), "");

            TreeNode rootNode = new TreeNode("Aria 5.0", 0, 0);
            rootNode.Nodes.Add(new TreeNode("Packages", 0, 0));//parent tree
            treeView1.Nodes.Add(rootNode);

            LoadAriaObjectChildren(rootNode.Nodes[0], 0);
        }

        //SAB 12-19-2013 Fix Dictionary Viewer Problem [Start]        
        public ObjectDictionaryViewer(string serverName, string serverPort)
        {
            Aria.Utilities.RemoteCall.AriaActivator activator = new Aria.Utilities.RemoteCall.AriaActivator();
            objectDictionary = (AriaObjectDictionaryDBCentric)activator.GetRemoteObject("Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric", serverName, int.Parse(serverPort));

            InitializeComponent();

            treeView1.ImageList = imageList1;


            ariaObjects = objectDictionary.LoadAriaObjects(new AriaDbConnection("", ""), "");            

            TreeNode rootNode = new TreeNode("Aria 5.0", 0, 0);
            rootNode.Nodes.Add(new TreeNode("Packages", 0, 0));//parent tree
            treeView1.Nodes.Add(rootNode);

            LoadAriaObjectChildren(rootNode.Nodes[0], 0);
        }
        //SAB 12-19-2013 Fix Dictionary Viewer Problem [End]

        private void loadAriaObjectProperties(AriaObject ariaObject, AriaObjectRevision ariaObjectRevision, TreeNode parentNode)
        {   //get properties to object
            //SAB 12-19-2013 Fix Dictionary Viewer Problem [Start]        
            //AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            //SAB 12-19-2013 Fix Dictionary Viewer Problem [End]        
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
            //SAB 12-19-2013 Fix Dictionary Viewer Problem [Start]
            //AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            //SAB 12-19-2013 Fix Dictionary Viewer Problem [End]
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
            //SAB 12-19-2013 Fix Dictionary Viewer Problem [Start]
            //AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            //SAB 12-19-2013 Fix Dictionary Viewer Problem [End]

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
            //SAB 12-19-2013 Fix Dictionary Viewer Problem [Start]
            //AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            //SAB 12-19-2013 Fix Dictionary Viewer Problem [End]

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
            //SAB 12-19-2013 Fix Dictionary Viewer Problem [Start]
            //AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            //SAB 12-19-2013 Fix Dictionary Viewer Problem [End]

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
        {//464 object from system.master AriaObjectRevision
            foreach (AriaObject ariaObject in ariaObjects)
            {
                if (!ariaObject.ParentObjectID.Equals(id))
                    continue;

                loadAriaObject(parentNode, ariaObject.ObjectID);
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
            if (id == 18566)
            {
                int x = 0;
            }

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

        private void propertyGrid1_Click(object sender, EventArgs e)
        {

        }
    }
}