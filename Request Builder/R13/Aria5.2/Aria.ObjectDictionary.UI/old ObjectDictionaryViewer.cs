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

namespace Aria.ObjectDictionary.UI
{
    /// <summary>
    /// That is windows form used to browse object dictionary (object store in System.Master Database).
    /// </summary>
    public partial class ObjectDictionaryViewer : Form
    {
        AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
        ArrayList ariaObjects = new ArrayList();
        private string clientId;
        public ObjectDictionaryViewer()
        {
            InitializeComponent();

            treeView1.ImageList = imageList1;

            ariaObjects = objectDictionary.LoadAriaObjects(new AriaDbConnection("", ""),clientId);

            TreeNode rootNode = new TreeNode("ObjectDictionary",0,0);
            rootNode.Nodes.Add(new TreeNode("Packages",0,0));
            treeView1.Nodes.Add(rootNode);
            loadAriaPackages(rootNode.Nodes[0]);
        }

        private void loadAriaPackages(TreeNode parent)
        {
            foreach (AriaObject ariaObject in ariaObjects)
            {
                if (ariaObject.ObjectType.Equals(AriaObjectTypes.Package))
                {
                    TreeNode node = new TreeNode(ariaObject.ObjectName, 6, 6);
                    node.Nodes.Add(new TreeNode("DataObjects", 0, 0));
                    parent.Nodes.Add(node);
                    node.Tag = ariaObject;

                    LoadAriaDataObjects(node.Nodes[0], ariaObject);
                }
            }
        }

        private void LoadAriaDataObjects(TreeNode parent, AriaObject parentObject)
        {
            foreach (AriaObject ariaObject in ariaObjects)
            {
                if (!ariaObject.ObjectType.Equals(AriaObjectTypes.Data))
                    continue;

                if (!ariaObject.ParentObjectID.Equals(parentObject.ObjectID))
                    continue;

                string name = ariaObject.ObjectName.Substring(ariaObject.ObjectName.LastIndexOf('.') + 1);
                TreeNode node = new TreeNode(name, 1, 1);
                node.Tag = ariaObject;
                parent.Nodes.Add(node);

                ArrayList revisions = objectDictionary.LoadAriaObjectRevisions(new AriaDbConnection("", ""), ariaObject.ObjectName,clientId);

                for (int index = 0; index < revisions.Count; index++)
                {
                    TreeNode revisionNode = new TreeNode(((AriaObjectRevision)revisions[index]).ObjectRevision, 1, 1);
                    revisionNode.Tag = revisions[index];
                    node.Nodes.Add(revisionNode);
                    loadAriaObjectProperties(ariaObject, (AriaObjectRevision)revisions[index], revisionNode);
                    loadAriaObjectEvents(ariaObject, revisionNode);
                    loadAriaObjectMethods(ariaObject, revisionNode);
                }
            }
        }

        private void LoadAriaDataObjectChildren(TreeNode parentNode, AriaObject parent)
        {
            foreach (AriaObject ariaObject in ariaObjects)
            {
                if (!ariaObject.ObjectType.Equals(AriaObjectTypes.Data))
                    continue;

                if (!ariaObject.ParentObjectID.Equals(parent.ObjectID))
                    continue;

                string name = ariaObject.ObjectName.Substring(ariaObject.ObjectName.LastIndexOf('.')+1);
                TreeNode node = new TreeNode(name,1,1);
                parentNode.Nodes.Add(node);
                node.Tag = ariaObject;
            }
        }

        private void loadAriaRelatedDataObjects(TreeNode parentNode, AriaObject parent)
        {
            foreach (AriaObject ariaObject in ariaObjects)
            {
                if (!ariaObject.ObjectType.Equals(AriaObjectTypes.RelatedData))
                    continue;

                if (!ariaObject.ParentObjectID.Equals(parent.ObjectID))
                    continue;

                string name = ariaObject.ObjectName.Substring(ariaObject.ObjectName.LastIndexOf('.')+1);
                TreeNode node = new TreeNode(name, 7, 7);
                parentNode.Nodes.Add(node);
                node.Tag = ariaObject;
            }
        }

        private void loadAriaObjectEvents(AriaObject ariaObject, TreeNode parentNode)
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            ArrayList objectEvents = objectDictionary.LoadAriaObjectEvents(new AriaDbConnection("", ""), ariaObject.ObjectName, ariaObject.ActiveRevision, false,clientId);

            foreach (AriaObjectEvent objectEvent in objectEvents)
            {
                TreeNode eventNode = new TreeNode(objectEvent.EventName,3,3);
                parentNode.Nodes.Add(eventNode);
                loadAriaObjectEventParameters(ariaObject, objectEvent,eventNode);
                eventNode.Tag = objectEvent;
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

        private void loadAriaObjectMethods(AriaObject ariaObject, TreeNode parentNode)
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            ArrayList objectMethods = objectDictionary.LoadAriaObjectMethods(new AriaDbConnection("", ""), ariaObject.ObjectName, ariaObject.ActiveRevision, false, clientId);

            foreach (AriaObjectMethod objectMethhod in objectMethods)
            {
                TreeNode methodNode = new TreeNode(objectMethhod.MethodName,4,4);
                parentNode.Nodes.Add(methodNode);
                loadAriaObjectMethodParameters(ariaObject, objectMethhod,methodNode);
                methodNode.Tag = objectMethhod;
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

        private void loadAriaObjectProperties(AriaObject ariaObject, AriaObjectRevision ariaObjectRevision, TreeNode parentNode)
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            ArrayList objectProperties = objectDictionary.LoadAriaObjectProperties(new AriaDbConnection("", ""), ariaObject.ObjectName, ariaObjectRevision.ObjectRevision, false, clientId);

            foreach (AriaObjectProperty property in objectProperties)
            {
                TreeNode node = new TreeNode(property.PropertyName,5,5);
                parentNode.Nodes.Add(node);
                node.Tag = property;
            }
        }

        private void treeView1_AfterSelect(object sender, TreeViewEventArgs e)
        {
            propertyGrid1.SelectedObject = e.Node.Tag;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            if( treeView1.SelectedNode == null )
                return;

            if (((TreeViewEventArgs)e).Node.Tag == null)
                return;

            //this.Enabled = false;

            //object selectedObject = ((TreeViewEventArgs)e).Node.Tag;

            //if (selectedObject.GetType().Equals(typeof(AriaObject)))
            //    objectDictionary.updateAriaObject(new AriaDbConnection("", ""), (AriaObject)selectedObject);

            //this.Enabled = true;
        }
    }
}