using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Aria.DataTypes.ObjectDictionary;
using Aria.EnterpriseServices.ObjectDictionary;
using System.Collections;
using Aria.Environment;
using Aria.Data;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Xml;
using Aria.DataTypes;
using Aria.DataTypes.Settings;


namespace Aria.Utilities.ObjectDictionary
{
    /// <summary>
    /// This is main windows to manage work with data dictionary
    /// </summary>
    public partial class ObjectDictionary : Form
    {
        public ObjectDictionary()
        {
            InitializeComponent();
            RefreshMenu();
        }
        private string clientId;
        private string _lastSelectObject = "";
        public string LastSelectObject
        {
            get { return _lastSelectObject; }
            set { _lastSelectObject = value; }
        }

        private void RefreshMenu()
        {
            revisionToolStripMenuItem.Enabled = false;
            propertyToolStripMenuItem.Enabled = false;
            methodToolStripMenuItem.Enabled = false;
            methodParameterToolStripMenuItem.Enabled = false;
            eventToolStripMenuItem.Enabled = false;
            eventParameterToolStripMenuItem.Enabled = false;

            saveToolStripMenuItem.Enabled = false;
            deleteToolStripMenuItem.Enabled = false;

            importTableFieldsToolStripMenuItem.Enabled = false;
            importTableRelatedFieldsToolStripMenuItem.Enabled = false;

            if (tvwTreeView.SelectedNode == null) return;

            if (tvwTreeView.SelectedNode.Tag == null &&
                tvwTreeView.SelectedNode.Text == "Properites")
            {
                propertyToolStripMenuItem.Enabled = true;

                importTableFieldsToolStripMenuItem.Enabled = true;
                importTableRelatedFieldsToolStripMenuItem.Enabled = true;
            }

            if (tvwTreeView.SelectedNode.Tag == null &&
                tvwTreeView.SelectedNode.Text == "Methods")
            {
                methodToolStripMenuItem.Enabled = true;
            }

            if (tvwTreeView.SelectedNode.Tag == null &&
                tvwTreeView.SelectedNode.Text == "Events")
            {
                eventToolStripMenuItem.Enabled = true;
            }

            if (tvwTreeView.SelectedNode.Tag is AriaObject)
            {
                revisionToolStripMenuItem.Enabled = true;
            }

            if (tvwTreeView.SelectedNode.Tag is AriaObjectMethod)
            {
                methodParameterToolStripMenuItem.Enabled = true;
            }

            if (tvwTreeView.SelectedNode.Tag is AriaObjectEvent)
            {
                eventParameterToolStripMenuItem.Enabled = true;
            }

            if (tvwTreeView.SelectedNode.Tag != null)
            {
                saveToolStripMenuItem.Enabled = true;
                deleteToolStripMenuItem.Enabled = true;
            }
        }
        
        private void LoadTree(string  clientId)
        {
            tvwTreeView.Nodes.Clear();

            if (LastSelectObject == "") return;

            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();
            ArrayList revisions = dictionary.LoadAriaObjectRevisions(new Aria.Data.AriaDbConnection("", ""), LastSelectObject,clientId);

            tvwTreeView.Nodes.Add(LastSelectObject, LastSelectObject);
            tvwTreeView.Nodes[LastSelectObject].Tag = dictionary.LoadAriaObjectByName(new Aria.Data.AriaDbConnection("", ""), LastSelectObject, clientId);
            
            for (int index = 0; index < revisions.Count; index++)
            {
                tvwTreeView.Nodes[LastSelectObject].Nodes.Add(((AriaObjectRevision)revisions[index]).ObjectRevision,
                                                              ((AriaObjectRevision)revisions[index]).ObjectRevision);

                tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Tag = ((AriaObjectRevision)revisions[index]);

                // Properties
                ArrayList properites = dictionary.LoadAriaObjectProperties(new Aria.Data.AriaDbConnection("", ""), LastSelectObject, ((AriaObjectRevision)revisions[index]).ObjectRevision, true, clientId);

                tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes.Add("Properites", 
                                                                                                                           "Properites");
                
                for (int innerIndex = 0; innerIndex < properites.Count; innerIndex++)
                {
                    tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes["Properites"].Nodes.Add(((AriaObjectProperty)properites[innerIndex]).PropertyName,
                                                                                                                                                   ((AriaObjectProperty)properites[innerIndex]).PropertyName);
                    tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes["Properites"].Nodes[((AriaObjectProperty)properites[innerIndex]).PropertyName].Tag = ((AriaObjectProperty)properites[innerIndex]);
                }

                // Methods
                ArrayList methods = dictionary.LoadAriaObjectMethods(new Aria.Data.AriaDbConnection("", ""), LastSelectObject, ((AriaObjectRevision)revisions[index]).ObjectRevision, true, clientId);

                tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes.Add("Methods",
                                                                                                                           "Methods");

                for (int innerIndex = 0; innerIndex < methods.Count; innerIndex++)
                {
                    tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes["Methods"].Nodes.Add(((AriaObjectMethod)methods[innerIndex]).MethodName,
                                                                                                                                                   ((AriaObjectMethod)methods[innerIndex]).MethodName);
                    tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes["Methods"].Nodes[((AriaObjectMethod)methods[innerIndex]).MethodName].Tag = ((AriaObjectMethod)methods[innerIndex]);
                    
                    // Method Parameters
                    ArrayList methodParameters = dictionary.LoadAriaObjectMethodParameters(new Aria.Data.AriaDbConnection("", ""), LastSelectObject, ((AriaObjectRevision)revisions[index]).ObjectRevision, ((AriaObjectMethod)methods[innerIndex]).MethodName, clientId);

                    for (int innerInnerIndex = 0; innerInnerIndex < methodParameters.Count; innerInnerIndex++)
                    {
                        tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes["Methods"].Nodes[((AriaObjectMethod)methods[innerIndex]).MethodName].Nodes.Add(((AriaObjectMethodParameter)methodParameters[innerInnerIndex]).ParameterName,
                                                                                                                                                       ((AriaObjectMethodParameter)methodParameters[innerInnerIndex]).ParameterName);
                        tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes["Methods"].Nodes[((AriaObjectMethod)methods[innerIndex]).MethodName].Nodes[((AriaObjectMethodParameter)methodParameters[innerInnerIndex]).ParameterName].Tag = ((AriaObjectMethodParameter)methodParameters[innerInnerIndex]);
                    }
                }

                // Events
                ArrayList events = dictionary.LoadAriaObjectEvents(new Aria.Data.AriaDbConnection("", ""), LastSelectObject, ((AriaObjectRevision)revisions[index]).ObjectRevision, true, clientId);

                tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes.Add("Events",
                                                                                                                           "Events");

                for (int innerIndex = 0; innerIndex < events.Count; innerIndex++)
                {
                    tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes["Events"].Nodes.Add(((AriaObjectEvent)events[innerIndex]).EventName,
                                                                                                                                                   ((AriaObjectEvent)events[innerIndex]).EventName);
                    
                    tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes["Events"].Nodes[((AriaObjectEvent)events[innerIndex]).EventName].Tag = ((AriaObjectEvent)events[innerIndex]);

                    // Event Parameters
                    ArrayList eventParameters = dictionary.LoadAriaObjectEventParameters(new Aria.Data.AriaDbConnection("", ""), LastSelectObject, ((AriaObjectRevision)revisions[index]).ObjectRevision, ((AriaObjectEvent)events[innerIndex]).EventName, clientId);

                    for (int innerInnerIndex = 0; innerInnerIndex < eventParameters.Count; innerInnerIndex++)
                    {
                        tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes["Events"].Nodes[((AriaObjectEvent)events[innerIndex]).EventName].Nodes.Add(((AriaObjectEventParameter)eventParameters[innerInnerIndex]).ParameterName,
                                                                                                                                                       ((AriaObjectEventParameter)eventParameters[innerInnerIndex]).ParameterName);

                        tvwTreeView.Nodes[LastSelectObject].Nodes[((AriaObjectRevision)revisions[index]).ObjectRevision].Nodes["Events"].Nodes[((AriaObjectEvent)events[innerIndex]).EventName].Nodes[((AriaObjectEventParameter)eventParameters[innerInnerIndex]).ParameterName].Tag = ((AriaObjectEventParameter)eventParameters[innerInnerIndex]);
                    }
                }
            }
        }

        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            SelectObject select = new SelectObject();
            if (select.ShowDialog() == DialogResult.OK)
            {
                LastSelectObject = select.cboObjects.Text;

                LoadTree(  clientId);
            }
        }

        private void tvwTreeView_AfterSelect(object sender, TreeViewEventArgs e)
        {
            RefreshMenu();

            if (tvwTreeView.SelectedNode.Tag == null) return;
            pgdPropertyGrid.SelectedObject = tvwTreeView.SelectedNode.Tag;
        }

        private void saveToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (tvwTreeView.SelectedNode.Tag == null) return;

            AriaDataProvider dataProvider = new AriaDataProvider();

            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            
            if(tvwTreeView.SelectedNode.Tag is AriaObject)
            {
                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObject", pgdPropertyGrid.SelectedObject, new string[] { "ObjectID", "ObjectType", "ParentObjectID", "ActiveRevision" }, "ObjectName = @ObjectName", clientId);
            }
            
            if(tvwTreeView.SelectedNode.Tag is AriaObjectRevision)
            {
                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObjectRevision", pgdPropertyGrid.SelectedObject, new string[] { "ObjectRevisionSettings" }, "ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision", clientId);
            }

            if (tvwTreeView.SelectedNode.Tag is AriaObjectProperty)
            {
                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObjectProperty", pgdPropertyGrid.SelectedObject, new string[] { "ModificationType", "PropertyType", "PropertySettings" }, "ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND PropertyName = @PropertyName", clientId);
            }

            if (tvwTreeView.SelectedNode.Tag is AriaObjectMethod)
            {
                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObjectMethod", pgdPropertyGrid.SelectedObject, new string[] { "ModificationType", "BusinessObjectParameterName" }, "ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND MethodName = @MethodName", clientId);
            }

            if (tvwTreeView.SelectedNode.Tag is AriaObjectMethodParameter)
            {
                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObjectMethodParameter", pgdPropertyGrid.SelectedObject, new string[] { "ParameterNo", "ParameterType", "ParameterSettings" }, "ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND MethodName = @MethodName AND ParameterName = @ParameterName", clientId);
            }

            if (tvwTreeView.SelectedNode.Tag is AriaObjectEvent)
            {
                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObjectEvent", pgdPropertyGrid.SelectedObject, new string[] { "ModificationType", "BusinessObjectParameterName" }, "ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND EventName = @EventName", clientId);
            }

            if (tvwTreeView.SelectedNode.Tag is AriaObjectEventParameter)
            {
                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObjectEventParameter", pgdPropertyGrid.SelectedObject, new string[] { "ParameterNo", "ParameterType", "ParameterSettings" }, "ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND MethodName = @MethodName AND ParameterName = @ParameterName", clientId);
            }
        }

        private void objectToolStripMenuItem_Click(object sender, EventArgs e)
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            NewObject newObject = new NewObject();
            newObject.ShowDialog();

            if (newObject.txtName.Text.Trim().Length > 0)
            {   
                dictionary.SaveAriaObject(new AriaDbConnection("Aria", ""), newObject.txtName.Text, (AriaObjectTypes)Enum.Parse(typeof(AriaObjectTypes), newObject.cboType.Text), clientId, newObject.txtName.Text);
                
                LastSelectObject = newObject.txtName.Text;
                LoadTree( clientId);
            }
        }

        private void revisionToolStripMenuItem_Click(object sender, EventArgs e)
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            AriaObject ariaObject = dictionary.LoadAriaObjectByName(new Aria.Data.AriaDbConnection("", ""), tvwTreeView.Nodes[0].Text, clientId);

            NewObjectRevision newObjectRevision = new NewObjectRevision();
            newObjectRevision.ShowDialog();

            if (newObjectRevision.txtRevision.Text.Trim().Length > 0)
            {
                string xmlString = "";
                AriaXmlSerializer xml = new AriaXmlSerializer();

                switch (ariaObject.ObjectType)
                {
                    case AriaObjectTypes.Data:
                        xmlString = xml.ConvertToXml(new AriaDataObjectSettings());
                        break;

                    case AriaObjectTypes.OptionGrid:
                        xmlString = xml.ConvertToXml(new AriaOptionGridSettings());
                        break;

                    case AriaObjectTypes.Package:
                        xmlString = xml.ConvertToXml(new AriaPackageSettings());
                        break;

                    case AriaObjectTypes.RelatedData:
                        xmlString = xml.ConvertToXml(new AriaRelatedDataObjectSettings());
                        break;

                    case AriaObjectTypes.Report:
                        xmlString = xml.ConvertToXml(new AriaReportObjectSettings());
                        break;

                    case AriaObjectTypes.Server:
                        xmlString = xml.ConvertToXml(new AriaServerObjectSettings());
                        break;
                }

                dictionary.SaveAriaObjectRevision(new AriaDbConnection("Aria", ""), ariaObject.ObjectID, newObjectRevision.txtRevision.Text, xmlString, clientId);

                LoadTree( clientId);
            }
        }

        private void propertyToolStripMenuItem_Click(object sender, EventArgs e)
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            AriaObject ariaObject = dictionary.LoadAriaObjectByName(new Aria.Data.AriaDbConnection("", ""), tvwTreeView.Nodes[0].Text, clientId);

            NewObjectProperty newObjectProperty = new NewObjectProperty();
            newObjectProperty.ShowDialog();

            if (newObjectProperty.txtName.Text.Trim().Length > 0)
            {

                string xmlString = "";
                AriaXmlSerializer xml = new AriaXmlSerializer();

                switch ((AriaDataTypes)Enum.Parse(typeof(AriaDataTypes), newObjectProperty.cboType.Text))
                {
                    case AriaDataTypes.AriaDataObjectPointer:
                        xmlString = xml.ConvertToXml(new Aria.DataTypes.Settings.AriaDataObjectPointerSettings());
                        break;

                    case AriaDataTypes.AriaDictionaryDefinedObject:
                        xmlString = xml.ConvertToXml(new Aria.DataTypes.Settings.AriaDictionaryDefinedObjectSettings());
                        break;

                    case AriaDataTypes.AriaOption:
                        xmlString = xml.ConvertToXml(new Aria.DataTypes.Settings.AriaOptionSettings());
                        break;

                    case AriaDataTypes.AriaOptionGridXmlDataSet:
                        xmlString = xml.ConvertToXml(new Aria.DataTypes.Settings.AriaOptionGridXmlDataSetSettings());
                        break;

                    case AriaDataTypes.AriaField:
                        xmlString = xml.ConvertToXml(new Aria.DataTypes.Settings.AriaFieldSettings());
                        break;

                    case AriaDataTypes.AriaRelatedField:
                        xmlString = xml.ConvertToXml(new Aria.DataTypes.Settings.AriaRelatedFieldSettings());
                        break;
                }

                dictionary.SaveAriaObjectProperty(new AriaDbConnection("Aria", ""), ariaObject.ObjectID, tvwTreeView.SelectedNode.Parent.Text, newObjectProperty.txtName.Text, AriaModificationTypes.Add, (AriaDataTypes)Enum.Parse(typeof(AriaDataTypes), newObjectProperty.cboType.Text), xmlString, clientId, newObjectProperty.txtName.Text);

                LoadTree( clientId);
            }
        }

        private void methodToolStripMenuItem_Click(object sender, EventArgs e)
        {
            NewObjectMethod newObjectMethod = new NewObjectMethod();
            newObjectMethod.ShowDialog();

            if (newObjectMethod.txtMethod.Text.Trim().Length > 0)
            {
                AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();
                dictionary.SaveAriaObjectMethod(new Aria.Data.AriaDbConnection("", ""), dictionary.LoadAriaObjectByName(new AriaDbConnection("Aria", ""), LastSelectObject, clientId).ObjectID,
                                                tvwTreeView.SelectedNode.Parent.Text, newObjectMethod.txtMethod.Text, AriaModificationTypes.Add, clientId);

                LoadTree( clientId);
            }
        }

        private void methodParameterToolStripMenuItem_Click(object sender, EventArgs e)
        {
            NewObjectMethodParameter newObjectMethodParameter = new NewObjectMethodParameter();
            newObjectMethodParameter.ShowDialog();

            if (newObjectMethodParameter.txtMethodParameter.Text.Trim().Length > 0)
            {
                AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

                Aria.Xml.AriaXmlSerializer xml = new AriaXmlSerializer();
                dictionary.SaveAriaObjectMethodParameter(new Aria.Data.AriaDbConnection("", ""), dictionary.LoadAriaObjectByName(new AriaDbConnection("Aria", ""), LastSelectObject, clientId).ObjectID,
                                                            tvwTreeView.SelectedNode.Parent.Parent.Text, tvwTreeView.SelectedNode.Text, 1, newObjectMethodParameter.txtMethodParameter.Text, AriaDataTypes.AriaOptionGridXmlDataSet, xml.ConvertToXml(new Aria.DataTypes.Settings.AriaOptionGridXmlDataSetSettings()), clientId);

                LoadTree( clientId);
            }
        }

        private void eventToolStripMenuItem_Click(object sender, EventArgs e)
        {
            NewObjectEvent newObjectEvent = new NewObjectEvent();
            newObjectEvent.ShowDialog();

            if (newObjectEvent.txtEvent.Text.Trim().Length > 0)
            {
                AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();
                dictionary.SaveAriaObjectEvent(new Aria.Data.AriaDbConnection("", ""), dictionary.LoadAriaObjectByName(new AriaDbConnection("Aria", ""), LastSelectObject, clientId).ObjectID,
                                                tvwTreeView.SelectedNode.Parent.Text, newObjectEvent.txtEvent.Text, AriaModificationTypes.Add, clientId, newObjectEvent.txtEvent.Text);

                LoadTree( clientId);
            }
        }

        private void eventParameterToolStripMenuItem_Click(object sender, EventArgs e)
        {
            NewObjectEventParameter newObjectEventParameter = new NewObjectEventParameter();
            newObjectEventParameter.ShowDialog();

            if (newObjectEventParameter.txtEventParameter.Text.Trim().Length > 0)
            {
                AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

                Aria.Xml.AriaXmlSerializer xml = new AriaXmlSerializer();
                dictionary.SaveAriaObjectEventParameter(new Aria.Data.AriaDbConnection("", ""), dictionary.LoadAriaObjectByName(new AriaDbConnection("Aria", ""), LastSelectObject, clientId).ObjectID,
                                                            tvwTreeView.SelectedNode.Parent.Parent.Text, tvwTreeView.SelectedNode.Text, 1, newObjectEventParameter.txtEventParameter.Text, AriaDataTypes.AriaDataObjectPointer, xml.ConvertToXml(new Aria.DataTypes.Settings.AriaDataObjectPointerSettings()), clientId);

                LoadTree( clientId);
            }
        }

        private void deleteToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (tvwTreeView.SelectedNode == null || tvwTreeView.SelectedNode.Tag == null) return;

            if (MessageBox.Show("Do you want to delete?", "Delete", MessageBoxButtons.YesNo) == DialogResult.Yes)
            {
                AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

                AriaDbCommand command = new AriaDbCommand("", new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, clientId);

                AriaObject ariaObject = dictionary.LoadAriaObjectByName(new Aria.Data.AriaDbConnection("", ""), tvwTreeView.Nodes[0].Text, clientId);

                if (tvwTreeView.SelectedNode.Tag is AriaObject)
                {
                    command.CommandText = "DELETE FROM AriaObject Where ObjectID = @ObjectID";
                    command.Parameters.Add("ObjectID", ariaObject.ObjectID);
                    command.ExecuteNonQuery();

                    command.CommandText = "DELETE FROM AriaObjectRevision Where ObjectID = @ObjectID";
                    command.ExecuteNonQuery();

                    command.CommandText = "DELETE FROM AriaObjectProperty Where ObjectID = @ObjectID";
                    command.ExecuteNonQuery();

                    command.CommandText = "DELETE FROM AriaObjectMethod Where ObjectID = @ObjectID";
                    command.ExecuteNonQuery();

                    command.CommandText = "DELETE FROM AriaObjectMethodParameter Where ObjectID = @ObjectID";
                    command.ExecuteNonQuery();

                    command.CommandText = "DELETE FROM AriaObjectEvent Where ObjectID = @ObjectID";
                    command.ExecuteNonQuery();

                    command.CommandText = "DELETE FROM AriaObjectEventParameter Where ObjectID = @ObjectID";
                    command.ExecuteNonQuery();

                    //LoadTree();
                    pgdPropertyGrid.SelectedObject = null;
                    tvwTreeView.Nodes.Clear();
                    return;
                }

                if (tvwTreeView.SelectedNode.Tag is AriaObjectRevision)
                {
                    command.CommandText = "DELETE FROM AriaObjectRevision Where ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision";
                    command.Parameters.Add("ObjectID", ariaObject.ObjectID);
                    command.Parameters.Add("ObjectRevision", ((AriaObjectRevision)tvwTreeView.SelectedNode.Tag).ObjectRevision);
                    command.ExecuteNonQuery();

                    command.CommandText = "DELETE FROM AriaObjectMethod Where ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision";
                    command.ExecuteNonQuery();

                    command.CommandText = "DELETE FROM AriaObjectMethodParameter Where ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision";
                    command.ExecuteNonQuery();

                    command.CommandText = "DELETE FROM AriaObjectEvent Where ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision";
                    command.ExecuteNonQuery();

                    command.CommandText = "DELETE FROM AriaObjectEventParameter Where ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision";
                    command.ExecuteNonQuery();

                    LoadTree(clientId);
                    pgdPropertyGrid.SelectedObject = null;
                    return;
                }

                if (tvwTreeView.SelectedNode.Tag is AriaObjectProperty)
                {
                    command.CommandText = "DELETE FROM AriaObjectProperty Where ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND PropertyName = @PropertyName";
                    command.Parameters.Add("ObjectID", ariaObject.ObjectID);
                    command.Parameters.Add("ObjectRevision", ((AriaObjectProperty)tvwTreeView.SelectedNode.Tag).ObjectRevision);
                    command.Parameters.Add("PropertyName", ((AriaObjectProperty)tvwTreeView.SelectedNode.Tag).PropertyName);
                    command.ExecuteNonQuery();

                    LoadTree( clientId);
                    pgdPropertyGrid.SelectedObject = null;
                    return;
                }

                if (tvwTreeView.SelectedNode.Tag is AriaObjectMethod)
                {
                    command.CommandText = "DELETE FROM AriaObjectMethod Where ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND MethodName = @MethodName";
                    command.Parameters.Add("ObjectID", ariaObject.ObjectID);
                    command.Parameters.Add("ObjectRevision", ((AriaObjectMethod)tvwTreeView.SelectedNode.Tag).ObjectRevision);
                    command.Parameters.Add("MethodName", ((AriaObjectMethod)tvwTreeView.SelectedNode.Tag).MethodName);
                    command.ExecuteNonQuery();

                    command.CommandText = "DELETE FROM AriaObjectMethodParameter Where ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND MethodName = @MethodName";
                    command.ExecuteNonQuery();

                    LoadTree(clientId);
                    pgdPropertyGrid.SelectedObject = null;
                    return;
                }

                if (tvwTreeView.SelectedNode.Tag is AriaObjectMethodParameter)
                {
                    command.CommandText = "DELETE FROM AriaObjectMethodParameter Where ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND MethodName = @MethodName AND ParameterName = @ParameterName";
                    command.Parameters.Add("ObjectID", ariaObject.ObjectID);
                    command.Parameters.Add("ObjectRevision", ((AriaObjectMethodParameter)tvwTreeView.SelectedNode.Tag).ObjectRevision);
                    command.Parameters.Add("MethodName", ((AriaObjectMethodParameter)tvwTreeView.SelectedNode.Tag).MethodName);
                    command.Parameters.Add("ParameterName", ((AriaObjectMethodParameter)tvwTreeView.SelectedNode.Tag).ParameterName);
                    command.ExecuteNonQuery();

                    LoadTree( clientId);
                    pgdPropertyGrid.SelectedObject = null;
                    return;
                }

                if (tvwTreeView.SelectedNode.Tag is AriaObjectEvent)
                {
                    command.CommandText = "DELETE FROM AriaObjectEvent Where ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND EventName = @EventName";
                    command.Parameters.Add("ObjectID", ariaObject.ObjectID);
                    command.Parameters.Add("ObjectRevision", ((AriaObjectEvent)tvwTreeView.SelectedNode.Tag).ObjectRevision);
                    command.Parameters.Add("EventName", ((AriaObjectEvent)tvwTreeView.SelectedNode.Tag).EventName);
                    command.ExecuteNonQuery();

                    command.CommandText = "DELETE FROM AriaObjectEventParameter Where ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND EventName = @EventName";
                    command.ExecuteNonQuery();

                    LoadTree( clientId);
                    pgdPropertyGrid.SelectedObject = null;
                    return;
                }

                if (tvwTreeView.SelectedNode.Tag is AriaObjectEventParameter)
                {
                    command.CommandText = "DELETE FROM AriaObjectEventParameter Where ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND EventName = @EventName AND ParameterName = @ParameterName";
                    command.Parameters.Add("ObjectID", ariaObject.ObjectID);
                    command.Parameters.Add("ObjectRevision", ((AriaObjectEventParameter)tvwTreeView.SelectedNode.Tag).ObjectRevision);
                    command.Parameters.Add("EventName", ((AriaObjectEventParameter)tvwTreeView.SelectedNode.Tag).EventName);
                    command.Parameters.Add("ParameterName", ((AriaObjectEventParameter)tvwTreeView.SelectedNode.Tag).ParameterName);
                    command.ExecuteNonQuery();

                    LoadTree( clientId);
                    pgdPropertyGrid.SelectedObject = null;
                    return;
                }
            }
        }

        private void importTableFieldsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ImportTableFields import = new ImportTableFields();
            if (import.ShowDialog() == DialogResult.OK)
            {
                string tableName = import.cboTables.Text.Substring(0, 8).Trim();

                AriaDbCommand command = new AriaDbCommand("SELECT SYDFIELD.*, sydflfld.nfld_pos FROM SYDFIELD join sydflfld on (SYDFIELD.cfld_name = sydflfld.cfld_name AND sydflfld.cFile_nam = '" + tableName + "') WHERE SYDFIELD.cfld_name IN (SELECT cfld_name FROM sydflfld WHERE cFile_nam = '" + tableName.PadRight(8) + "') ORDER BY nfld_pos", new AriaDbConnection("Aria", ""), import.chkAria4XP.Checked ? Aria.Environment.AriaDatabaseTypes.Aria40SystemFiles : Aria.Environment.AriaDatabaseTypes.Aria27SystemFiles, clientId);
                DataTable table = command.GetDataTable();

                for (int index = 0; index < table.Rows.Count; index++)
                {
                    DataRow row = table.Rows[index];

                    AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

                    AriaObject ariaObject = dictionary.LoadAriaObjectByName(new Aria.Data.AriaDbConnection("", ""), tvwTreeView.Nodes[0].Text, clientId);

                    string xmlString = "";

                    AriaFieldSettings fieldSettings = new AriaFieldSettings();
                    fieldSettings.Code = (string)row["mcodeinfo"];
                    if ((string)row["cdata_typ"] == "C") fieldSettings.DataType = AriaStandardDataTypes.String;
                    if ((string)row["cdata_typ"] == "N") fieldSettings.DataType = AriaStandardDataTypes.Numeric;
                    if ((string)row["cdata_typ"] == "D") fieldSettings.DataType = AriaStandardDataTypes.Date;
                    if ((string)row["cdata_typ"] == "L") fieldSettings.DataType = AriaStandardDataTypes.Logical;

                    fieldSettings.DecimalPlaces = (int)(Decimal)row["nfld_dec"];
                    fieldSettings.FieldName = (string)row["cfld_name"];
                    fieldSettings.HasReleatedField = (bool)row["lrltfields"];
                    fieldSettings.Head = (string)row["cfld_head"];
                    fieldSettings.IsReleatedField = (bool)row["lrelated"];
                    fieldSettings.Mask = (string)row["cpict_str"];
                    fieldSettings.Message = (string)row["cfld_msg"];
                    fieldSettings.ReleatedFields = ((string)row["mrltfields"]).Split('|');
                    if (((string)row["mventries"]).IndexOf("~") > 0) fieldSettings.ValidEntries = ((string)row["mventries"]).Substring(0, ((string)row["mventries"]).IndexOf("~")).Split('|');
                    fieldSettings.ValidEntry = (bool)row["lvldentry"];
                    fieldSettings.ValidExpression = (string)row["mvald_str"];
                    fieldSettings.Width = (int)(decimal)row["nfld_wdth"];

                    AriaXmlSerializer xml = new AriaXmlSerializer();
                    xmlString = xml.ConvertToXml(fieldSettings);

                    dictionary.SaveAriaObjectProperty(new AriaDbConnection("Aria", ""), ariaObject.ObjectID, tvwTreeView.SelectedNode.Parent.Text, fieldSettings.Head.Replace(" ", "").Replace(".", ""), AriaModificationTypes.Add, AriaDataTypes.AriaField, xmlString, clientId, fieldSettings.Head.Replace(" ", "").Replace(".", ""));
                }

                LoadTree(clientId);
            }
        }

        private void importTableRelatedFieldsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ImportTableFields import = new ImportTableFields();
            if (import.ShowDialog() == DialogResult.OK)
            {
                string tableName = import.cboTables.Text.Substring(0, 8).Trim();

                AriaDbCommand command = new AriaDbCommand("SELECT SYDFIELD.*, sydflfld.nfld_pos FROM SYDFIELD join sydflfld on (SYDFIELD.cfld_name = sydflfld.cfld_name AND sydflfld.cFile_nam = '" + tableName.PadRight(8) + "') WHERE SYDFIELD.cfld_name IN (SELECT cfld_name FROM sydflfld WHERE cFile_nam = '" + tableName + "') ORDER BY nfld_pos", new AriaDbConnection("Aria", ""), Aria.Environment.AriaDatabaseTypes.Aria27SystemFiles, clientId);
                DataTable table = command.GetDataTable();

                for (int index = 0; index < table.Rows.Count; index++)
                {
                    DataRow row = table.Rows[index];

                    AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

                    AriaObject ariaObject = dictionary.LoadAriaObjectByName(new Aria.Data.AriaDbConnection("", ""), tvwTreeView.Nodes[0].Text, clientId);

                    string xmlString = "";

                    AriaRelatedFieldSettings fieldSettings = new AriaRelatedFieldSettings();
                    fieldSettings.Code = (string)row["mcodeinfo"];
                    if ((string)row["cdata_typ"] == "C") fieldSettings.DataType = AriaStandardDataTypes.String;
                    if ((string)row["cdata_typ"] == "N") fieldSettings.DataType = AriaStandardDataTypes.Numeric;
                    if ((string)row["cdata_typ"] == "D") fieldSettings.DataType = AriaStandardDataTypes.Date;
                    if ((string)row["cdata_typ"] == "L") fieldSettings.DataType = AriaStandardDataTypes.Logical;

                    fieldSettings.DecimalPlaces = (int)(Decimal)row["nfld_dec"];
                    fieldSettings.FieldName = (string)row["cfld_name"];
                    fieldSettings.HasReleatedField = (bool)row["lrltfields"];
                    fieldSettings.Head = (string)row["cfld_head"];
                    fieldSettings.IsReleatedField = (bool)row["lrelated"];
                    fieldSettings.Mask = (string)row["cpict_str"];
                    fieldSettings.Message = (string)row["cfld_msg"];
                    fieldSettings.ReleatedFields = ((string)row["mrltfields"]).Split('|');
                    if (((string)row["mventries"]).IndexOf("~") > 0) fieldSettings.ValidEntries = ((string)row["mventries"]).Substring(0, ((string)row["mventries"]).IndexOf("~")).Split('|');
                    fieldSettings.ValidEntry = (bool)row["lvldentry"];
                    fieldSettings.ValidExpression = (string)row["mvald_str"];
                    fieldSettings.Width = (int)(decimal)row["nfld_wdth"];

                    AriaXmlSerializer xml = new AriaXmlSerializer();
                    xmlString = xml.ConvertToXml(fieldSettings);

                    dictionary.SaveAriaObjectProperty(new AriaDbConnection("Aria", ""), ariaObject.ObjectID, tvwTreeView.SelectedNode.Parent.Text, fieldSettings.Head.Replace(" ", "").Replace(".", ""), AriaModificationTypes.Add, AriaDataTypes.AriaRelatedField, xmlString, clientId,"");
                }

                LoadTree( clientId);
            }
        }

        private string RemoveSpecialChar(string value)
        {
            string result = "";
            for (int index = 0; index < value.Length; index++)
            {
                if (Char.IsLetterOrDigit(value.Substring(index, 1).ToCharArray()[0]))
                {
                    result += value.Substring(index, 1);
                }
            }

            return result;
        }

        private void importOptionGridToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ImportReport importOption = new ImportReport();
            if (importOption.ShowDialog() == DialogResult.OK)
            {
                string optionName = importOption.cboOptionGrids.Text.Substring(0, 8).Trim();

                AriaDbCommand commandReport = new AriaDbCommand("SELECT * FROM SYDREPRT WHERE crep_ID = '" + optionName.PadRight(8) + "'", new AriaDbConnection("Aria", ""), importOption.chkAria4XP.Checked ? Aria.Environment.AriaDatabaseTypes.Aria40SystemFiles : Aria.Environment.AriaDatabaseTypes.Aria27SystemFiles, clientId);
                DataTable tableReport = commandReport.GetDataTable();

                // Add Report Obeject
                string objectName = "Aria4XP." + RemoveSpecialChar(tableReport.Rows[0]["crep_name"].ToString());

                AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();
                dictionary.SaveAriaObject(new AriaDbConnection("Aria", ""), objectName, (AriaObjectTypes)Enum.Parse(typeof(AriaObjectTypes), "Report"), clientId,"");

                AriaObject ariaObject = dictionary.LoadAriaObjectByName(new AriaDbConnection("Aria", ""), objectName, clientId);
                ariaObject.ActiveRevision = "001.000";
                ariaObject.ParentObjectID = dictionary.LoadAriaObjectByName(new AriaDbConnection("Aria", ""), "Aria4XP", clientId).ObjectID;

                AriaDataProvider dataProvider = new AriaDataProvider();

                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObject", ariaObject, new string[] { "ObjectID", "ObjectType", "ParentObjectID", "ActiveRevision" }, "ObjectName = @ObjectName", clientId);

                // Add Report Obeject Revision
                string xmlString = "";
                AriaXmlSerializer xml = new AriaXmlSerializer();
                AriaReportObjectSettings settings = new AriaReportObjectSettings();
                settings.ClassName = tableReport.Rows[0]["crep_id"].ToString().TrimEnd() + "." + 
                                     tableReport.Rows[0]["crep_id"].ToString().TrimEnd();
                settings.ModificationType = AriaModificationTypes.Add;
                settings.SupportedFormats = new AriaOutputFormatTypes[] { AriaOutputFormatTypes.Excel, AriaOutputFormatTypes.Html, AriaOutputFormatTypes.Pdf, AriaOutputFormatTypes.Txt, AriaOutputFormatTypes.Xml };
                xmlString = xml.ConvertToXml(settings);

                dictionary.SaveAriaObjectRevision(new AriaDbConnection("Aria", ""), ariaObject.ObjectID, "001.000", xmlString, clientId);

                // Add method
                dictionary.SaveAriaObjectMethod(new Aria.Data.AriaDbConnection("", ""), ariaObject.ObjectID,
                                                        "001.000", "PrintReport", AriaModificationTypes.Add, clientId);
                AriaObjectMethod objectMethod = dictionary.LoadAriaObjectMethod(new Aria.Data.AriaDbConnection("", ""), ariaObject.ObjectName, "001.000", "PrintReport", clientId);
                objectMethod.BusinessObjectParameterName = "OptionGrid";
                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObjectMethod", objectMethod, new string[] { "ModificationType", "BusinessObjectParameterName" }, "ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND MethodName = @MethodName", clientId);

                // Add method Parameter
                AriaOptionGridXmlDataSetSettings methodSetting = new AriaOptionGridXmlDataSetSettings();
                methodSetting.OptionGridObjectName = ariaObject.ObjectName.TrimEnd() + ".OptionGrid";
                methodSetting.OptionGridRevision = "001.000";
                dictionary.SaveAriaObjectMethodParameter(new Aria.Data.AriaDbConnection("", ""), ariaObject.ObjectID,
                                                            "001.000", "PrintReport", 1, "OptionGrid", AriaDataTypes.AriaOptionGridXmlDataSet, xml.ConvertToXml(methodSetting), clientId);

                // Add Option Grid
                string optionGridObjectName = "Aria4XP." + RemoveSpecialChar(tableReport.Rows[0]["crep_name"].ToString().TrimEnd()) + ".OptionGrid";
                dictionary.SaveAriaObject(new AriaDbConnection("Aria", ""), optionGridObjectName, (AriaObjectTypes)Enum.Parse(typeof(AriaObjectTypes), "OptionGrid"), clientId,"");

                AriaObject ariaOptionGridObject = dictionary.LoadAriaObjectByName(new AriaDbConnection("Aria", ""), optionGridObjectName, clientId);
                ariaOptionGridObject.ActiveRevision = "001.000";
                ariaOptionGridObject.ParentObjectID = dictionary.LoadAriaObjectByName(new AriaDbConnection("Aria", ""), ariaObject.ObjectName, clientId).ObjectID;
                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObject", ariaOptionGridObject, new string[] { "ObjectID", "ObjectType", "ParentObjectID", "ActiveRevision" }, "ObjectName = @ObjectName", clientId);

                // Add Option Grid Revision
                string xmlOptionGridString = "";
                    AriaOptionGridSettings optionGridSettings = new AriaOptionGridSettings();
                optionGridSettings.OptionGridId = tableReport.Rows[0]["crep_id"].ToString().TrimEnd();
                optionGridSettings.ModificationType = AriaModificationTypes.Add;
                xmlOptionGridString = xml.ConvertToXml(optionGridSettings);

                dictionary.SaveAriaObjectRevision(new AriaDbConnection("Aria", ""), ariaOptionGridObject.ObjectID, "001.000", xmlOptionGridString, clientId);

                AriaDbCommand command = new AriaDbCommand("SELECT * FROM SYREPUVR WHERE crep_ID = '" + optionName.PadRight(8) + "' ORDER BY nVarPos", new AriaDbConnection("Aria", ""), importOption.chkAria4XP.Checked ? Aria.Environment.AriaDatabaseTypes.Aria40SystemFiles : Aria.Environment.AriaDatabaseTypes.Aria27SystemFiles, clientId);
                DataTable table = command.GetDataTable();

                for (int index = 0; index < table.Rows.Count; index++)
                {
                    DataRow row = table.Rows[index];

                    AriaObjectProperty property = new AriaObjectProperty();
                    property.ModificationType = AriaModificationTypes.Add;
                    property.ObjectID = ariaOptionGridObject.ObjectID;
                    property.ObjectRevision = "001.000";
                    property.PropertyName = ((string)row["cfld_head"]).Replace(" ", "");
                    property.PropertySettings = new Aria.DataTypes.Settings.AriaOptionSettings();

                    if (property.PropertyName.TrimEnd().Length > 0 && ((bool)row["laskrunt"] || ((string)row["mfld_name"]).Contains(".")))
                    {

                        ((AriaOptionSettings)property.PropertySettings).BrowseField = (string)row["cbrwselfld"];
                        ((AriaOptionSettings)property.PropertySettings).BrowseFields = (string)row["mbrwfields"];
                        ((AriaOptionSettings)property.PropertySettings).BrowseFilter = (string)row["mbrwfltexp"];
                        if (((string)row["cdefa_typ"]) == "V") ((AriaOptionSettings)property.PropertySettings).DefaultType = AriaOptionDefaultValueTypes.Fixed;
                        if (((string)row["cdefa_typ"]) == "E") ((AriaOptionSettings)property.PropertySettings).DefaultType = AriaOptionDefaultValueTypes.Expression;
                        ((AriaOptionSettings)property.PropertySettings).DefaultValue = (string)row["mdata_def"];
                        ((AriaOptionSettings)property.PropertySettings).Editor = (string)row["cassociate"];
                        ((AriaOptionSettings)property.PropertySettings).HideExpression = (string)row["msupexpr"];
                        ((AriaOptionSettings)property.PropertySettings).BrowseOpenFunction = (string)row["csetfunc"];
                        ((AriaOptionSettings)property.PropertySettings).BrowseSelectFunction = (string)row["csetfunc"];
                        ((AriaOptionSettings)property.PropertySettings).BrowseUnselectFunction = (string)row["csetfunc"];

                        ((AriaOptionSettings)property.PropertySettings).Code = (string)row["cCodes_fld"];
                        if ((string)row["cdata_typ"] == "C") ((AriaOptionSettings)property.PropertySettings).DataType = AriaStandardDataTypes.String;
                        if ((string)row["cdata_typ"] == "N") ((AriaOptionSettings)property.PropertySettings).DataType = AriaStandardDataTypes.Numeric;
                        if ((string)row["cdata_typ"] == "D") ((AriaOptionSettings)property.PropertySettings).DataType = AriaStandardDataTypes.Date;
                        if ((string)row["cdata_typ"] == "L") ((AriaOptionSettings)property.PropertySettings).DataType = AriaStandardDataTypes.Logical;

                        ((AriaOptionSettings)property.PropertySettings).DecimalPlaces = (int)(Decimal)row["nfld_dec"];
                        ((AriaOptionSettings)property.PropertySettings).Description = (string)row["mfld_des"];
                        ((AriaOptionSettings)property.PropertySettings).FieldName = (string)row["mfld_name"];
                        ((AriaOptionSettings)property.PropertySettings).Head = (string)row["cfld_head"];
                        ((AriaOptionSettings)property.PropertySettings).Mask = (string)row["cpict_str"];
                        ((AriaOptionSettings)property.PropertySettings).Message = (string)row["cfld_msg"];
                        if (((string)row["mventries"]).IndexOf("~") > 0) ((AriaOptionSettings)property.PropertySettings).ValidEntries = ((string)row["mventries"]).Substring(0, ((string)row["mventries"]).IndexOf("~")).Split('|');
                        ((AriaOptionSettings)property.PropertySettings).ValidEntry = (bool)row["lvldentry"];
                        ((AriaOptionSettings)property.PropertySettings).ValidExpression = (string)row["mvald_str"];
                        ((AriaOptionSettings)property.PropertySettings).Width = (int)(decimal)row["nfld_wdth"];

                        dictionary.SaveAriaObjectProperty(new AriaDbConnection("Aria", ""), property.ObjectID, property.ObjectRevision, property.PropertyName, AriaModificationTypes.Add, AriaDataTypes.AriaOption, xml.ConvertToXml(property.PropertySettings), clientId,"");
                    }
                }

                LoadTree( clientId);
            }
        }

        private void toolStripMenuItem1_Click(object sender, EventArgs e)
        {
            ImportReport importOption = new ImportReport();
            if (importOption.ShowDialog() == DialogResult.OK)
            {
                string optionName = importOption.cboOptionGrids.Text.Substring(0, 8).Trim();

                AriaDbCommand commandReport = new AriaDbCommand("SELECT * FROM SYDREPRT WHERE crep_ID = '" + optionName.PadRight(8) + "'", new AriaDbConnection("Aria", ""), importOption.chkAria4XP.Checked ? Aria.Environment.AriaDatabaseTypes.Aria40SystemFiles : Aria.Environment.AriaDatabaseTypes.Aria27SystemFiles, clientId);
                DataTable tableReport = commandReport.GetDataTable();

                // Add Report Obeject
                string objectName = "Aria4XP." + RemoveSpecialChar(tableReport.Rows[0]["crep_name"].ToString());

                AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();
                dictionary.SaveAriaObject(new AriaDbConnection("Aria", ""), objectName, (AriaObjectTypes)Enum.Parse(typeof(AriaObjectTypes), "Server"), clientId,"");

                AriaObject ariaObject = dictionary.LoadAriaObjectByName(new AriaDbConnection("Aria", ""), objectName, clientId);
                ariaObject.ActiveRevision = "001.000";
                ariaObject.ParentObjectID = dictionary.LoadAriaObjectByName(new AriaDbConnection("Aria", ""), "Aria4XP", clientId).ObjectID;

                AriaDataProvider dataProvider = new AriaDataProvider();

                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObject", ariaObject, new string[] { "ObjectID", "ObjectType", "ParentObjectID", "ActiveRevision" }, "ObjectName = @ObjectName", clientId);

                // Add Report Obeject Revision
                string xmlString = "";
                AriaXmlSerializer xml = new AriaXmlSerializer();
                AriaServerObjectSettings settings = new AriaServerObjectSettings();
                settings.ClassName = tableReport.Rows[0]["crep_id"].ToString().TrimEnd() + "." +
                                     tableReport.Rows[0]["crep_id"].ToString().TrimEnd();
                settings.ModificationType = AriaModificationTypes.Add;
                xmlString = xml.ConvertToXml(settings);

                dictionary.SaveAriaObjectRevision(new AriaDbConnection("Aria", ""), ariaObject.ObjectID, "001.000", xmlString, clientId);

                // Add method
                dictionary.SaveAriaObjectMethod(new Aria.Data.AriaDbConnection("", ""), ariaObject.ObjectID,
                                                        "001.000", "Execute", AriaModificationTypes.Add, clientId);
                AriaObjectMethod objectMethod = dictionary.LoadAriaObjectMethod(new Aria.Data.AriaDbConnection("", ""), ariaObject.ObjectName, "001.000", "Execute", clientId);
                objectMethod.BusinessObjectParameterName = "OptionGrid";
                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObjectMethod", objectMethod, new string[] { "ModificationType", "BusinessObjectParameterName" }, "ObjectID = @ObjectID AND ObjectRevision = @ObjectRevision AND MethodName = @MethodName", clientId);

                // Add method Parameter
                AriaOptionGridXmlDataSetSettings methodSetting = new AriaOptionGridXmlDataSetSettings();
                methodSetting.OptionGridObjectName = ariaObject.ObjectName.TrimEnd() + ".OptionGrid";
                methodSetting.OptionGridRevision = "001.000";
                dictionary.SaveAriaObjectMethodParameter(new Aria.Data.AriaDbConnection("", ""), ariaObject.ObjectID,
                                                            "001.000", "Execute", 1, "OptionGrid", AriaDataTypes.AriaOptionGridXmlDataSet, xml.ConvertToXml(methodSetting), clientId);

                // Add Option Grid
                string optionGridObjectName = "Aria4XP." + RemoveSpecialChar(tableReport.Rows[0]["crep_name"].ToString().TrimEnd()) + ".OptionGrid";
                dictionary.SaveAriaObject(new AriaDbConnection("Aria", ""), optionGridObjectName, (AriaObjectTypes)Enum.Parse(typeof(AriaObjectTypes), "OptionGrid"), clientId,"");

                AriaObject ariaOptionGridObject = dictionary.LoadAriaObjectByName(new AriaDbConnection("Aria", ""), optionGridObjectName, clientId);
                ariaOptionGridObject.ActiveRevision = "001.000";
                ariaOptionGridObject.ParentObjectID = dictionary.LoadAriaObjectByName(new AriaDbConnection("Aria", ""), ariaObject.ObjectName, clientId).ObjectID;
                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, "AriaObject", ariaOptionGridObject, new string[] { "ObjectID", "ObjectType", "ParentObjectID", "ActiveRevision" }, "ObjectName = @ObjectName", clientId);

                // Add Option Grid Revision
                string xmlOptionGridString = "";
                AriaOptionGridSettings optionGridSettings = new AriaOptionGridSettings();
                optionGridSettings.OptionGridId = tableReport.Rows[0]["crep_id"].ToString().TrimEnd();
                optionGridSettings.ModificationType = AriaModificationTypes.Add;
                xmlOptionGridString = xml.ConvertToXml(optionGridSettings);

                dictionary.SaveAriaObjectRevision(new AriaDbConnection("Aria", ""), ariaOptionGridObject.ObjectID, "001.000", xmlOptionGridString, clientId);

                AriaDbCommand command = new AriaDbCommand("SELECT * FROM SYREPUVR WHERE crep_ID = '" + optionName.PadRight(8) + "' ORDER BY nVarPos", new AriaDbConnection("Aria", ""), importOption.chkAria4XP.Checked ? Aria.Environment.AriaDatabaseTypes.Aria40SystemFiles : Aria.Environment.AriaDatabaseTypes.Aria27SystemFiles, clientId);
                DataTable table = command.GetDataTable();

                for (int index = 0; index < table.Rows.Count; index++)
                {
                    DataRow row = table.Rows[index];

                    AriaObjectProperty property = new AriaObjectProperty();
                    property.ModificationType = AriaModificationTypes.Add;
                    property.ObjectID = ariaOptionGridObject.ObjectID;
                    property.ObjectRevision = "001.000";
                    property.PropertyName = ((string)row["cfld_head"]).Replace(" ", "");
                    property.PropertySettings = new Aria.DataTypes.Settings.AriaOptionSettings();

                    if (property.PropertyName.TrimEnd().Length > 0 && ((bool)row["laskrunt"] || ((string)row["mfld_name"]).Contains(".")))
                    {

                        ((AriaOptionSettings)property.PropertySettings).BrowseField = (string)row["cbrwselfld"];
                        ((AriaOptionSettings)property.PropertySettings).BrowseFields = (string)row["mbrwfields"];
                        ((AriaOptionSettings)property.PropertySettings).BrowseFilter = (string)row["mbrwfltexp"];
                        if (((string)row["cdefa_typ"]) == "V") ((AriaOptionSettings)property.PropertySettings).DefaultType = AriaOptionDefaultValueTypes.Fixed;
                        if (((string)row["cdefa_typ"]) == "E") ((AriaOptionSettings)property.PropertySettings).DefaultType = AriaOptionDefaultValueTypes.Expression;
                        ((AriaOptionSettings)property.PropertySettings).DefaultValue = (string)row["mdata_def"];
                        ((AriaOptionSettings)property.PropertySettings).Editor = (string)row["cassociate"];
                        ((AriaOptionSettings)property.PropertySettings).HideExpression = (string)row["msupexpr"];
                        ((AriaOptionSettings)property.PropertySettings).BrowseOpenFunction = (string)row["csetfunc"];
                        ((AriaOptionSettings)property.PropertySettings).BrowseSelectFunction = (string)row["csetfunc"];
                        ((AriaOptionSettings)property.PropertySettings).BrowseUnselectFunction = (string)row["csetfunc"];

                        ((AriaOptionSettings)property.PropertySettings).Code = (string)row["cCodes_fld"];
                        if ((string)row["cdata_typ"] == "C") ((AriaOptionSettings)property.PropertySettings).DataType = AriaStandardDataTypes.String;
                        if ((string)row["cdata_typ"] == "N") ((AriaOptionSettings)property.PropertySettings).DataType = AriaStandardDataTypes.Numeric;
                        if ((string)row["cdata_typ"] == "D") ((AriaOptionSettings)property.PropertySettings).DataType = AriaStandardDataTypes.Date;
                        if ((string)row["cdata_typ"] == "L") ((AriaOptionSettings)property.PropertySettings).DataType = AriaStandardDataTypes.Logical;

                        ((AriaOptionSettings)property.PropertySettings).DecimalPlaces = (int)(Decimal)row["nfld_dec"];
                        ((AriaOptionSettings)property.PropertySettings).Description = (string)row["mfld_des"];
                        ((AriaOptionSettings)property.PropertySettings).FieldName = (string)row["mfld_name"];
                        ((AriaOptionSettings)property.PropertySettings).Head = (string)row["cfld_head"];
                        ((AriaOptionSettings)property.PropertySettings).Mask = (string)row["cpict_str"];
                        ((AriaOptionSettings)property.PropertySettings).Message = (string)row["cfld_msg"];
                        if (((string)row["mventries"]).IndexOf("~") > 0) ((AriaOptionSettings)property.PropertySettings).ValidEntries = ((string)row["mventries"]).Substring(0, ((string)row["mventries"]).IndexOf("~")).Split('|');
                        ((AriaOptionSettings)property.PropertySettings).ValidEntry = (bool)row["lvldentry"];
                        ((AriaOptionSettings)property.PropertySettings).ValidExpression = (string)row["mvald_str"];
                        ((AriaOptionSettings)property.PropertySettings).Width = (int)(decimal)row["nfld_wdth"];

                        dictionary.SaveAriaObjectProperty(new AriaDbConnection("Aria", ""), property.ObjectID, property.ObjectRevision, property.PropertyName, AriaModificationTypes.Add, AriaDataTypes.AriaOption, xml.ConvertToXml(property.PropertySettings), clientId, property.PropertyName);
                    }
                }

                LoadTree( clientId);
            }
        }
    }
}