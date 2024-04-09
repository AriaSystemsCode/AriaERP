using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Aria.Environment;
using System.Data.SqlClient;
using System.Data.Odbc;
using Aria.Xml;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.DataTypes.ObjectDictionary;
using Aria.DataTypes.Settings;
using Aria.DataTypes;


namespace Aria.Utilities.Dictionary
{
    public partial class FormAddRelatedFileToDictionary : Form
    {
        public FormAddRelatedFileToDictionary()
        {
            InitializeComponent();
        }

        const string ActiveRevision = "000001";

        AriaXmlSerializer Serializer = new AriaXmlSerializer(); 
        
        AriaEnviromentVariables EnviromentVariables = new AriaEnviromentVariables();

        DataTable Clients;
        DataTable Objects;
        DataTable RelatedObjects;

        private void FillCustomers()
        {
            Clients = new DataTable();

            comboBoxCustomer.Items.Clear();

            SqlCommand clientsCom = new SqlCommand("SELECT * FROM CLIENTS ORDER BY CCLIENTNAME");
            clientsCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            clientsCom.Connection.Open();
            Clients.Load(clientsCom.ExecuteReader());
            clientsCom.Connection.Close();

            for (int i = 0; i < Clients.Rows.Count; i++)
            {
                comboBoxCustomer.Items.Add(Clients.Rows[i]["CCLIENTNAME"].ToString().TrimEnd());
            }

            comboBoxCustomer.SelectedIndex = 0;

            comboBoxCustomer_SelectedIndexChanged(null, null);
        }

        private void FillObjects()
        {
            Objects = new DataTable();

            comboBoxFiles.Items.Clear();

            SqlCommand filesCom = new SqlCommand("SELECT * FROM AriaObject WHERE ObjectType = 'Data' ORDER BY ObjectName");
            filesCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            filesCom.Connection.Open();
            Objects.Load(filesCom.ExecuteReader());
            filesCom.Connection.Close();

            for (int i = 0; i < Objects.Rows.Count; i++)
            {
                comboBoxFiles.Items.Add(Objects.Rows[i]["ObjectName"].ToString().TrimEnd());
            }

            if (comboBoxFiles.Items.Count > 0)
            {
                comboBoxFiles.SelectedIndex = 0;
            }

            comboBoxFiles_SelectedIndexChanged(null, null);
        }

        private void FillRelatedObjects()
        {
            RelatedObjects = new DataTable();

            comboBoxRelatedFiles.Items.Clear();

            SqlCommand filesCom = new SqlCommand("SELECT * FROM AriaObject WHERE ObjectType = 'Data' ORDER BY ObjectName");
            filesCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            filesCom.Connection.Open();
            RelatedObjects.Load(filesCom.ExecuteReader());
            filesCom.Connection.Close();

            for (int i = 0; i < RelatedObjects.Rows.Count; i++)
            {
                comboBoxRelatedFiles.Items.Add(RelatedObjects.Rows[i]["ObjectName"].ToString().TrimEnd());
            }

            if (comboBoxRelatedFiles.Items.Count > 0)
            {
                comboBoxRelatedFiles.SelectedIndex = 0;
            }

            comboBoxRelatedFiles_SelectedIndexChanged(null, null);
        }

        private bool IsFileExist(out long objectID, out long parentObjectID, out string objectName)
        {
            objectID = 0;
            parentObjectID = 0;

            if (comboBoxFiles.SelectedIndex > -1)
            {
                SqlCommand revCom = new SqlCommand("Select *, (SELECT ObjectName FROM AriaObject WHERE AriaObject.ObjectID = AriaObjectRevision.ObjectID) AS ObjectName, (SELECT ParentObjectID FROM AriaObject WHERE AriaObject.ObjectID = AriaObjectRevision.ObjectID) AS ParentObjectID FROM AriaObjectRevision WHERE ObjectID IN (SELECT ObjectID FROM AriaObject WHERE ObjectType = 'Data')");
                revCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
                revCom.Connection.Open();

                DataTable rev = new DataTable();
                rev.Load(revCom.ExecuteReader());

                revCom.Connection.Close();

                for(int i = 0; i < rev.Rows.Count; i++)
                {
                    AriaDataObjectSettings dataObject = (AriaDataObjectSettings)Serializer.ConvertFromXml(rev.Rows[i]["ObjectRevisionSettings"].ToString());

                    if (dataObject.TableName.Trim().ToUpper() == Objects.Rows[comboBoxFiles.SelectedIndex]["ObjectName"].ToString().Trim())
                    {
                        objectID = System.Convert.ToInt64(rev.Rows[i]["ObjectID"]);
                        parentObjectID = System.Convert.ToInt64(rev.Rows[i]["ParentObjectID"]);
                        objectName = rev.Rows[i]["ObjectName"].ToString();
                        return true;
                    }
                }

                objectName = "";
                return false;
            }
            else
            {
                objectName = "";
                return false;
            }
        }

        private bool IsRelatedFileExist(out long objectID, out long parentObjectID, out string objectName)
        {
            objectID = 0;
            parentObjectID = 0;

            if (comboBoxFiles.SelectedIndex > -1)
            {
                SqlCommand revCom = new SqlCommand("Select *, (SELECT ObjectName FROM AriaObject WHERE AriaObject.ObjectID = AriaObjectRevision.ObjectID) AS ObjectName, (SELECT ParentObjectID FROM AriaObject WHERE AriaObject.ObjectID = AriaObjectRevision.ObjectID) AS ParentObjectID FROM AriaObjectRevision WHERE ObjectID IN (SELECT ObjectID FROM AriaObject WHERE ObjectType = 'Data')");
                revCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
                revCom.Connection.Open();

                DataTable rev = new DataTable();
                rev.Load(revCom.ExecuteReader());

                revCom.Connection.Close();

                for (int i = 0; i < rev.Rows.Count; i++)
                {
                    AriaDataObjectSettings dataObject = (AriaDataObjectSettings)Serializer.ConvertFromXml(rev.Rows[i]["ObjectRevisionSettings"].ToString());

                    if (dataObject.TableName.Trim().ToUpper() == RelatedObjects.Rows[comboBoxRelatedFiles.SelectedIndex]["CFILE_NAM"].ToString().Trim())
                    {
                        objectID = System.Convert.ToInt64(rev.Rows[i]["ObjectID"]);
                        parentObjectID = System.Convert.ToInt64(rev.Rows[i]["ParentObjectID"]);
                        objectName = rev.Rows[i]["ObjectName"].ToString();
                        return true;
                    }
                }

                objectName = "";
                return false;
            }
            else
            {
                objectName = "";
                return false;
            }
        }

        public long GetAria4XPObjectID()
        {
            SqlCommand command = new SqlCommand();
            command.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            command.Connection.Open();
            command.CommandText = "SELECT ObjectID FROM AriaObject WHERE ObjectName = 'Aria4XP'";
            long returnValue = System.Convert.ToInt64(command.ExecuteScalar());
            command.Connection.Close();

            return returnValue;
        }

        public long GetObjectID(string objectName)
        {
            SqlCommand command = new SqlCommand();
            command.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            command.Connection.Open();
            command.CommandText = "SELECT ObjectID FROM AriaObject WHERE ObjectName = '" + objectName  + "'";
            long returnValue = System.Convert.ToInt64(command.ExecuteScalar());
            command.Connection.Close();

            return returnValue;
        }

        public void AddRelatedFileToAria5()
        {
            SqlCommand command = new SqlCommand();
            command.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            command.Connection.Open();

            // Add Related Table
            command.Parameters.Clear();


            command.CommandText = "INSERT INTO AriaObject(ParentObjectID, ObjectName, ObjectDescription, ObjectType, ActiveRevision) Values (@ParentObjectID, @ObjectName, @ObjectDescription, @ObjectType, @ActiveRevision); SELECT @@IDENTITY";
            command.Parameters.AddWithValue("@ParentObjectID", GetObjectID(comboBoxFiles.SelectedItem.ToString()));
            command.Parameters.AddWithValue("@ObjectName", textBoxAria4XP.Text + textBoxRelatedFileName.Text);
            command.Parameters.AddWithValue("@ObjectDescription", textBoxRelatedFileName.Text);
            command.Parameters.AddWithValue("@ObjectType", AriaObjectTypes.RelatedData.ToString());
            command.Parameters.AddWithValue("@ActiveRevision", ActiveRevision);
            long relatedDataObjectID = System.Convert.ToInt64(command.ExecuteScalar());

            /// Revision
            AriaRelatedDataObjectSettings relatedDataRevisionSettings = new AriaRelatedDataObjectSettings();
            relatedDataRevisionSettings.DataObjectName = comboBoxRelatedFiles.SelectedItem.ToString().TrimEnd();
            relatedDataRevisionSettings.DataObjectRevision = ActiveRevision;
            relatedDataRevisionSettings.Filter = textBoxFileFilter.Text;
            relatedDataRevisionSettings.ParentDataObjectRevision = ActiveRevision;
            relatedDataRevisionSettings.ModificationType = AriaModificationTypes.Add;

            command.Parameters.Clear();

            command.CommandText = "INSERT INTO AriaObjectRevision(ObjectID, ObjectRevision, ObjectRevisionSettings) Values (@ObjectID, @ObjectRevision, @ObjectRevisionSettings)";
            command.Parameters.AddWithValue("@ObjectID", relatedDataObjectID);
            command.Parameters.AddWithValue("@ObjectRevision", ActiveRevision);
            command.Parameters.AddWithValue("@ObjectRevisionSettings", Serializer.ConvertToXml(relatedDataRevisionSettings));
            command.ExecuteNonQuery();

            command.Parameters.Clear();


            /// Add Properties
            command.CommandText = "SELECT * FROM AriaObjectProperty WHERE ObjectID = @ObjectID";
            command.Parameters.AddWithValue("@ObjectID", GetObjectID(comboBoxRelatedFiles.SelectedItem.ToString()));

            DataTable properties = new DataTable();
            properties.Load(command.ExecuteReader());

            foreach (DataRow row in properties.Rows)
            {
                AriaFieldSettings fieldSettings = (AriaFieldSettings)Serializer.ConvertFromXml(row["PropertySettings"].ToString());

                AriaRelatedFieldSettings relatedFieldSettings = new AriaRelatedFieldSettings();

                relatedFieldSettings.Code = fieldSettings.Code;
                relatedFieldSettings.DataType = fieldSettings.DataType;
                relatedFieldSettings.DecimalPlaces = fieldSettings.DecimalPlaces;
                relatedFieldSettings.EmailAddressSeparator = fieldSettings.EmailAddressSeparator;
                relatedFieldSettings.FieldName = fieldSettings.FieldName;
                relatedFieldSettings.HasReleatedField = fieldSettings.HasReleatedField;
                relatedFieldSettings.Head = fieldSettings.Head;
                relatedFieldSettings.InternalEmail = fieldSettings.InternalEmail;
                relatedFieldSettings.IsEmail = fieldSettings.IsEmail;
                relatedFieldSettings.IsPrimaryKey = fieldSettings.IsPrimaryKey;
                relatedFieldSettings.IsReleatedField = fieldSettings.IsReleatedField;
                relatedFieldSettings.Mask = fieldSettings.Mask;
                relatedFieldSettings.Message = fieldSettings.Message;
                relatedFieldSettings.ReleatedFields = fieldSettings.ReleatedFields;
                relatedFieldSettings.ValidEntries = fieldSettings.ValidEntries;
                relatedFieldSettings.ValidEntry = fieldSettings.ValidEntry;
                relatedFieldSettings.ValidExpression = fieldSettings.ValidExpression;
                relatedFieldSettings.Width = fieldSettings.Width;

                command.Parameters.Clear();

                command.CommandText = "INSERT INTO AriaObjectProperty(ObjectID, ObjectRevision, PropertyName, PropertyDescription, ModificationType, PropertyType, PropertySettings) Values (@ObjectID, @ObjectRevision, @PropertyName, @PropertyDescription, @ModificationType, @PropertyType, @PropertySettings)";
                command.Parameters.AddWithValue("@ObjectID", relatedDataObjectID);
                command.Parameters.AddWithValue("@ObjectRevision", ActiveRevision);
                command.Parameters.AddWithValue("@PropertyName", row["PropertyName"].ToString());
                command.Parameters.AddWithValue("@PropertyDescription", row["PropertyDescription"].ToString().TrimEnd());
                command.Parameters.AddWithValue("@ModificationType", AriaModificationTypes.Add.ToString());
                command.Parameters.AddWithValue("@PropertyType", AriaDataTypes.AriaRelatedField.ToString());
                command.Parameters.AddWithValue("@PropertySettings", Serializer.ConvertToXml(relatedFieldSettings));
                command.ExecuteNonQuery();
            }

            command.Connection.Close();
        }

        public void RemoveReportFromAria5()
        {
            long objectID, parentObjectID;
            string objectName;
            IsRelatedFileExist(out objectID, out parentObjectID, out objectName);

            SqlCommand command = new SqlCommand();
            command.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            command.Connection.Open();
            
            command.Parameters.AddWithValue("@ObjectID", objectID);
            command.Parameters.AddWithValue("@ParentObjectID", parentObjectID);

            command.CommandText = "DELETE FROM AriaObjectProperty WHERE ObjectID = @ObjectID";
            command.ExecuteNonQuery();

            command.CommandText = "DELETE FROM AriaObjectMethodParameter WHERE ObjectID = @ParentObjectID";
            command.ExecuteNonQuery();

            command.CommandText = "DELETE FROM AriaObjectMethod WHERE ObjectID = @ParentObjectID";
            command.ExecuteNonQuery();

            command.CommandText = "DELETE FROM AriaObjectRevision WHERE ObjectID = @ParentObjectID";
            command.ExecuteNonQuery();

            command.CommandText = "DELETE FROM AriaObjectRevision WHERE ObjectID = @ObjectID";
            command.ExecuteNonQuery();

            command.CommandText = "DELETE FROM AriaObject WHERE ObjectID = @ParentObjectID";
            command.ExecuteNonQuery();

            command.CommandText = "DELETE FROM AriaObject WHERE ObjectID = @ObjectID";
            command.ExecuteNonQuery();

            command.Connection.Close();
        }

        private void FormConvertReport_Load(object sender, EventArgs e)
        {
            FillCustomers();
        }

        private void comboBoxCustomer_SelectedIndexChanged(object sender, EventArgs e)
        {
            EnviromentVariables.ClientID = Clients.Rows[comboBoxCustomer.SelectedIndex]["CCLIENTID"].ToString().TrimEnd();
            EnviromentVariables.ConnectionsRefresh();

            FillObjects();
            FillRelatedObjects();
        }

        private void comboBoxFiles_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (comboBoxFiles.SelectedIndex == -1)
            {
                textBoxAria4XP.Text = "Aria4XP.";

                textBoxRelatedFileName.ReadOnly = true;
                buttonAddToAria4.Enabled = false;
            }
            else
            {
                textBoxAria4XP.Text = comboBoxFiles.SelectedItem.ToString().TrimEnd() + ".";

                textBoxRelatedFileName.ReadOnly = false;
                buttonAddToAria4.Enabled = true;
            }
        }

        private void textBoxReportName_TextChanged(object sender, EventArgs e)
        {
            errorProvider.Clear();
        }

        private void buttonAddToAria4_Click(object sender, EventArgs e)
        {
            foreach (char c in textBoxRelatedFileName.Text.ToCharArray())
            {
                if (!char.IsLetterOrDigit(c) && c != ' ')
                {
                    errorProvider.SetError(textBoxRelatedFileName, "Data object name cannot contains special character.");
                    return;
                }
            }

            SqlCommand command = new SqlCommand();
            command.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            command.Connection.Open();
            command.CommandText = "SELECT ObjectID FROM AriaObject WHERE ObjectName = @ObjectName";
            command.Parameters.AddWithValue("@ObjectName", textBoxAria4XP.Text + textBoxRelatedFileName.Text);
            DataTable report = new DataTable();
            report.Load(command.ExecuteReader());
            command.Connection.Close();

            if (report.Rows.Count > 0)
            {
                errorProvider.SetError(textBoxAria4XP, "Related data object name is used by another data object.");
            }
            else
            {
                AddRelatedFileToAria5();
                MessageBox.Show("Related data object sucessfully added to Aria5", "Aria5", MessageBoxButtons.OK, MessageBoxIcon.Information);
                DialogResult = DialogResult.OK;
            }
        }

        private void buttonClose_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void comboBoxRelatedFiles_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (comboBoxRelatedFiles.SelectedIndex == -1)
            {
                textBoxRelatedFileName.Text = "";
                textBoxRelatedFileName.ReadOnly = true;
                buttonAddToAria4.Enabled = false;
            }
            else
            {
                textBoxRelatedFileName.Text = RelatedObjects.Rows[comboBoxRelatedFiles.SelectedIndex]["ObjectName"].ToString().TrimEnd().Split('.').Last();
                textBoxRelatedFileName.ReadOnly = false;
                buttonAddToAria4.Enabled = true;
            }
        }
    }
}