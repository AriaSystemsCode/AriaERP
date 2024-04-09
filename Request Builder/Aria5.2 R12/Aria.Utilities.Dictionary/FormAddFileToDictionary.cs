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
    public partial class FormAddFileToDictionary : Form
    {
        public FormAddFileToDictionary()
        {
            InitializeComponent();
        }

        const string ActiveRevision = "000001";

        AriaXmlSerializer Serializer = new AriaXmlSerializer(); 
        
        AriaEnviromentVariables EnviromentVariables = new AriaEnviromentVariables();

        DataTable Clients;
        DataTable Files;

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

        private void FillFiles()
        {
            Files = new DataTable();

            comboBoxFiles.Items.Clear();

            OdbcCommand filesCom = new OdbcCommand("SELECT * FROM SYDFILES ORDER BY CFILE_NAM");
            filesCom.Connection = new OdbcConnection(comboBoxApplication.SelectedIndex == 0 ? EnviromentVariables.Aria27SystemFilesConnectionString : EnviromentVariables.Aria40SystemFilesConnectionString);
            filesCom.Connection.Open();
            Files.Load(filesCom.ExecuteReader());
            filesCom.Connection.Close();

            for (int i = 0; i < Files.Rows.Count; i++)
            {
                comboBoxFiles.Items.Add(Files.Rows[i]["CFILE_NAM"].ToString().TrimEnd());
            }

            if (comboBoxFiles.Items.Count > 0)
            {
                comboBoxFiles.SelectedIndex = 0;
            }

            comboBoxFiles_SelectedIndexChanged(null, null);
        }

        private bool IsFileExist(out long objectID, out long parentObjectID)
        {
            objectID = 0;
            parentObjectID = 0;

            if (comboBoxFiles.SelectedIndex > -1)
            {
                SqlCommand revCom = new SqlCommand("Select *, (SELECT ParentObjectID FROM AriaObject WHERE AriaObject.ObjectID = AriaObjectRevision.ObjectID) AS ParentObjectID FROM AriaObjectRevision WHERE ObjectID IN (SELECT ObjectID FROM AriaObject WHERE ObjectType = 'Data')");
                revCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
                revCom.Connection.Open();

                DataTable rev = new DataTable();
                rev.Load(revCom.ExecuteReader());

                revCom.Connection.Close();

                for(int i = 0; i < rev.Rows.Count; i++)
                {
                    AriaDataObjectSettings dataObject = (AriaDataObjectSettings)Serializer.ConvertFromXml(rev.Rows[i]["ObjectRevisionSettings"].ToString());

                    if (dataObject.TableName.Trim().ToUpper() == Files.Rows[comboBoxFiles.SelectedIndex]["CFILE_NAM"].ToString().Trim())
                    {
                        objectID = System.Convert.ToInt64(rev.Rows[i]["ObjectID"]);
                        parentObjectID = System.Convert.ToInt64(rev.Rows[i]["ParentObjectID"]);
                        return true;
                    }
                }

                return false;
            }
            else
            {
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

        public void AddFileToAria5()
        {
            string fileName = Files.Rows[comboBoxFiles.SelectedIndex]["CFILE_NAM"].ToString();

            SqlCommand command = new SqlCommand();
            command.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            command.Connection.Open();

            // Add Report
            /// Object
            command.Parameters.Clear();

            command.CommandText = "INSERT INTO AriaObject(ParentObjectID, ObjectName, ObjectDescription, ObjectType, ActiveRevision) Values (@ParentObjectID, @ObjectName, @ObjectDescription, @ObjectType, @ActiveRevision); SELECT @@IDENTITY";
            command.Parameters.AddWithValue("@ParentObjectID", GetAria4XPObjectID());
            command.Parameters.AddWithValue("@ObjectName", textBoxAria4XP.Text + textBoxFileName.Text.TrimEnd());
            command.Parameters.AddWithValue("@ObjectDescription", textBoxFileName.Text.TrimEnd());
            command.Parameters.AddWithValue("@ObjectType", AriaObjectTypes.Data.ToString());
            command.Parameters.AddWithValue("@ActiveRevision", ActiveRevision);
            long reportObjectID = System.Convert.ToInt64(command.ExecuteScalar());

            /// Revision
            AriaDataObjectSettings dataRevisionSettings = new AriaDataObjectSettings();
            dataRevisionSettings.DatabaseType = comboBoxApplication.SelectedIndex == 0 ? AriaDatabaseTypes.Aria27Data : AriaDatabaseTypes.Aria40Data;
            dataRevisionSettings.Filter = textBoxFileFilter.Text.TrimEnd();
            dataRevisionSettings.FixedFilter = textBoxFileFixedFilterAria5.Text.TrimEnd();
            dataRevisionSettings.ModificationType = AriaModificationTypes.Add;
            dataRevisionSettings.TableName = fileName.TrimEnd();
            dataRevisionSettings.ParentDataObjectRevision = ActiveRevision;

            command.Parameters.Clear();

            command.CommandText = "INSERT INTO AriaObjectRevision(ObjectID, ObjectRevision, ObjectRevisionSettings) Values (@ObjectID, @ObjectRevision, @ObjectRevisionSettings)";
            command.Parameters.AddWithValue("@ObjectID", reportObjectID);
            command.Parameters.AddWithValue("@ObjectRevision", ActiveRevision);
            command.Parameters.AddWithValue("@ObjectRevisionSettings", Serializer.ConvertToXml(dataRevisionSettings));
            command.ExecuteNonQuery();

            command.Connection.Close();
        }


        private void FormConvertReport_Load(object sender, EventArgs e)
        {
            FillCustomers();
        }

        private void comboBoxApplication_SelectedIndexChanged(object sender, EventArgs e)
        {
            FillFiles();
        }

        private void comboBoxCustomer_SelectedIndexChanged(object sender, EventArgs e)
        {
            EnviromentVariables.ClientID = Clients.Rows[comboBoxCustomer.SelectedIndex]["CCLIENTID"].ToString().TrimEnd();
            EnviromentVariables.ConnectionsRefresh();
            comboBoxApplication.SelectedIndex = 0;

            FillFiles();
        }

        private void comboBoxFiles_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (comboBoxFiles.SelectedIndex == -1)
            {
                textBoxFileName.Text = "";
                textBoxFileName.ReadOnly = true;
                buttonAddToAria4.Enabled = false;
            }
            else
            {
                textBoxFileName.Text = Files.Rows[comboBoxFiles.SelectedIndex]["CFILE_TTL"].ToString().TrimEnd();
                textBoxFileDescription.Text = Files.Rows[comboBoxFiles.SelectedIndex]["CFILE_TTL"].ToString().TrimEnd();
                textBoxFileName.ReadOnly = false;
                buttonAddToAria4.Enabled = true;
            }
        }

        private void textBoxReportName_TextChanged(object sender, EventArgs e)
        {
            errorProvider.Clear();
        }

        private void buttonAddToAria4_Click(object sender, EventArgs e)
        {
            foreach (char c in textBoxFileName.Text.ToCharArray())
            {
                if (!char.IsLetterOrDigit(c) && c != ' ')
                {
                    errorProvider.SetError(textBoxFileName, "Data object name cannot contains special character.");
                    return;
                }
            }

            SqlCommand command = new SqlCommand();
            command.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            command.Connection.Open();
            command.CommandText = "SELECT ObjectID FROM AriaObject WHERE ObjectName = @ObjectName";
            command.Parameters.AddWithValue("@ObjectName", textBoxAria4XP.Text + textBoxFileName.Text);
            DataTable report = new DataTable();
            report.Load(command.ExecuteReader());
            command.Connection.Close();

            if (report.Rows.Count > 0)
            {
                errorProvider.SetError(textBoxFileName, "Data object name is used by another data object.");
            }
            else
            {
                AddFileToAria5();
                MessageBox.Show("Data object sucessfully added to Aria 5.0.", "Aria 5.0", MessageBoxButtons.OK, MessageBoxIcon.Information);
                DialogResult = DialogResult.OK;
            }
        }

        private void buttonClose_Click(object sender, EventArgs e)
        {
            Close();
        }
    }
}