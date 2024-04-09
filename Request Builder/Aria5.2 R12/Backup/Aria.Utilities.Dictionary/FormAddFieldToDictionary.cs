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
    public partial class FormAddFieldToDictionary : Form
    {
        public FormAddFieldToDictionary()
        {
            InitializeComponent();
        }

        const string ActiveRevision = "000001";

        AriaXmlSerializer Serializer = new AriaXmlSerializer(); 
        
        AriaEnviromentVariables EnviromentVariables = new AriaEnviromentVariables();

        DataTable Clients;
        DataTable Objects;
        DataTable FileFields;

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

        private void FillObjectNames()
        {
            Objects = new DataTable();

            comboBoxObjectName.Items.Clear();

            SqlCommand filesCom = new SqlCommand("SELECT * FROM AriaObject WHERE ObjectType = 'Data' ORDER BY ObjectName");
            filesCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            filesCom.Connection.Open();
            Objects.Load(filesCom.ExecuteReader());
            filesCom.Connection.Close();

            for (int i = 0; i < Objects.Rows.Count; i++)
            {
                comboBoxObjectName.Items.Add(Objects.Rows[i]["ObjectName"].ToString().TrimEnd());
            }

            if (comboBoxObjectName.Items.Count > 0)
            {
                comboBoxObjectName.SelectedIndex = 0;
            }

            comboBoxObjectNames_SelectedIndexChanged(null, null);
        }

        private void FillFileFields()
        {
            FileFields = new DataTable();
            
            comboBoxFields.Items.Clear();

            if (comboBoxObjectName.SelectedIndex > -1 && GetObjectTableType() >= 0)
            {
                OdbcCommand fileFieldsCom = new OdbcCommand("SELECT SYDFIELD.* FROM SYDFLFLD LEFT JOIN SYDFIELD ON SYDFLFLD.CFLD_NAME = SYDFIELD.CFLD_NAME WHERE SYDFLFLD.CFILE_NAM = '" + GetObjectTable() + "' ORDER BY SYDFLFLD.CFILE_NAM");
                fileFieldsCom.Connection = new OdbcConnection(GetObjectTableType() == 0 ? EnviromentVariables.Aria27SystemFilesConnectionString : EnviromentVariables.Aria40SystemFilesConnectionString);
                fileFieldsCom.Connection.Open();
                FileFields.Load(fileFieldsCom.ExecuteReader());
                fileFieldsCom.Connection.Close();

                for (int i = 0; i < FileFields.Rows.Count; i++)
                {
                    comboBoxFields.Items.Add(FileFields.Rows[i]["CFLD_NAME"].ToString().TrimEnd());
                }
            }

            if (comboBoxFields.Items.Count > 0)
            {
                comboBoxFields.SelectedIndex = 0;
                comboBoxFields_SelectedIndexChanged(null, null);
            }
        }

        private bool IsPropertyExist(out string propertyName)
        {
            if (GetFileObjectID() > -1)
            {
                SqlCommand propCom = new SqlCommand("Select * FROM AriaObjectProperty WHERE ObjectID = @ObjectID");
                propCom.Parameters.AddWithValue("@ObjectID", GetFileObjectID());
                propCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
                propCom.Connection.Open();

                DataTable prop = new DataTable();
                prop.Load(propCom.ExecuteReader());

                propCom.Connection.Close();

                for(int i = 0; i < prop.Rows.Count; i++)
                {
                    AriaFieldSettings field = (AriaFieldSettings)Serializer.ConvertFromXml(prop.Rows[i]["PropertySettings"].ToString());

                    if (prop.Rows[i]["PropertySettings"].ToString().ToUpper() == textBoxPropertyName.Text.Trim().ToUpper())
                    {
                        propertyName = prop.Rows[i]["PropertyName"].ToString();
                        return true;
                    }

                    if (field.FieldName.Trim().ToUpper() == FileFields.Rows[comboBoxFields.SelectedIndex]["CFLD_NAME"].ToString().Trim().ToUpper())
                    {
                        propertyName = prop.Rows[i]["PropertyName"].ToString();
                        return true;
                    }
                }

                propertyName = "";
                return false;
            }
            else
            {
                propertyName = "";
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

        private string GetObjectTable()
        {
            if (comboBoxObjectName.SelectedIndex > -1)
            {
                SqlCommand revCom = new SqlCommand("Select * FROM AriaObjectRevision WHERE ObjectID IN (SELECT ObjectID FROM AriaObject WHERE ObjectName = '" + comboBoxObjectName.SelectedItem + "')");
                revCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
                revCom.Connection.Open();

                DataTable rev = new DataTable();
                rev.Load(revCom.ExecuteReader());

                revCom.Connection.Close();

                object a = Serializer.ConvertFromXml(rev.Rows[0]["ObjectRevisionSettings"].ToString());

                return ((AriaDataObjectSettings)a).TableName;
            }
            else
            {
                return "";
            }
        }

        private int GetObjectTableType()
        {
            if (comboBoxObjectName.SelectedIndex > -1)
            {
                SqlCommand revCom = new SqlCommand("Select * FROM AriaObjectRevision WHERE ObjectID IN (SELECT ObjectID FROM AriaObject WHERE ObjectName = '" + comboBoxObjectName.SelectedItem + "')");
                revCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
                revCom.Connection.Open();

                DataTable rev = new DataTable();
                rev.Load(revCom.ExecuteReader());

                revCom.Connection.Close();

                object a = Serializer.ConvertFromXml(rev.Rows[0]["ObjectRevisionSettings"].ToString());

                return ((AriaDataObjectSettings)a).DatabaseType == AriaDatabaseTypes.Aria27Data ? 0 : 1;
            }
            else
            {
                return -1;
            }
        }

        private long GetFileObjectID()
        {
            if (comboBoxObjectName.SelectedIndex > -1)
            {
                SqlCommand revCom = new SqlCommand("Select * FROM AriaObjectRevision WHERE ObjectID IN (SELECT ObjectID FROM AriaObject WHERE ObjectType = 'Data' and )");
                revCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
                revCom.Connection.Open();

                DataTable rev = new DataTable();
                rev.Load(revCom.ExecuteReader());

                revCom.Connection.Close();

                for (int i = 0; i < rev.Rows.Count; i++)
                {
                    object a = Serializer.ConvertFromXml(rev.Rows[i]["ObjectRevisionSettings"].ToString());
                    AriaDataObjectSettings b = new AriaDataObjectSettings();

                    AriaDataObjectSettings dataObject = (AriaDataObjectSettings)Serializer.ConvertFromXml(rev.Rows[i]["ObjectRevisionSettings"].ToString());

                    if (dataObject.TableName.Trim().ToUpper() == GetObjectTable().Trim().ToUpper())
                    {
                        return System.Convert.ToInt64(rev.Rows[i]["ObjectID"]);
                    }
                }

                return -1;
            }
            else
            {
                return -1;
            }
        }

        public void AddPropertyToAria5()
        {
            AriaFieldSettings settings = new AriaFieldSettings();

            DataRow row = FileFields.Rows[comboBoxFields.SelectedIndex];

            settings.IsPrimaryKey = checkBoxIsPrimaryKeyAria5.Checked;

            settings.FieldName = row["CFLD_NAME"].ToString().Trim().TrimEnd();
            settings.Head = textBoxPropertyDescription.Text.TrimEnd();
            settings.Message = row["CFLD_MSG"].ToString().Trim().TrimEnd();

            switch (row["CDATA_TYP"].ToString().Trim())
            {
                case "D":
                    settings.DataType = AriaStandardDataTypes.Date;
                    break;

                case "G":
                    settings.DataType = AriaStandardDataTypes.Binary;
                    break;

                case "L":
                    settings.DataType = AriaStandardDataTypes.Logical;
                    break;

                case "M":
                    settings.DataType = AriaStandardDataTypes.Memo;
                    break;

                case "N":
                    settings.DataType = AriaStandardDataTypes.Numeric;
                    break;

                case "C":
                    settings.DataType = AriaStandardDataTypes.String;
                    break;
            }

            settings.Width = Convert.ToInt32(row["NFLD_WDTH"].ToString().Trim());
            settings.DecimalPlaces = Convert.ToInt32(row["NFLD_DEC"].ToString().Trim());
            settings.ValidExpression = row["mvald_str"].ToString().Trim();
            settings.Mask = row["cpict_str"].ToString().Trim();
            settings.ValidEntry = (bool)row["LVLDENTRY"];

            if (((string)row["mventries"]).IndexOf("~") > 0)
            {
                string[] description = ((string)row["mventries"]).Substring(0, ((string)row["mventries"]).IndexOf("~")).Split('|');
                string[] values = ((string)row["mventries"]).Substring(((string)row["mventries"]).IndexOf("~") + 1).Split('|');

                for (int validIndex = 0; validIndex < values.Length; validIndex++)
                {
                    values[validIndex] += "|" + description[validIndex];
                }

                settings.ValidEntries = values;
            }

            settings.Code = (string)row["mcodeinfo"];
            settings.HasReleatedField = (bool)row["lrltfields"];
            settings.ReleatedFields = ((string)row["mrltfields"]).Split('|');
            settings.IsReleatedField = (bool)row["lrelated"];

            settings.IsEmail = checkBoxIsEmailAria5.Checked;
            settings.InternalEmail = checkBoxIsInternalEmailAria5.Checked;
            settings.InternalEmailConnectionType = textBoxInternalEmailConnectionTypeAria5.Text;
            settings.InternalEmailParameter = textBoxInternalEmailParameterAria5.Text;
            settings.InternalEmailSelect = textBoxInternalEmailSelectAria5.Text;


            SqlCommand command = new SqlCommand();
            command.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            command.Connection.Open();

            command.Parameters.Clear();

            command.CommandText = "INSERT INTO AriaObjectProperty(ObjectID, ObjectRevision, PropertyName, PropertyDescription, ModificationType, PropertyType, PropertySettings) Values (@ObjectID, @ObjectRevision, @PropertyName, @PropertyDescription, @ModificationType, @PropertyType, @PropertySettings)";
            command.Parameters.AddWithValue("@ObjectID", GetFileObjectID());
            command.Parameters.AddWithValue("@ObjectRevision", ActiveRevision);
            command.Parameters.AddWithValue("@PropertyName", textBoxPropertyName.Text.TrimEnd());
            command.Parameters.AddWithValue("@PropertyDescription", settings.Head.TrimEnd());
            command.Parameters.AddWithValue("@ModificationType", AriaModificationTypes.Add.ToString());
            command.Parameters.AddWithValue("@PropertyType", AriaDataTypes.AriaField.ToString());
            command.Parameters.AddWithValue("@PropertySettings", Serializer.ConvertToXml(settings));

            command.ExecuteNonQuery();


            command.Connection.Close();
        }

        private void FormConvertProperty_Load(object sender, EventArgs e)
        {
            FillCustomers();
        }

        private void comboBoxCustomer_SelectedIndexChanged(object sender, EventArgs e)
        {
            EnviromentVariables.ClientID = Clients.Rows[comboBoxCustomer.SelectedIndex]["CCLIENTID"].ToString().TrimEnd();
            EnviromentVariables.ConnectionsRefresh();

            FillObjectNames();
        }

        private void comboBoxObjectNames_SelectedIndexChanged(object sender, EventArgs e)
        {
            FillFileFields();
        }

        private void textBoxPropertyName_TextChanged(object sender, EventArgs e)
        {
            errorProvider.Clear();
        }

        private void buttonAddToAria4_Click(object sender, EventArgs e)
        {
            foreach (char c in textBoxPropertyName.Text.ToCharArray())
            {
                if (!char.IsLetterOrDigit(c) && c != ' ')
                {
                    errorProvider.SetError(textBoxPropertyName, "Property name cannot contains special character.");
                    return;
                }
            }

            string propName;
            if (IsPropertyExist(out propName))
            {
                MessageBox.Show("Property already exist in Aria 5.0.", "Aria 5.0", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            else
            {
                AddPropertyToAria5();
                MessageBox.Show("Report Sucessfully added to Aria 5.0", "Aria 5.0", MessageBoxButtons.OK, MessageBoxIcon.Information);
                DialogResult = DialogResult.OK;
            }
        }

        private void buttonClose_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void comboBoxFields_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (comboBoxObjectName.SelectedIndex == -1)
            {
                textBoxPropertyName.Text = "";
                textBoxPropertyName.ReadOnly = true;
                buttonAddToAria4.Enabled = false;
            }
            else
            {
                textBoxPropertyName.Text = FileFields.Rows[comboBoxFields.SelectedIndex]["CFLD_HEAD"].ToString().Replace(".", "").Replace("\\", "").TrimEnd();
                textBoxValidEntryAria5.Text = FileFields.Rows[comboBoxFields.SelectedIndex]["mventries"].ToString().TrimEnd();
                textBoxPropertyDescription.Text = FileFields.Rows[comboBoxFields.SelectedIndex]["CFLD_HEAD"].ToString().TrimEnd();
                textBoxPropertyName.ReadOnly = false;
                buttonAddToAria4.Enabled = true;
            }

        }
    }
}