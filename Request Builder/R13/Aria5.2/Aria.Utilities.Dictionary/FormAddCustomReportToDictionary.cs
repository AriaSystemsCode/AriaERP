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
    public partial class FormAddCustomReportToDictionary : Form
    {
        public FormAddCustomReportToDictionary()
        {
            InitializeComponent();
        }

        const string ActiveRevision = "000001";

        AriaXmlSerializer Serializer = new AriaXmlSerializer(); 
        
        AriaEnviromentVariables EnviromentVariables = new AriaEnviromentVariables();

        DataTable Clients;
        DataTable Reports;
        DataTable ReportOptions;

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

        private void FillReports()
        {
            Reports = new DataTable();

            comboBoxReports.Items.Clear();

            OdbcCommand reportsCom = new OdbcCommand("SELECT * FROM SYDREPRT WHERE CUPGRDLVL = 'U' ORDER BY CREP_NAME");
            reportsCom.Connection = new OdbcConnection(EnviromentVariables.Aria40SystemFilesConnectionString);
            reportsCom.Connection.Open();
            Reports.Load(reportsCom.ExecuteReader());
            reportsCom.Connection.Close();

            for (int i = 0; i < Reports.Rows.Count; i++)
            {
                comboBoxReports.Items.Add(Reports.Rows[i]["CREP_NAME"].ToString().TrimEnd());
            }

            if (comboBoxReports.Items.Count > 0)
            {
                comboBoxReports.SelectedIndex = 0;
            }

            comboBoxReports_SelectedIndexChanged(null, null);
        }

        private void FillReportsOptions()
        {
            ReportOptions = new DataTable();
            
            listBoxOptionGrid.Items.Clear();

            if (comboBoxReports.SelectedIndex > -1)
            {
                OdbcCommand reportOptionsCom = new OdbcCommand("SELECT * FROM SYREPUVR WHERE CREP_ID = '" + Reports.Rows[comboBoxReports.SelectedIndex]["CREP_ID"].ToString() + "' .AND. !EMPTY(CFLD_HEAD) .AND. ('.' $ MFLD_NAME .OR. LASKRUNT)");
                reportOptionsCom.Connection = new OdbcConnection(EnviromentVariables.Aria40SystemFilesConnectionString);
                reportOptionsCom.Connection.Open();
                ReportOptions.Load(reportOptionsCom.ExecuteReader());
                reportOptionsCom.Connection.Close();

                for (int i = 0; i < ReportOptions.Rows.Count; i++)
                {
                    listBoxOptionGrid.Items.Add(ReportOptions.Rows[i]["CFLD_HEAD"].ToString().TrimEnd());
                }
            }
        }

        private bool IsReportExist(out long objectID, out long parentObjectID)
        {
            objectID = 0;
            parentObjectID = 0;

            if (comboBoxReports.SelectedIndex > -1)
            {
                SqlCommand revCom = new SqlCommand("Select *, (SELECT ParentObjectID FROM CustomObject WHERE CustomObject.ObjectID = CustomObjectRevision.ObjectID) AS ParentObjectID FROM CustomObjectRevision WHERE ObjectID IN (SELECT ObjectID FROM CustomObject WHERE ObjectType = 'OptionGrid')");
                revCom.Connection = new SqlConnection(EnviromentVariables.Aria50ClientSystemFilesConnectionString);
                revCom.Connection.Open();

                DataTable rev = new DataTable();
                rev.Load(revCom.ExecuteReader());

                revCom.Connection.Close();

                for(int i = 0; i < rev.Rows.Count; i++)
                {
                    AriaOptionGridSettings optionGrid = (AriaOptionGridSettings)Serializer.ConvertFromXml(rev.Rows[i]["ObjectRevisionSettings"].ToString());

                    if (optionGrid.OptionGridId.Trim() == Reports.Rows[comboBoxReports.SelectedIndex]["CREP_ID"].ToString().Trim())
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

        public void AddReportToAria5()
        {
            string optionGirdID = Reports.Rows[comboBoxReports.SelectedIndex]["CREP_ID"].ToString();

            OdbcCommand reportOptionsCom = new OdbcCommand("UPDATE SYDREPRT SET A5OBJNAM = '" + textBoxAria4XP.Text + textBoxReportName.Text + "' WHERE CREP_ID = '" + optionGirdID + "'");
            reportOptionsCom.Connection = new OdbcConnection(EnviromentVariables.Aria40SystemFilesConnectionString);
            reportOptionsCom.Connection.Open();
            ReportOptions.Load(reportOptionsCom.ExecuteReader());
            reportOptionsCom.Connection.Close();

            SqlCommand command = new SqlCommand();
            command.Connection = new SqlConnection(EnviromentVariables.Aria50ClientSystemFilesConnectionString);
            command.Connection.Open();

            // Add Report
            /// Object
            command.Parameters.Clear();

            command.CommandText = "INSERT INTO CustomObject(ParentObjectID, ObjectName, ObjectDescription, ObjectType, ActiveRevision) Values (@ParentObjectID, @ObjectName, @ObjectDescription, @ObjectType, @ActiveRevision); SELECT @@IDENTITY";
            command.Parameters.AddWithValue("@ParentObjectID", GetAria4XPObjectID());
            command.Parameters.AddWithValue("@ObjectName", textBoxAria4XP.Text + textBoxReportName.Text);
            command.Parameters.AddWithValue("@ObjectDescription", textBoxReportName.Text);
            command.Parameters.AddWithValue("@ObjectType", AriaObjectTypes.Report.ToString());
            command.Parameters.AddWithValue("@ActiveRevision", ActiveRevision);
            long reportObjectID = System.Convert.ToInt64(command.ExecuteScalar());

            /// Revision
            AriaReportObjectSettings reportRevisionSettings = new AriaReportObjectSettings();
            reportRevisionSettings.ClassName = optionGirdID.Trim() + "." + optionGirdID.Trim();
            reportRevisionSettings.SupportedFormats = new AriaOutputFormatTypes[] { AriaOutputFormatTypes.Excel, AriaOutputFormatTypes.Html, AriaOutputFormatTypes.Pdf, AriaOutputFormatTypes.Txt, AriaOutputFormatTypes.Xml };
            reportRevisionSettings.ModificationType = AriaModificationTypes.Add;
            reportRevisionSettings.ParentDataObjectRevision = ActiveRevision;

            command.Parameters.Clear();

            command.CommandText = "INSERT INTO CustomObjectRevision(ObjectID, ObjectRevision, ObjectRevisionSettings) Values (@ObjectID, @ObjectRevision, @ObjectRevisionSettings)";
            command.Parameters.AddWithValue("@ObjectID", reportObjectID);
            command.Parameters.AddWithValue("@ObjectRevision", ActiveRevision);
            command.Parameters.AddWithValue("@ObjectRevisionSettings", Serializer.ConvertToXml(reportRevisionSettings));
            command.ExecuteNonQuery();

            /// Method
            command.Parameters.Clear();

            command.CommandText = "INSERT INTO CustomObjectMethod(ObjectID, ObjectRevision, MethodName, ModificationType, BusinessObjectParameterName) Values (@ObjectID, @ObjectRevision, @MethodName, @ModificationType, @BusinessObjectParameterName)";
            command.Parameters.AddWithValue("@ObjectID", reportObjectID);
            command.Parameters.AddWithValue("@ObjectRevision", ActiveRevision);
            command.Parameters.AddWithValue("@MethodName", "PrintReport");
            command.Parameters.AddWithValue("@ModificationType", AriaModificationTypes.Add.ToString());
            command.Parameters.AddWithValue("@BusinessObjectParameterName", "");
            command.ExecuteNonQuery();
            
            /// Method Parameter
            AriaOptionGridXmlDataSetSettings reportParameterSettings = new AriaOptionGridXmlDataSetSettings();
            reportParameterSettings.OptionGridObjectName = textBoxAria4XP.Text + textBoxReportName.Text.Trim() + ".OptionGrid";
            reportParameterSettings.OptionGridRevision = ActiveRevision;
            
            command.Parameters.Clear();

            command.CommandText = "INSERT INTO CustomObjectMethodParameter(ObjectID, ObjectRevision, MethodName, ParameterNo, ParameterName, ParameterType, ParameterSettings) Values (@ObjectID, @ObjectRevision, @MethodName, @ParameterNo, @ParameterName, @ParameterType, @ParameterSettings)";
            command.Parameters.AddWithValue("@ObjectID", reportObjectID);
            command.Parameters.AddWithValue("@ObjectRevision", ActiveRevision);
            command.Parameters.AddWithValue("@MethodName", "PrintReport");
            command.Parameters.AddWithValue("@ParameterNo", 1);
            command.Parameters.AddWithValue("@ParameterName", AriaObjectTypes.OptionGrid.ToString());
            command.Parameters.AddWithValue("@ParameterType", Aria.DataTypes.AriaDataTypes.AriaOptionGridXmlDataSet.ToString());
            command.Parameters.AddWithValue("@ParameterSettings", Serializer.ConvertToXml(reportParameterSettings));
            command.ExecuteNonQuery();

            // Add Option Grid
            /// Object
            command.Parameters.Clear();

            command.CommandText = "INSERT INTO CustomObject(ParentObjectID, ObjectName, ObjectDescription, ObjectType, ActiveRevision) Values (@ParentObjectID, @ObjectName, @ObjectDescription, @ObjectType, @ActiveRevision); SELECT @@IDENTITY";
            command.Parameters.AddWithValue("@ParentObjectID", reportObjectID);
            command.Parameters.AddWithValue("@ObjectName", textBoxAria4XP.Text + textBoxReportName.Text.Trim() + "." + AriaObjectTypes.OptionGrid.ToString());
            command.Parameters.AddWithValue("@ObjectDescription", textBoxReportName.Text.Trim() + " " + AriaObjectTypes.OptionGrid.ToString());
            command.Parameters.AddWithValue("@ObjectType", AriaObjectTypes.OptionGrid.ToString());
            command.Parameters.AddWithValue("@ActiveRevision", ActiveRevision);
            long optionsObjectID = System.Convert.ToInt64(command.ExecuteScalar());

            /// Revision
            AriaOptionGridSettings optionsRevisionSettings = new AriaOptionGridSettings();
            optionsRevisionSettings.OptionGridId = optionGirdID.Trim();
            optionsRevisionSettings.ModificationType = AriaModificationTypes.Add;
            optionsRevisionSettings.ParentDataObjectRevision = ActiveRevision;

            command.Parameters.Clear();

            command.CommandText = "INSERT INTO CustomObjectRevision(ObjectID, ObjectRevision, ObjectRevisionSettings) Values (@ObjectID, @ObjectRevision, @ObjectRevisionSettings)";
            command.Parameters.AddWithValue("@ObjectID", optionsObjectID);
            command.Parameters.AddWithValue("@ObjectRevision", ActiveRevision);
            command.Parameters.AddWithValue("@ObjectRevisionSettings", Serializer.ConvertToXml(optionsRevisionSettings));
            command.ExecuteNonQuery();

            /// Property
            List<string> properties = new List<string>();

            foreach(DataRow row in ReportOptions.Rows)
            {
                string propertyName = row["CFLD_HEAD"].ToString().Trim();

                propertyName = propertyName.Replace(".", "").Replace("\\", "");

                if (propertyName != "" && !properties.Contains(propertyName))
                {
                    properties.Add(propertyName);

                    AriaOptionSettings optionsSettings = new AriaOptionSettings();

                    optionsSettings.VariableName = propertyName;

                    optionsSettings.FieldName = (string)row["mfld_name"];
                    optionsSettings.Description = (string)row["mfld_des"];
                    optionsSettings.Head = (string)row["cfld_head"];
                    optionsSettings.Message = (string)row["cfld_msg"];
                    if ((string)row["cdata_typ"] == "C") optionsSettings.DataType = AriaStandardDataTypes.String;
                    if ((string)row["cdata_typ"] == "N") optionsSettings.DataType = AriaStandardDataTypes.Numeric;
                    if ((string)row["cdata_typ"] == "D") optionsSettings.DataType = AriaStandardDataTypes.Date;
                    if ((string)row["cdata_typ"] == "L") optionsSettings.DataType = AriaStandardDataTypes.Logical;
                    optionsSettings.Width = (int)(decimal)row["nfld_wdth"];
                    optionsSettings.DecimalPlaces = (int)(Decimal)row["nfld_dec"];
                    optionsSettings.DefaultValue = (string)row["mdata_def"];
                    if (((string)row["cdefa_typ"]) == "V") optionsSettings.DefaultType = AriaOptionDefaultValueTypes.Fixed;
                    if (((string)row["cdefa_typ"]) == "E") optionsSettings.DefaultType = AriaOptionDefaultValueTypes.Expression;
                    optionsSettings.ValidExpression = (string)row["mvald_str"];
                    optionsSettings.Mask = (string)row["cpict_str"];
                    optionsSettings.ValidEntry = (bool)row["lvldentry"];
                    if (((string)row["mventries"]).IndexOf("~") > 0) optionsSettings.ValidEntries = ((string)row["mventries"]).Substring(0, ((string)row["mventries"]).IndexOf("~")).Split('|');
                    optionsSettings.Code = (string)row["cCodes_fld"];
                    optionsSettings.HideExpression = (string)row["msupexpr"];
                    optionsSettings.Editor = (string)row["cassociate"];
                    optionsSettings.BrowseFields = (string)row["mbrwfields"];
                    optionsSettings.BrowseField = (string)row["cbrwselfld"];
                    optionsSettings.BrowseFilter = (string)row["mbrwfltexp"];
                    optionsSettings.BrowseOpenFunction = (string)row["csetfunc"];
                    optionsSettings.BrowseSelectFunction = (string)row["csetfunc"];
                    optionsSettings.BrowseUnselectFunction = (string)row["csetfunc"];

                    command.Parameters.Clear();

                    command.CommandText = "INSERT INTO CustomObjectProperty(ObjectID, ObjectRevision, PropertyName, PropertyDescription, ModificationType, PropertyType, PropertySettings) Values (@ObjectID, @ObjectRevision, @PropertyName, @PropertyDescription, @ModificationType, @PropertyType, @PropertySettings)";
                    command.Parameters.AddWithValue("@ObjectID", optionsObjectID);
                    command.Parameters.AddWithValue("@ObjectRevision", ActiveRevision);
                    command.Parameters.AddWithValue("@PropertyName", propertyName);
                    command.Parameters.AddWithValue("@PropertyDescription", row["CFLD_HEAD"].ToString().TrimEnd());
                    command.Parameters.AddWithValue("@ModificationType", AriaModificationTypes.Add.ToString());
                    command.Parameters.AddWithValue("@PropertyType", AriaDataTypes.AriaOption.ToString());
                    command.Parameters.AddWithValue("@PropertySettings", Serializer.ConvertToXml(optionsSettings));
                    command.ExecuteNonQuery();
                }
            }

            command.Connection.Close();
        }

        public void RemoveReportFromAria5()
        {
            long objectID, parentObjectID;
            IsReportExist(out objectID, out parentObjectID);

            string optionGirdID = Reports.Rows[comboBoxReports.SelectedIndex]["CREP_ID"].ToString();

            OdbcCommand reportOptionsCom = new OdbcCommand("UPDATE SYDREPRT SET A5OBJNAM = '' WHERE CREP_ID = '" + optionGirdID);
            reportOptionsCom.Connection = new OdbcConnection(EnviromentVariables.Aria40SystemFilesConnectionString);
            reportOptionsCom.Connection.Open();
            ReportOptions.Load(reportOptionsCom.ExecuteReader());
            reportOptionsCom.Connection.Close();

            SqlCommand command = new SqlCommand();
            command.Connection = new SqlConnection(EnviromentVariables.Aria50ClientSystemFilesConnectionString);
            command.Connection.Open();
            
            command.Parameters.AddWithValue("@ObjectID", objectID);
            command.Parameters.AddWithValue("@ParentObjectID", parentObjectID);

            command.CommandText = "DELETE FROM CustomObjectProperty WHERE ObjectID = @ObjectID";
            command.ExecuteNonQuery();

            command.CommandText = "DELETE FROM CustomObjectMethodParameter WHERE ObjectID = @ParentObjectID";
            command.ExecuteNonQuery();

            command.CommandText = "DELETE FROM CustomObjectMethod WHERE ObjectID = @ParentObjectID";
            command.ExecuteNonQuery();

            command.CommandText = "DELETE FROM CustomObjectRevision WHERE ObjectID = @ParentObjectID";
            command.ExecuteNonQuery();

            command.CommandText = "DELETE FROM CustomObjectRevision WHERE ObjectID = @ObjectID";
            command.ExecuteNonQuery();

            command.CommandText = "DELETE FROM CustomObject WHERE ObjectID = @ParentObjectID";
            command.ExecuteNonQuery();

            command.CommandText = "DELETE FROM CustomObject WHERE ObjectID = @ObjectID";
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

            FillReports();
        }

        private void comboBoxReports_SelectedIndexChanged(object sender, EventArgs e)
        {
            FillReportsOptions();

            if (comboBoxReports.SelectedIndex == -1)
            {
                textBoxReportName.Text = "";
                textBoxReportName.ReadOnly = true;
                buttonAddToAria4.Enabled = false;
            }
            else
            {
                textBoxReportName.Text = Reports.Rows[comboBoxReports.SelectedIndex]["CREP_NAME"].ToString().TrimEnd();
                textBoxReportName.ReadOnly = false;
                buttonAddToAria4.Enabled = true;
            }
        }

        private void textBoxReportName_TextChanged(object sender, EventArgs e)
        {
            errorProvider.Clear();
        }

        private void buttonAddToAria4_Click(object sender, EventArgs e)
        {
            foreach (char c in textBoxReportName.Text.ToCharArray())
            {
                if (!char.IsLetterOrDigit(c) && c != ' ')
                {
                    errorProvider.SetError(textBoxReportName, "Report name cannot contains special character.");
                    return;
                }
            }

            SqlCommand command = new SqlCommand();
            command.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            command.Connection.Open();
            command.CommandText = "SELECT ObjectID FROM AriaObject WHERE ObjectName = @ObjectName";
            command.Parameters.AddWithValue("@ObjectName", textBoxAria4XP.Text + textBoxReportName.Text);
            DataTable report = new DataTable();
            report.Load(command.ExecuteReader());
            command.Connection.Close();

            if (report.Rows.Count > 0)
            {
                errorProvider.SetError(textBoxReportName, "Report name is used by another report.");
                return;
            }
            else
            {
                AddReportToAria5();
                MessageBox.Show("Report Sucessfully added to Aria5", "Aria5", MessageBoxButtons.OK, MessageBoxIcon.Information);
                DialogResult = DialogResult.OK;
            }
        }

        private void buttonClose_Click(object sender, EventArgs e)
        {
            Close();
        }
    }
}