using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Aria.Utilities.Aria40Converter.AriaDictionaryTree;
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using Aria.Utilities.Aria40Converter.Properties;
using Aria.Data;
using Aria.Utilities.Aria40Converter;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.DataTypes.ObjectDictionary;
using Aria.Environment;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.Xml;
using System.Text.RegularExpressions;
using System.Data;
using Aria.DataTypes.Settings;
using Aria.DataTypes;
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using Aria.Utilities.Aria40Converter.Helpers;
using System.Data.Odbc;
using System.IO;
using Aria.Utilities.Aria40Converter.WhiteTesting;
using System.Data.SqlClient;


namespace Aria.Utilities.Aria40Converter
{
    public partial class ConvertWindow : Form
    {
        public ConvertWindow()
        {
            InitializeComponent();
        }

        private void ConvertWindow_Load(object sender, EventArgs e)
        {
        }

        private void btnAria27Path_Click(object sender, EventArgs e)
        {
            folderBrowserDialog1.ShowDialog();
            txtAria27Path.Text = folderBrowserDialog1.SelectedPath;
        }

        private void btnAria4Xp_Click(object sender, EventArgs e)
        {
            folderBrowserDialog1.ShowDialog();
            txt4XpPath.Text = folderBrowserDialog1.SelectedPath;
        }

        private void btnAria27Merger_Click(object sender, EventArgs e)
        {
            folderBrowserDialog1.ShowDialog();
            txtAria27Merge.Text = folderBrowserDialog1.SelectedPath;
        }

        private void btnAria4XpMerge_Click(object sender, EventArgs e)
        {
            folderBrowserDialog1.ShowDialog();
            txtAria4XpMerge.Text = folderBrowserDialog1.SelectedPath;
        }

        Aria50SchemaInformation schema = null;

        private void btnConvert_Click(object sender, EventArgs e)
        {
            MessageBox.Show("Please note this task may take few minutes.", "Convert", MessageBoxButtons.OK);

            AriaDbCommand command7 = new AriaDbCommand("DELETE  FROM AriaObjectMethodParameter where ObjectID IN(select objectID from AriaObject where ObjectName like 'Aria4Xp%')", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command7.ExecuteNonQuery();

            AriaDbCommand command6 = new AriaDbCommand("DELETE  FROM AriaObjectMethod  where ObjectID IN(select objectID from AriaObject where ObjectName like 'Aria4Xp%')", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command6.ExecuteNonQuery();

            AriaDbCommand command5 = new AriaDbCommand("DELETE  FROM AriaObjectEventParameter  where ObjectID IN(select objectID from AriaObject where ObjectName like 'Aria4Xp%')", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command5.ExecuteNonQuery();

            AriaDbCommand command4 = new AriaDbCommand("DELETE  FROM AriaObjectEvent  where ObjectID IN(select objectID from AriaObject where ObjectName like 'Aria4Xp%')", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command4.ExecuteNonQuery();

            AriaDbCommand command3 = new AriaDbCommand("DELETE  FROM AriaObjectProperty  where ObjectID IN(select objectID from AriaObject where ObjectName like 'Aria4Xp%')", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command3.ExecuteNonQuery();

            AriaDbCommand command2 = new AriaDbCommand("DELETE  FROM AriaObjectRevision  where ObjectID IN(select objectID from AriaObject where ObjectName like 'Aria4Xp%')", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command2.ExecuteNonQuery();

            AriaDbCommand command1 = new AriaDbCommand("DELETE  FROM AriaObject where ObjectName like 'Aria4Xp%'", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command1.ExecuteNonQuery();

            if (schema == null)
            {
                schema = new Aria50SchemaInformation("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes", "Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txtAria27Path.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes", @"MergeData\Aria4XP\", @"MergeData\Aria27\");
            }
            else
            {
                schema.Refresh();
            }

            schema.CreatingSystemMasterType = CreatingSystemMasterTypes.Merge;

            AriaPackageObjects root = new AriaPackageObjects(schema);
            root.CreateChildren();

            root.Save();

            schema.SaveAll();

            MessageBox.Show("Converting completed successfully.", "Convert", MessageBoxButtons.OK);

        }

        private void btnGenerateA27XML_Click(object sender, EventArgs e)
        {
            File.Delete(Path.Combine(txtAria27Merge.Text, "SYDFILES.XML"));
            File.Delete(Path.Combine(txtAria27Merge.Text, "SYDFIELD.XML"));
            File.Delete(Path.Combine(txtAria27Merge.Text, "SYDINDEX.XML"));
            File.Delete(Path.Combine(txtAria27Merge.Text, "SYDFLFLD.XML"));

            AriaDataAccessFilesException exp = new AriaDataAccessFilesException("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txtAria27Path.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes", @"MergeData\Aria27\", txtAria27Merge.Text);
            exp.CreateDoublicateTableNameXml();
            exp.CreateDoublicateTablesTitleXml();
            exp.CreateCustomTableXml();
            exp.CreateDuplicateTableFieldsNameXml();
            exp.CreateDuplicateTableFieldsTitleXml();
            exp.CreateMissingTablePKXml();

            exp.CreateCustomTableXml();
            exp.CreateCustomFieldXml();
            exp.CreateCustomIndexXml();
            exp.CreateCustomTableFieldXml();

            MessageBox.Show("Generating completed successfully.", "Convert", MessageBoxButtons.OK);

        }

        private void btnGenerateA4XPXML_Click(object sender, EventArgs e)
        {
            File.Delete(Path.Combine(txtAria4XpMerge.Text, "SYDFILES.XML"));
            File.Delete(Path.Combine(txtAria4XpMerge.Text, "SYDFIELD.XML"));
            File.Delete(Path.Combine(txtAria4XpMerge.Text, "SYDINDEX.XML"));
            File.Delete(Path.Combine(txtAria4XpMerge.Text, "SYDFLFLD.XML"));
            File.Delete(Path.Combine(txtAria4XpMerge.Text, "SYDREPRT.XML"));
            File.Delete(Path.Combine(txtAria4XpMerge.Text, "SYREPUVR.XML"));

            AriaDataAccessFilesException exp = new AriaDataAccessFilesException("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes", @"MergeData\Aria4XP\", txtAria4XpMerge.Text);
            exp.CreateDoublicateTableNameXml();
            exp.CreateDoublicateTablesTitleXml();
            exp.CreateDuplicateTableFieldsNameXml();
            exp.CreateDuplicateTableFieldsTitleXml();
            exp.CreateMissingTablePKXml();
            exp.CreateAllReportsXml();
            exp.CreateAllReportsVariablesXml();

            exp.CreateCustomTableXml();
            exp.CreateCustomFieldXml();
            exp.CreateCustomIndexXml();
            exp.CreateCustomTableFieldXml();

            MessageBox.Show("Generating completed successfully.", "Convert", MessageBoxButtons.OK);
        }

        private void btnFixA4XPDic_Click(object sender, EventArgs e)
        {
            vfputility.vfputility x = new vfputility.vfputility();
            object p1, p2;

            try
            {
                p1 = @"ALTER TABLE " + Path.Combine(txt4XpPath.Text, "sydobjct") + " ADD COLUMN A5OBJNAM c(100)";
                p2 = false;
                x.Execute(ref p1, ref p2);

                p1 = @"ALTER TABLE " + Path.Combine(txt4XpPath.Text, "sydreprt") + " ADD COLUMN A5OBJNAM c(100)";
                x.Execute(ref p1, ref p2);

                p1 = @"CLOSE ALL";
                x.Execute(ref p1, ref p2);
            }
            catch
            {
            }
            
            AriaDbCommand ariaObjectCommand = new AriaDbCommand("Select * FROM AriaObject", new AriaDbConnection("", ""),
                                                                Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            DataTable MasterTable = ariaObjectCommand.GetDataTable();

            AriaDbCommand ariaAriaObjectBarsCommand = new AriaDbCommand("Select * FROM AriaObjectBars", new AriaDbConnection("", ""),
                                                                        Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            DataTable AriaObjectBarsTable = ariaAriaObjectBarsCommand.GetDataTable();

            OdbcCommand sydObjectCommand = new OdbcCommand();
            sydObjectCommand.CommandText = "SELECT * FROM sydobjct";
            sydObjectCommand.Connection = new OdbcConnection("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");
            sydObjectCommand.Connection.Open();
            OdbcDataAdapter sydObjectAdapter = new OdbcDataAdapter(sydObjectCommand);
            DataTable sydObjectTable = new DataTable();
            sydObjectAdapter.Fill(sydObjectTable);
            sydObjectCommand.Connection.Close();

            OdbcCommand sydReportCommand = new OdbcCommand();
            sydReportCommand.CommandText = "SELECT * FROM sydreprt";
            sydReportCommand.Connection = new OdbcConnection("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");
            sydReportCommand.Connection.Open();
            OdbcDataAdapter sydReportAdapter = new OdbcDataAdapter(sydReportCommand);
            DataTable sydReportTable = new DataTable();
            sydReportAdapter.Fill(sydReportTable);
            sydReportCommand.Connection.Close();

            DataSet oldDictionaryTableDataSet = new DataSet();
            DataTable oldDictionaryTable = new DataTable();
            oldDictionaryTableDataSet.ReadXml(Path.Combine(Application.StartupPath, @"MergeData\Aria5\System.Master.Old.xml"));
            oldDictionaryTable = oldDictionaryTableDataSet.Tables[0];

            MessageBox.Show("Updating SYDOBJCT.", "Convert", MessageBoxButtons.OK);

            foreach (DataRow sydObjectRow in sydObjectTable.Rows)
            {
                if (string.IsNullOrEmpty(sydObjectRow["A5OBJNAM"].ToString().Trim()))
                {
                    if (!string.IsNullOrEmpty(sydObjectRow["CARIA5ID"].ToString().Trim()))
                    {
                        if (oldDictionaryTable.Select("ObjectID = '" + sydObjectRow["CARIA5ID"] + "'").Count() > 0)
                        {
                            string objectName = oldDictionaryTable.Select("ObjectID = '" + sydObjectRow["CARIA5ID"] + "'").First()["ObjectName"].ToString().Trim();

                            if (MasterTable.Select("ObjectName = '" + objectName + "'").Count() > 0)
                            {
                                string objectID = MasterTable.Select("ObjectName = '" + objectName + "'").First()["ObjectID"].ToString();

                                OdbcCommand updateSydObjectCommand = new OdbcCommand();
                                updateSydObjectCommand.CommandText = "UPDATE sydobjct SET " +
                                                                     "A5OBJNAM = '" + objectName + "'," +
                                                                     "CARIA5ID ='" + objectID + "' " +
                                                                     "WHERE CARIA5ID= '" + sydObjectRow["CARIA5ID"].ToString() + "'";
                                updateSydObjectCommand.Connection = new OdbcConnection("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");
                                updateSydObjectCommand.Connection.Open();
                                updateSydObjectCommand.ExecuteNonQuery();
                                updateSydObjectCommand.Connection.Close();
                            }
                            else
                            {
                                if (MessageBox.Show("ObjectName = " + objectName.Trim() + " is not exist in new dictionary do you want to remove it?", "Convert", MessageBoxButtons.YesNo, MessageBoxIcon.Stop, MessageBoxDefaultButton.Button2) == DialogResult.Yes)
                                {
                                    OdbcCommand updateSydObjectCommand = new OdbcCommand();
                                    updateSydObjectCommand.CommandText = "UPDATE sydobjct SET A5OBJNAM = '', CARIA5ID  = '' " +
                                                                         "WHERE CARIA5ID = '" + sydObjectRow["CARIA5ID"].ToString() + "'";
                                    updateSydObjectCommand.Connection = new OdbcConnection("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");
                                    updateSydObjectCommand.Connection.Open();
                                    updateSydObjectCommand.ExecuteNonQuery();
                                    updateSydObjectCommand.Connection.Close();
                                }
                            }
                        }
                        else
                        {
                            if (MessageBox.Show("ObjectID = " + sydObjectRow["CARIA5ID"].ToString().Trim() + " is not exist in old dictionary do you want to remove it?", "Convert", MessageBoxButtons.YesNo, MessageBoxIcon.Stop, MessageBoxDefaultButton.Button2) == DialogResult.Yes)
                            {
                                OdbcCommand updateSydObjectCommand = new OdbcCommand();
                                updateSydObjectCommand.CommandText = "UPDATE sydobjct SET A5OBJNAM = '', CARIA5ID  = '' " +
                                                                     "WHERE CARIA5ID = '" + sydObjectRow["CARIA5ID"].ToString() + "'";
                                updateSydObjectCommand.Connection = new OdbcConnection("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");
                                updateSydObjectCommand.Connection.Open();
                                updateSydObjectCommand.ExecuteNonQuery();
                                updateSydObjectCommand.Connection.Close();
                            }
                        }
                    }
                }
                else
                {
                    if (MasterTable.Select("ObjectName = '" + sydObjectRow["A5OBJNAM"].ToString().Trim() + "'").Count() == 0)
                    {
                        if (MessageBox.Show("ObjectID = " + sydObjectRow["CARIA5ID"].ToString().Trim() + " is not exist in new dictionary do you want to remove it?", "Convert", MessageBoxButtons.YesNo, MessageBoxIcon.Stop, MessageBoxDefaultButton.Button2) == DialogResult.Yes)
                        {
                            OdbcCommand updateSydObjectCommand = new OdbcCommand();
                            updateSydObjectCommand.CommandText = "UPDATE sydobjct SET A5OBJNAM = '', CARIA5ID  = '' " +
                                                                 "WHERE CARIA5ID = '" + sydObjectRow["CARIA5ID"].ToString() + "'";
                            updateSydObjectCommand.Connection = new OdbcConnection("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");
                            updateSydObjectCommand.Connection.Open();
                            updateSydObjectCommand.ExecuteNonQuery();
                            updateSydObjectCommand.Connection.Close();
                        }
                    }
                    else
                    {
                        OdbcCommand updateSydObjectCommand = new OdbcCommand();
                        updateSydObjectCommand.CommandText = "UPDATE sydobjct SET CARIA5ID  =  '" + MasterTable.Select("ObjectName = '" + sydObjectRow["A5OBJNAM"].ToString().Trim() + "'")[0]["ObjectID"].ToString() + "'" +
                                                             " WHERE CARIA5ID = '" + sydObjectRow["CARIA5ID"].ToString() + "'";
                        updateSydObjectCommand.Connection = new OdbcConnection("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");
                        updateSydObjectCommand.Connection.Open();
                        updateSydObjectCommand.ExecuteNonQuery();
                        updateSydObjectCommand.Connection.Close();
                    }
                }
            }

            MessageBox.Show("Updating SYDREPRT.", "Convert", MessageBoxButtons.OK);

            foreach (DataRow sydReportRow in sydReportTable.Rows)
            {
                if (string.IsNullOrEmpty(sydReportRow["A5OBJNAM"].ToString().Trim()))
                {
                    if (!string.IsNullOrEmpty(sydReportRow["CARIA5ID"].ToString().Trim()))
                    {
                        if (oldDictionaryTable.Select("ObjectID = '" + sydReportRow["CARIA5ID"] + "'").Count() > 0)
                        {
                            string objectName = oldDictionaryTable.Select("ObjectID = '" + sydReportRow["CARIA5ID"] + "'").First()["ObjectName"].ToString().Trim();

                            if (MasterTable.Select("ObjectName = '" + objectName + "'").Count() > 0)
                            {
                                string objectID = MasterTable.Select("ObjectName = '" + objectName + "'").First()["ObjectID"].ToString();

                                OdbcCommand updatesydReportCommand = new OdbcCommand();
                                updatesydReportCommand.CommandText = "UPDATE sydReprt SET " +
                                                                     "A5OBJNAM = '" + objectName + "'," +
                                                                     "CARIA5ID ='" + objectID + "' " +
                                                                     "WHERE CARIA5ID= '" + sydReportRow["CARIA5ID"].ToString() + "'";
                                updatesydReportCommand.Connection = new OdbcConnection("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");
                                updatesydReportCommand.Connection.Open();
                                updatesydReportCommand.ExecuteNonQuery();
                                updatesydReportCommand.Connection.Close();
                            }
                            else
                            {
                                if (MessageBox.Show("ObjectName = " + objectName.Trim() + " is not exist in new dictionary do you want to remove it?", "Convert", MessageBoxButtons.YesNo, MessageBoxIcon.Stop, MessageBoxDefaultButton.Button2) == DialogResult.Yes)
                                {
                                    OdbcCommand updatesydReportCommand = new OdbcCommand();
                                    updatesydReportCommand.CommandText = "UPDATE sydReprt SET A5OBJNAM = '', CARIA5ID  = '' " +
                                                                         "WHERE CARIA5ID = '" + sydReportRow["CARIA5ID"].ToString() + "'";
                                    updatesydReportCommand.Connection = new OdbcConnection("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");
                                    updatesydReportCommand.Connection.Open();
                                    updatesydReportCommand.ExecuteNonQuery();
                                    updatesydReportCommand.Connection.Close();
                                }
                            }
                        }
                        else
                        {
                            if (MessageBox.Show("ObjectID = " + sydReportRow["CARIA5ID"].ToString().Trim() + " is not exist in old dictionary do you want to remove it?", "Convert", MessageBoxButtons.YesNo, MessageBoxIcon.Stop, MessageBoxDefaultButton.Button2) == DialogResult.Yes)
                            {
                                OdbcCommand updatesydReportCommand = new OdbcCommand();
                                updatesydReportCommand.CommandText = "UPDATE sydReprt SET A5OBJNAM = '', CARIA5ID  = '' " +
                                                                     "WHERE CARIA5ID = '" + sydReportRow["CARIA5ID"].ToString() + "'";
                                updatesydReportCommand.Connection = new OdbcConnection("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");
                                updatesydReportCommand.Connection.Open();
                                updatesydReportCommand.ExecuteNonQuery();
                                updatesydReportCommand.Connection.Close();
                            }
                        }
                    }
                }
                else
                {
                    if (MasterTable.Select("ObjectName = '" + sydReportRow["A5OBJNAM"].ToString().Trim() + "'").Count() == 0)
                    {
                        if (MessageBox.Show("ObjectID = " + sydReportRow["CARIA5ID"].ToString().Trim() + " is not exist in new dictionary do you want to remove it?", "Convert", MessageBoxButtons.YesNo, MessageBoxIcon.Stop, MessageBoxDefaultButton.Button2) == DialogResult.Yes)
                        {
                            OdbcCommand updatesydReportCommand = new OdbcCommand();
                            updatesydReportCommand.CommandText = "UPDATE sydReprt SET A5OBJNAM = '', CARIA5ID  = '' " +
                                                                 " WHERE CARIA5ID = '" + sydReportRow["CARIA5ID"].ToString() + "'";
                            updatesydReportCommand.Connection = new OdbcConnection("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");
                            updatesydReportCommand.Connection.Open();
                            updatesydReportCommand.ExecuteNonQuery();
                            updatesydReportCommand.Connection.Close();
                        }
                    }
                    else
                    {
                        OdbcCommand updatesydReportCommand = new OdbcCommand();
                        updatesydReportCommand.CommandText = "UPDATE sydReprt SET CARIA5ID  =  '" + MasterTable.Select("ObjectName = '" + sydReportRow["A5OBJNAM"].ToString().Trim() + "'")[0]["ObjectID"].ToString() +  "'" + 
                                                             "WHERE CARIA5ID = '" + sydReportRow["CARIA5ID"].ToString() + "'";
                        updatesydReportCommand.Connection = new OdbcConnection("Driver={Microsoft Visual FoxPro Driver};sourcedb=" + txt4XpPath.Text + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes");
                        updatesydReportCommand.Connection.Open();
                        updatesydReportCommand.ExecuteNonQuery();
                        updatesydReportCommand.Connection.Close();
                    }
                }
            }

            MessageBox.Show("Updating AriaObjectBars.", "Convert", MessageBoxButtons.OK);

            foreach (DataRow ObjectBarsRow in AriaObjectBarsTable.Rows)
            {
                if (ObjectBarsRow["ObjectName"] == DBNull.Value || ObjectBarsRow["ObjectName"].ToString().Trim().Length == 0)
                {
                    if (oldDictionaryTable.Select("ObjectID = '" + ObjectBarsRow["ObjectID"] + "'").Count() > 0)
                    {
                        string objectName = oldDictionaryTable.Select("ObjectID = '" + ObjectBarsRow["ObjectID"].ToString() + "'").First()["ObjectName"].ToString().Trim();

                        if (MasterTable.Select("ObjectName = '" + objectName + "'").Count() > 0)
                        {
                            AriaDbCommand updateObjectBarsCommand = new AriaDbCommand("UPDATE AriaObjectBars SET  ObjectID = " + MasterTable.Select("ObjectName = '" + objectName + "'")[0]["ObjectID"].ToString() + ", ParentObjectID = " + MasterTable.Select("ObjectName = '" + objectName + "'")[0]["ParentObjectID"].ToString() + ", ObjectName = '" + objectName + "' WHERE ObjectID = " + ObjectBarsRow["ObjectID"].ToString(), new AriaDbConnection("", ""),
                                                                    Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
                            updateObjectBarsCommand.ExecuteNonQuery();
                        }
                    }
                }
                else
                {
                    if (MasterTable.Select("ObjectName = '" + ObjectBarsRow["ObjectName"].ToString().Trim() + "'").Count() == 0)
                    {
                        if (MessageBox.Show("ObjectID = " + ObjectBarsRow["ObjectID"].ToString().Trim() + " is not exist in new dictionary do you want to remove it?", "Convert", MessageBoxButtons.YesNo, MessageBoxIcon.Stop, MessageBoxDefaultButton.Button2) == DialogResult.Yes)
                        {
                            AriaDbCommand updateObjectBarsCommand = new AriaDbCommand("UPDATE AriaObjectBars SET ObjectName = NULL WHERE ObjectID = " + ObjectBarsRow["ObjectID"].ToString(), new AriaDbConnection("", ""),
                                                                    Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
                            updateObjectBarsCommand.ExecuteNonQuery();
                        }
                    }
                    else
                    {
                        AriaDbCommand updateObjectBarsCommand = new AriaDbCommand("UPDATE AriaObjectBars SET  ObjectID = " + MasterTable.Select("ObjectName = '" + ObjectBarsRow["ObjectName"].ToString().Trim() + "'")[0]["ObjectID"].ToString() + ", ParentObjectID = " + MasterTable.Select("ObjectName = '" + ObjectBarsRow["ObjectName"].ToString().Trim() + "'")[0]["ParentObjectID"].ToString() + " WHERE ObjectID = " + ObjectBarsRow["ObjectID"].ToString(), new AriaDbConnection("", ""),
                                                                Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
                        updateObjectBarsCommand.ExecuteNonQuery();
                    }
                }
            }

            MessageBox.Show("Fixing completed successfully.", "Convert", MessageBoxButtons.OK);
        }

        private void btnAria50Dic_Click(object sender, EventArgs e)
        {
            MessageBox.Show("Please note this task may take few minutes.", "Dictionary View", MessageBoxButtons.OK);

            Aria.ObjectDictionary.UI.AriaObjectDictionaryUI objectViewer = new Aria.ObjectDictionary.UI.AriaObjectDictionaryUI();
            objectViewer.showObjectDictionaryUI();
        }

        private void btnClose_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void btnAgent_Click(object sender, EventArgs e)
        {
            MessageBox.Show("Please note this task may take few minutes.", "Convert", MessageBoxButtons.OK);

            AriaRequestAgentWhiteTest agent = new AriaRequestAgentWhiteTest();

            DataTable result = agent.TestAriaRequestAgent(txtClientID.Text,txtCompany.Text);
            
            SaveFileDialog dialog = new SaveFileDialog();
            if (MessageBox.Show("Do you like to save  Agant testing result?", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    result.WriteXml(dialog.FileName);
                }
            }
        }

        private void btnTestParamSub_Click(object sender, EventArgs e)
        {
            MessageBox.Show("Please note this task may take few minutes.", "Convert", MessageBoxButtons.OK);

            AriaDataObjectPointerAdapterWhiteTest PointerAdapter = new AriaDataObjectPointerAdapterWhiteTest();

            DataTable result = PointerAdapter.ariaDataObjectPointerAdapter(txtClientID.Text, txtCompany.Text);

            SaveFileDialog dialog = new SaveFileDialog();
            if (MessageBox.Show("Do you like to save  Parameter Substitution result?", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                result.TableName = "Point";
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    result.WriteXml(dialog.FileName);
                }
            }
        }

        private void button1_Click(object sender, EventArgs e)
        {
            FormAriaObject x = new FormAriaObject();
            x.ShowDialog();
        }

        private void button2_Click(object sender, EventArgs e)
        {
            FormAriaObjectProperty x = new FormAriaObjectProperty();
            x.ShowDialog();
        }
   }
}
