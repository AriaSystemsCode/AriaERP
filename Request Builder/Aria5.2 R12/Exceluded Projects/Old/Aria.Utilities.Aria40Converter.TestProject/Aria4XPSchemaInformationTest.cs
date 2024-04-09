using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.Environment;
using Aria.DataTypes.ObjectDictionary;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.Xml;
using System.Text.RegularExpressions;
using System.Data;
using Aria.DataTypes.Settings;
using Aria.DataTypes;
using System.Data.Odbc;
using System.Xml;

using Aria.Utilities.Aria40Converter.Helpers;
using System.Windows.Forms;


namespace Aria.Utilities.Aria40Converter.TestProject
{


    /// <summary>
    ///This is a test class for Aria4XPSchemaInformationTest and is intended
    ///to contain all Aria4XPSchemaInformationTest Unit Tests
    ///</summary>
    [TestClass()]
    public class Aria4XPSchemaInformationTest
    {

        public DataTable DuplicatedAlgorithmWithoutRemove(DataTable Source, DataTable selected, string sortBy)
        {
            string flagFieldName = "";
            int flagfrist = 0;

            if (selected.Rows.Count == 0)
                selected = Source.Clone();
            DataRow[] rows = Source.Select("", sortBy);

            for (int i = 0; i < rows.Length; i++)
            {
                if (rows[i][sortBy].ToString().Trim() != "")
                {
                    if (rows[i][sortBy].ToString().Trim().ToUpper() == flagFieldName)
                    {
                        flagFieldName = rows[i][sortBy].ToString().Trim().ToUpper();
                        if (flagfrist == 0)
                        {
                            selected.ImportRow(rows[i - 1]);
                            flagfrist = 1;
                        }
                        selected.Rows.Add(rows[i].ItemArray);
                    }
                    else
                    {
                        flagfrist = 0;
                        flagFieldName = rows[i][sortBy].ToString().Trim().ToUpper();
                    }
                }
                else
                    selected.Rows.Add(rows[i].ItemArray);
            }

            return selected;
        }

        public DataTable DuplicatedAlgorithm(DataTable Source, DataTable selected, string sortBy)
        {
            string flagFieldName = "";
            int flagfrist = 0;

            if (selected.Rows.Count == 0)
                selected = Source.Clone();
            DataRow[] rows = Source.Select("", sortBy);

            for (int i = 0; i < rows.Length; i++)
            {
                if (rows[i][sortBy].ToString().Trim().RemoveSpecialChar() != "")
                {
                    if (rows[i][sortBy].ToString().Trim().ToUpper().RemoveSpecialChar() == flagFieldName)
                    {
                        flagFieldName = rows[i][sortBy].ToString().Trim().ToUpper().RemoveSpecialChar();
                        if (flagfrist == 0)
                        {
                            selected.ImportRow(rows[i - 1]);
                            flagfrist = 1;
                        }
                        selected.Rows.Add(rows[i].ItemArray);
                    }
                    else
                    {
                        flagfrist = 0;
                        flagFieldName = rows[i][sortBy].ToString().Trim().ToUpper().RemoveSpecialChar();
                    }
                }
                else
                    selected.Rows.Add(rows[i].ItemArray);
            }

            return selected;
        }

        //[TestMethod()]
        //public void CheckDuplicatedReportsName27()
        //{
        //    string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

        //    Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27);

        //    DataTable selected = new DataTable();

        //    selected = DuplicatedAlgorithm(schema.GetOptionGridDataTable(), selected, "crep_id").Copy();



        //    DataColumn col = new DataColumn();
        //    col.ColumnName = "ModeficationType";
        //    col.DefaultValue = "Remove";
        //    selected.Columns.Add(col);

        //    DataSet dt = new DataSet();
        //    dt.Tables.Add(selected);


        //    if (MessageBox.Show("Do you like to save doublicate  CheckDuplicatedReportsName27.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
        //    {
        //        SaveFileDialog dialog = new SaveFileDialog();
        //        dialog.DefaultExt = "xml";
        //        if (dialog.ShowDialog() == DialogResult.OK)
        //        {
        //            dt.WriteXml(dialog.FileName);
        //        }
        //    }

        //    Assert.IsTrue(selected.Rows.Count == 0);

        //    Assert.Inconclusive("Verify the correctness of this  CheckDuplicatedReportsName27.");

        //}


        //[TestMethod()]
        //public void CheckDuplicatedReportsTitle27()
        //{
        //    string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

        //    Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27);

        //    DataTable selected = new DataTable();

        //    selected = DuplicatedAlgorithm(schema.GetOptionGridDataTable(), selected, "crep_name").Copy();



        //    DataColumn col = new DataColumn();
        //    col.ColumnName = "ModeficationType";
        //    col.DefaultValue = "Remove";
        //    selected.Columns.Add(col);

        //    DataSet dt = new DataSet();
        //    dt.Tables.Add(selected);


        //    if (MessageBox.Show("Do you like to save doublicate  CheckDuplicatedReportsTitle27.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
        //    {
        //        SaveFileDialog dialog = new SaveFileDialog();
        //        dialog.DefaultExt = "xml";
        //        if (dialog.ShowDialog() == DialogResult.OK)
        //        {
        //            dt.WriteXml(dialog.FileName);
        //        }
        //    }

        //    Assert.IsTrue(selected.Rows.Count == 0);

        //    Assert.Inconclusive("Verify the correctness of this  CheckDuplicatedReportsTitle27.");

        //}

        //[TestMethod()]
        //public void CheckDuplicatedReportsName4Xp()
        //{
        //    string connectionString4Xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value

        //    Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString4Xp);

        //    DataTable selected = new DataTable();

        //    selected = DuplicatedAlgorithm(schema.GetOptionGridDataTable(), selected, "crep_id").Copy();



        //    DataColumn col = new DataColumn();
        //    col.ColumnName = "ModeficationType";
        //    col.DefaultValue = "Remove";
        //    selected.Columns.Add(col);

        //    DataSet dt = new DataSet();
        //    dt.Tables.Add(selected);


        //    if (MessageBox.Show("Do you like to save doublicate  CheckDuplicatedReportsName4Xp.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
        //    {
        //        SaveFileDialog dialog = new SaveFileDialog();
        //        dialog.DefaultExt = "xml";
        //        if (dialog.ShowDialog() == DialogResult.OK)
        //        {
        //            dt.WriteXml(dialog.FileName);
        //        }
        //    }

        //    Assert.IsTrue(selected.Rows.Count == 0);

        //    Assert.Inconclusive("Verify the correctness of this  CheckDuplicatedReportsName4Xp.");

        //}


        //[TestMethod()]
        //public void CheckDuplicatedReportsTitle4Xp()
        //{
        //    string connectionString4Xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value

        //    Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString4Xp);

        //    DataTable selected = new DataTable();

        //    selected = DuplicatedAlgorithm(schema.GetOptionGridDataTable(), selected, "crep_name").Copy();

        //    DataColumn col = new DataColumn();
        //    col.ColumnName = "ModeficationType";
        //    col.DefaultValue = "Remove";
        //    selected.Columns.Add(col);

        //    DataSet dt = new DataSet();
        //    dt.Tables.Add(selected);


        //    if (MessageBox.Show("Do you like to save doublicate  CheckDuplicatedReportsTitle4Xp.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
        //    {
        //        SaveFileDialog dialog = new SaveFileDialog();
        //        dialog.DefaultExt = "xml";
        //        if (dialog.ShowDialog() == DialogResult.OK)
        //        {
        //            dt.WriteXml(dialog.FileName);
        //        }
        //    }

        //    Assert.IsTrue(selected.Rows.Count == 0);

        //    Assert.Inconclusive("Verify the correctness of this  CheckDuplicatedReportsTitle4Xp.");

        //}


        //[TestMethod()]
        //public void CheckDuplicatedTablesIndeces27()
        //{
        //    string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

        //    Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27, ConnectionSrings.Aria27Merage);

        //    DataTable selected = new DataTable();
        //    selected.TableName = "Table";
        //    selected.Columns.Add("Cfile_nam");
        //    selected.Columns.Add("Expression");

        //    Table flag = new Table();
        //    int flagint = 0;
        //    int flag2 = 0;
        //    foreach (Table table in schema.GetTables().OrderBy(p => p.PrimaryKey.Expression))
        //    {
        //        if (flag2 != 0)
        //        {
        //            if (flag.PrimaryKey.Expression == table.PrimaryKey.Expression)
        //            {
        //                DataRow row = selected.NewRow();
        //                row["Cfile_nam"] = table.TableName;
        //                row["Expression"] = table.PrimaryKey.Expression;
        //                selected.Rows.Add(row);

        //                if (flagint == 0)
        //                {
        //                    DataRow row2 = selected.NewRow();
        //                    row2["Cfile_nam"] = flag.TableName;
        //                    row2["Expression"] = flag.PrimaryKey.Expression;

        //                    selected.Rows.Add(row2);
        //                    flagint = 1;
        //                }
        //            }
        //            else
        //            {
        //                flagint = 0;
        //            }
        //        }
        //        else
        //            flag2 = 1;
        //        flag = table;
        //    }

        //    DataColumn col = new DataColumn();
        //    col.ColumnName = "ModeficationType";
        //    col.DefaultValue = "Remove";
        //    selected.Columns.Add(col);

        //    DataSet dt = new DataSet();
        //    dt.Tables.Add(selected);


        //    if (MessageBox.Show("Do you like to save doublicate Aria27 CheckDuplicatedTablesIndeces27.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
        //    {
        //        SaveFileDialog dialog = new SaveFileDialog();
        //        dialog.DefaultExt = "xml";
        //        if (dialog.ShowDialog() == DialogResult.OK)
        //        {
        //            dt.WriteXml(dialog.FileName);
        //        }
        //    }

        //    Assert.IsTrue(selected.Rows.Count == 0);

        //    Assert.Inconclusive("Verify the correctness of this Aria27 CheckDuplicatedTablesIndeces27.");

        //}

        //[TestMethod()]
        //public void CheckDuplicatedTablesIndeces4Xp()
        //{
        //    string connectionString4Xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value

        //    Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString4Xp, ConnectionSrings.AriaXpMerage);

        //    DataTable selected = new DataTable();
        //    selected.TableName = "Table";
        //    selected.Columns.Add("Cfile_nam");
        //    selected.Columns.Add("Expression");

        //    Table flag = new Table();
        //    int flagint = 0;
        //    int flag2 = 0;
        //    foreach (Table table in schema.GetTables().OrderBy(p => p.PrimaryKey.Expression))
        //    {
        //        if (flag2 != 0)
        //        {
        //            if (flag.PrimaryKey.Expression == table.PrimaryKey.Expression)
        //            {
        //                DataRow row = selected.NewRow();
        //                row["Cfile_nam"] = table.TableName;
        //                row["Expression"] = table.PrimaryKey.Expression;
        //                selected.Rows.Add(row);

        //                if (flagint == 0)
        //                {
        //                    DataRow row2 = selected.NewRow();
        //                    row2["Cfile_nam"] = flag.TableName;
        //                    row2["Expression"] = flag.PrimaryKey.Expression;

        //                    selected.Rows.Add(row2);
        //                    flagint = 1;
        //                }
        //            }
        //            else
        //            {
        //                flagint = 0;
        //            }
        //        }
        //        else
        //            flag2 = 1;
        //        flag = table;
        //    }



        //    DataColumn col = new DataColumn();
        //    col.ColumnName = "ModeficationType";
        //    col.DefaultValue = "Remove";
        //    selected.Columns.Add(col);

        //    DataSet dt = new DataSet();
        //    dt.Tables.Add(selected);


        //    if (MessageBox.Show("Do you like to save doublicate Aria27 CheckDuplicatedTablesIndeces4Xp.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
        //    {
        //        SaveFileDialog dialog = new SaveFileDialog();
        //        dialog.DefaultExt = "xml";
        //        if (dialog.ShowDialog() == DialogResult.OK)
        //        {
        //            dt.WriteXml(dialog.FileName);
        //        }
        //    }

        //    Assert.IsTrue(selected.Rows.Count == 0);

        //    Assert.Inconclusive("Verify the correctness of this Aria27 CheckDuplicatedTablesIndeces4Xp.");

        //}


      
        // Validate 27 AND Check Duplicated Table Fields And Export it

        [TestMethod()]
        public void CheckDuplicateTableFieldsTitle27()
        {
            string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27, ConnectionSrings.Aria27);

            DataTable selected = new DataTable();
            selected.TableName = "Table";


            foreach (DataRow row in schema.DataAccess.GetTableDataTable().Rows)
            {
                OdbcCommand command = new OdbcCommand();
                command.CommandText = "SELECT  DISTINCT SYDFIELD.CFLD_NAME,SYDFIELD.CFLD_HEAD, sydflfld.nfld_pos ,sydflfld.cfile_nam FROM SYDFIELD join sydflfld on (SYDFIELD.cfld_name = sydflfld.cfld_name AND sydflfld.cFile_nam = '" + row["cfile_nam"] + "') WHERE SYDFIELD.cfld_name IN (SELECT cfld_name FROM sydflfld WHERE cFile_nam = '" + row["cfile_nam"] + "') ORDER BY cfld_head";
                command.Connection = new OdbcConnection(connectionString27);
                command.Connection.Open();

                OdbcDataAdapter adapter = new OdbcDataAdapter(command);
                DataTable table = new DataTable();

                adapter.Fill(table);

                command.Connection.Close();
                selected = DuplicatedAlgorithm(table, selected, "cfld_head").Copy();
            }

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);


            if (MessageBox.Show("Do you like to save doublicate Aria27 tables Fields Title.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria27 tables Fields Title not doublicated.");

        }


        /// <summary>
        /// Validate 4Xp AND Export Duplicated Data
        /// </summary>
        [TestMethod()]
        public void CheckDoublicateTablesTitle4Xp()
        {
            string connectionString4xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString4xp, ConnectionSrings.AriaXpMerage);
            DataTable selected = new DataTable();
            selected.TableName = "SYDFILES";
            selected = DuplicatedAlgorithm(schema.DataAccess.GetTableDataTable(), selected, "CFILE_TTL").Copy();

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            if (MessageBox.Show("Do you like to save doublicate Aria4XP tables Title.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    selected.WriteXml(dialog.FileName);
                }
            }


            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria4XP tables Title not doublicated.");
        }


        /// <summary>
        /// Validate 4Xp AND Export Duplicated Data
        /// </summary>
        [TestMethod()]
        public void CheckDoublicateTableNames4Xp()
        {
            string connectionString4xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString4xp, ConnectionSrings.AriaXpMerage);

            DataTable selected = new DataTable();
            selected.TableName = "SYDFILES";
            selected = DuplicatedAlgorithmWithoutRemove(schema.DataAccess.GetTableDataTable(), selected, "CFILE_NAM").Copy();

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);


            if (MessageBox.Show("Do you like to save doublicate Aria4XP tables.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    selected.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria4XP tables not doublicated.");
        }


        /// <summary>
        /// Validate 4Xp AND Check table PK missing And Export it
        /// </summary>
        [TestMethod()]
        public void CheckMissingTablePK4Xp()
        {
            string connectionString4Xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString4Xp, ConnectionSrings.AriaXpMerage);

            DataRow[] rows = schema.DataAccess.GetTableDataTable().Select("", "CFILE_TTL");

            DataTable selected = schema.DataAccess.GetTableDataTable().Clone();
            selected.TableName = "SYDFILES";

            foreach (DataRow row in rows)
            {
                string tableName = row["CFILE_NAM"].ToString().Trim();
                string pkIndexName = "";
                pkIndexName = row["CFILE_TAG"].ToString().ToUpper().Trim();
                Index pk = null;
                for (int i = 0; i < schema.GetTableIndexes(tableName).Count; i++)
                {
                    if (schema.GetTableIndexes(tableName)[i].IndexName.Trim() == pkIndexName)
                    {
                        pk = schema.GetTableIndexes(tableName)[i];
                        break;
                    }
                }
                if (pk == null)
                    selected.Rows.Add(row.ItemArray);

            }
            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);


            if (MessageBox.Show("Do you like to save Missing Aria4Xp tables PK.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    selected.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria4Xp tables PK not Missing.");

        }


        /// <summary>
        /// Validate 27 AND Export Duplicated Data
        /// </summary>
        [TestMethod()]
        public void CheckDoublicateTablesTitle27()
        {
            string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27, ConnectionSrings.Aria27Merage);

            DataTable selected = new DataTable();
            selected.TableName = "SYDFILES";
            selected = DuplicatedAlgorithm(schema.DataAccess.GetTableDataTable(), selected, "CFILE_TTL").Copy();

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);


            if (MessageBox.Show("Do you like to save doublicate Aria27 tables Title.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria27 tables Title not doublicated.");
        }


        /// <summary>
        /// Validate 27 AND Export Duplicated Data
        /// </summary>
        [TestMethod()]
        public void CheckDoublicateTables27()
        {
            string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27, ConnectionSrings.Aria27Merage);

            DataTable selected = new DataTable();
            selected.TableName = "SYDFILES";
            selected = DuplicatedAlgorithm(schema.DataAccess.GetTableDataTable(), selected, "CFILE_nam").Copy();

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);


            if (MessageBox.Show("Do you like to save doublicate Aria27 tables.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    selected.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria27 tables not doublicated.");
        }

        /// <summary>
        /// Validate 27 AND Check table PK missing And Export it
        /// </summary>
        [TestMethod()]
        public void CheckMissingTablePK27()
        {
            string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27, ConnectionSrings.Aria27Merage);

            DataRow[] rows = schema.DataAccess.GetTableDataTable().Select("", "");

            DataTable selected = schema.DataAccess.GetTableDataTable().Clone();
            selected.TableName = "SYDFILES";

            foreach (DataRow row in rows)
            {
                string tableName = row["CFILE_NAM"].ToString().Trim();

                Dictionary<string, string> aria5Dic = new Dictionary<string, string>();

                string pkIndexName = "";
                pkIndexName = row["CFILE_TAG"].ToString().ToUpper().Trim();

                Index pk = null;
                for (int i = 0; i < schema.GetTableIndexes(tableName).Count; i++)
                {
                    if (schema.GetTableIndexes(tableName)[i].IndexName.Trim() == pkIndexName)
                    {
                        pk = schema.GetTableIndexes(tableName)[i];
                        break;
                    }
                }
                if (pk == null)
                    selected.Rows.Add(row.ItemArray);

            }

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            if (MessageBox.Show("Do you like to save Missing Aria27 tables PK.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    selected.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria27 tables PK not Missing.");

        }



        [TestMethod()]
        public void CheckDuplicateTableFieldsTitle4Xp()
        {
            string connectionString4Xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString4Xp, ConnectionSrings.AriaXpMerage);

            DataTable selected = new DataTable();
            selected.TableName = "Table";

            foreach (DataRow row in schema.DataAccess.GetTableDataTable().Rows)
            {
                OdbcCommand command = new OdbcCommand();
                command.CommandText = "SELECT  DISTINCT SYDFIELD.CFLD_NAME,SYDFIELD.CFLD_HEAD, sydflfld.nfld_pos ,sydflfld.cfile_nam FROM SYDFIELD join sydflfld on (SYDFIELD.cfld_name = sydflfld.cfld_name AND sydflfld.cFile_nam = '" + row["cfile_nam"] + "') WHERE SYDFIELD.cfld_name IN (SELECT cfld_name FROM sydflfld WHERE cFile_nam = '" + row["cfile_nam"] + "') ORDER BY cfld_head";
                command.Connection = new OdbcConnection(connectionString4Xp);
                command.Connection.Open();

                OdbcDataAdapter adapter = new OdbcDataAdapter(command);
                DataTable table = new DataTable();

                adapter.Fill(table);

                command.Connection.Close();
                selected = DuplicatedAlgorithm(table, selected, "cfld_head").Copy();
            }

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);
            if (MessageBox.Show("Do you like to save doublicate Aria4Xp tables Fields Title.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    selected.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria4Xp tables Fields Title not doublicated.");

        }


        /// <summary>
        /// Validate 4Xp AND Check Duplicated Table Fields And Export it
        /// </summary>
        [TestMethod()]
        public void CheckDuplicateTableFieldsName4Xp()
        {
            string connectionString4Xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString4Xp, ConnectionSrings.AriaXpMerage);

            DataTable selected = new DataTable();
            selected.TableName = "Sydfield";

            selected = DuplicatedAlgorithmWithoutRemove(schema.DataAccess.GetFieldDataTable(), selected, "cfld_name").Copy();
            
            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);



            if (MessageBox.Show("Do you like to save doublicate Aria4Xp tables Fields .", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria4Xp tables Fields  not doublicated.");

        }


        //Validate 27 AND Check Duplicated Table Fields And Export it

        [TestMethod()]
        public void CheckDuplicateTableFieldsName27()
        {
            string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27, ConnectionSrings.Aria27Merage);

            DataTable selected = new DataTable();

            selected.TableName = "Sydfield";

            selected = DuplicatedAlgorithmWithoutRemove(schema.DataAccess.GetFieldDataTable(), selected, "cfld_name").Copy();
      

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);

            if (MessageBox.Show("Do you like to save doublicate Aria27 tables Fields .", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria27 tables Fields  not doublicated.");

        }



       
        [TestMethod()]
        public void CheckDuplicateReportIDs4Xp()
        {
            string connectionString4Xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString4Xp, ConnectionSrings.AriaXpMerage);

            DataTable selected = new DataTable();
            selected.TableName = "Sydreprt";

            selected = DuplicatedAlgorithm(schema.DataAccess.GetOptionGridDataTable(), selected, "crep_id").Copy();

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);


            if (MessageBox.Show("Do you like to save doublicate Aria4Xp ReportIDs4Xp .", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria4Xp  ReportIDs4Xp not doublicated.");

        }


        [TestMethod()]
        public void CheckDuplicateReportName4Xp()
        {
            string connectionString4Xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString4Xp, ConnectionSrings.AriaXpMerage);

            DataTable selected = new DataTable();
            selected.TableName = "Sydreprt";

            selected = DuplicatedAlgorithmWithoutRemove(schema.DataAccess.GetOptionGridDataTable(), selected, "crep_name").Copy();

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);



            if (MessageBox.Show("Do you like to save doublicate Aria4Xp ReportName4Xp .", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria4Xp  ReportName4Xp not doublicated.");

        }

        [TestMethod()]
        public void CheckDuplicateReportIDs27()
        {
            string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27, ConnectionSrings.Aria27Merage);

            DataTable selected = new DataTable();
            selected.TableName = "Sydreprt";

            selected = DuplicatedAlgorithm(schema.DataAccess.GetOptionGridDataTable(), selected, "crep_id").Copy();

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);


            if (MessageBox.Show("Do you like to save doublicate Aria4Xp ReportIDs27 .", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria4Xp  ReportIDs27 not doublicated.");

        }


        [TestMethod()]
        public void CheckDuplicateReportName27()
        {
            string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27, ConnectionSrings.Aria27Merage);

            DataTable selected = new DataTable();
            selected.TableName = "Sydreprt";

            selected = DuplicatedAlgorithmWithoutRemove(schema.DataAccess.GetOptionGridDataTable(), selected, "crep_name").Copy();

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);

            if (MessageBox.Show("Do you like to save doublicate Aria4Xp ReportName27 .", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria4Xp  ReportName27 not doublicated.");

        }


        //Export All Reports27 and exclude the convered
        [TestMethod()]
        public void ExportAllReports27()
        {
            string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27, ConnectionSrings.Aria27Merage);

            DataTable selected = new DataTable();
            selected.TableName = "Sydreprt";

            selected = schema.DataAccess.GetOptionGridDataTable().Copy();

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);



            if (MessageBox.Show("Do you like to save doublicate ExportAllReports27.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this Aria4Xp  RExportAllReports27");

        }

        //Export All Reports4Xp and exclude the convered
        [TestMethod()]
        public void ExportAllReports4Xp()
        {
            string connectionString4Xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString4Xp, ConnectionSrings.AriaXpMerage);

            DataTable selected = new DataTable();
            selected.TableName = "Sydreprt";

            selected = schema.DataAccess.GetOptionGridDataTable().Copy();

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);

            if (MessageBox.Show("Do you like to save doublicate ExportAllReports4Xp.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this  ExportAllReports4Xp");

        }

        
        [TestMethod()]
        public void CheckDuplicateReportVARHead27()
        {
            string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27, ConnectionSrings.Aria27Merage);

            DataTable selected = new DataTable();
            selected.TableName = "Sydreprt";

            foreach (DataRow row in schema.DataAccess.GetOptionGridDataTable().Rows)
            {
                OdbcCommand command = new OdbcCommand();
                command.CommandText = "SELECT DISTINCT syrepuvr.crep_ID,syrepuvr.cfld_head from syrepuvr join Sydreprt on syrepuvr.crep_ID = '" + row["crep_ID"] + "'";
                command.Connection = new OdbcConnection(connectionString27);
                command.Connection.Open();

                OdbcDataAdapter adapter = new OdbcDataAdapter(command);
                DataTable table = new DataTable();

                adapter.Fill(table);
                command.Connection.Close();
                selected = DuplicatedAlgorithm(table, selected, "cfld_head").Copy();
            }

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);

            if (MessageBox.Show("Do you like to save doublicate ReportVARHead27.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this  ReportVARHead27");

        }

        [TestMethod()]
        public void CheckDuplicateReportVARHead4Xp()
        {
            string connectionString4Xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value

            Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString4Xp, ConnectionSrings.AriaXpMerage);

            DataTable selected = new DataTable();
            selected.TableName = "Sydreprt";

            foreach (DataRow row in schema.DataAccess.GetOptionGridDataTable().Rows)
            {
                OdbcCommand command = new OdbcCommand();
                command.CommandText = "SELECT DISTINCT syrepuvr.crep_ID,syrepuvr.cfld_head from syrepuvr join Sydreprt on syrepuvr.crep_ID = '" + row["crep_ID"] + "'";
                command.Connection = new OdbcConnection(connectionString4Xp);
                command.Connection.Open();

                OdbcDataAdapter adapter = new OdbcDataAdapter(command);
                DataTable table = new DataTable();

                adapter.Fill(table);
                command.Connection.Close();
                selected = DuplicatedAlgorithm(table, selected, "cfld_head").Copy();
            }


            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            selected.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(selected);

            if (MessageBox.Show("Do you like to save doublicate ReportVARHead4Xp.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                SaveFileDialog dialog = new SaveFileDialog();
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            Assert.IsTrue(selected.Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this  ReportVARHead4Xp");

        }


        [TestMethod()]
        public void OldDectionary()
        {
            
            AriaDbCommand command7 = new AriaDbCommand("Select *  FROM Table_2", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            DataTable table = command7.GetDataTable();
 
            DataSet dt = new DataSet();
            dt.Tables.Add(table);

            SaveFileDialog dialog = new SaveFileDialog();
            if (MessageBox.Show("Do you like to save Export Old Dictionary", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName,XmlWriteMode.WriteSchema);
                }
            }
            Assert.IsTrue(dt.Tables[0].Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this no Export Old Dictionary.");

        }

        
        ///// <summary>
        ///// Validate SysFields Join Sysflfd
        ///// </summary>
        //[TestMethod()]
        //public void CheckSysFields27()
        //{
        //    string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value

        //    Aria27SchemaInformation schema = new Aria27SchemaInformation(connectionString27);

        //    DataTable table = new DataTable();
        //    table.TableName = "Table";
        //    table = schema.GetTableFieldDataTable().Copy();

        //    DataColumn col = new DataColumn();
        //    col.ColumnName = "ModeficationType";
        //    col.DefaultValue = "Remove";
        //    table.Columns.Add(col);

        //    DataSet dt = new DataSet();
        //    dt.Tables.Add(table);

        //    if (MessageBox.Show("Do you like to save  SydFields Join with Sydflfl.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
        //    {
        //        SaveFileDialog dialog = new SaveFileDialog();
        //        dialog.DefaultExt = "xml";
        //        if (dialog.ShowDialog() == DialogResult.OK)
        //        {
        //            dt.WriteXml(dialog.FileName);
        //        }
        //    }

        //    Assert.IsTrue(schema.GetTableFieldDataTable().Rows.Count == 0);

        //    Assert.Inconclusive("Verify the correctness of this SydFields Join with Sydflfl.");
        //}


        // //<summary>
        // //Validate 27 AND Check TablesIndeces27 And Export it
        // //</summary>
        //[TestMethod()]
        //public void TablesIndeces27()
        //{
        //    string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value
        //    Aria27SchemaInformation target = new Aria27SchemaInformation(connectionString27); // TODO: Initialize to an appropriate value

        //    List<Table> actual;

        //    actual = target.GetTables();

        //    DataTable selected = new DataTable();
        //    selected.Columns.Add("ParentTableName");
        //    selected.Columns.Add("ParentDescriptionName");
        //    selected.Columns.Add("Indeces");

        //    selected.TableName = "Table";
        //    selected = target.GetTableIndexDataTable().Copy();
        //    DataSet dt = new DataSet();
        //    dt.Tables.Add(selected);

        //    if (MessageBox.Show("Do you like to save TablesIndeces27.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
        //    {
        //        SaveFileDialog dialog = new SaveFileDialog();
        //        dialog.DefaultExt = "xml";
        //        if (dialog.ShowDialog() == DialogResult.OK)
        //        {
        //            dt.WriteXml(dialog.FileName);
        //        }
        //    }

        //    Assert.IsTrue(selected.Rows.Count == 0);

        //    Assert.Inconclusive("Verify the correctness of this no TablesIndeces27.");

        //}


        // //<summary>
        // //Validate 27 AND Check Duplicated Table Fields And Export it
        // //</summary>
        //[TestMethod()]
        //public void CheckTableChilderns()
        //{
        //    string connectionString4xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value
        //    string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value
        //    Aria4XPSchemaInformation target = new Aria4XPSchemaInformation(connectionString4xp, connectionString27); // TODO: Initialize to an appropriate value

        //    List<Table> actual;

        //    actual = target.GetTables();

        //    DataTable selected = new DataTable();
        //    selected.Columns.Add("ParentTableName");
        //    selected.Columns.Add("ParentDescriptionName");
        //    selected.Columns.Add("ChildTableName");
        //    selected.Columns.Add("ChildDescriptionName");

        //    selected.TableName = "Table";
        //    foreach (Table table in actual)
        //    {
        //        foreach (Table ChildTable in target.GetChildrenTables(table.TableName))
        //        {
        //            selected.Rows.Add(table.TableName, table.Description, ChildTable.TableName, ChildTable.Description);
        //        }
        //    }


        //    if (MessageBox.Show("Do you like to save Table Childern.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
        //    {
        //        SaveFileDialog dialog = new SaveFileDialog();
        //        dialog.DefaultExt = "xml";
        //        if (dialog.ShowDialog() == DialogResult.OK)
        //        {
        //            selected.WriteXml(dialog.FileName);
        //        }
        //    }

        //    Assert.IsTrue(selected.Rows.Count == 0);

        //    Assert.Inconclusive("Verify the correctness of this no Table Childern.");



        //}

        // //<summary>
        // //Validate 27 AND Check RelatedData And Export it
        // //</summary>
        //[TestMethod()]
        //public void CheckTableRelatedData()
        //{
        //    string connectionString4xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value
        //    string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value
        //    Aria4XPSchemaInformation target = new Aria4XPSchemaInformation(connectionString4xp, connectionString27); // TODO: Initialize to an appropriate value

        //    List<Table> actual;

        //    actual = target.GetTables();

        //    DataTable selected = new DataTable();
        //    selected.Columns.Add("ParentTableName");
        //    selected.Columns.Add("ParentDescriptionName");
        //    selected.Columns.Add("RelatedTableName");
        //    selected.Columns.Add("RelatedDescriptionName");

        //    selected.TableName = "Table";
        //    foreach (Table table in actual)
        //    {
        //        foreach (Table ChildTable in target.GetRelatedTables(table.TableName))
        //        {
        //            selected.Rows.Add(table.TableName, table.Description, ChildTable.TableName, ChildTable.Description);
        //        }
        //    }


        //    if (MessageBox.Show("Do you like to save Table Related Data.", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
        //    {
        //        SaveFileDialog dialog = new SaveFileDialog();
        //        dialog.DefaultExt = "xml";
        //        if (dialog.ShowDialog() == DialogResult.OK)
        //        {
        //            selected.WriteXml(dialog.FileName);
        //        }
        //    }

        //    Assert.IsTrue(selected.Rows.Count == 0);

        //    Assert.Inconclusive("Verify the correctness of this no Related Data.");

        //}

        // //<summary>
        // //Validate 27 AND Check RelatedData with New represention structure  And Export it
        // //</summary>

        //[TestMethod()]
        //public void CheckAllRelations()
        //{
        //    string connectionString4xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value
        //    string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value
        //    Aria4XPSchemaInformation target = new Aria4XPSchemaInformation(connectionString4xp, connectionString27); // TODO: Initialize to an appropriate value

        //    DataTable result = new DataTable();
        //    result = target.GetTableRelationDataTable().Copy();


        //    if (MessageBox.Show("Do you like to save All R", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
        //    {
        //        SaveFileDialog dialog = new SaveFileDialog();
        //        dialog.DefaultExt = "xml";
        //        if (dialog.ShowDialog() == DialogResult.OK)
        //        {
        //            result.WriteXml(dialog.FileName);
        //        }
        //    }

        //    Assert.IsTrue(result.Rows.Count == 0);

        //    Assert.Inconclusive("Verify the correctness of this no Related Data.");

        //}

        //<summary>
        //Validate 27 AND Check RelatedDataFields with New represention structure  And Export it
        //</summary>

        //[TestMethod()]
        //public void CheckAllRelationsFields()
        //{
        //    string connectionString4xp = ConnectionSrings.AriaXp; // TODO: Initialize to an appropriate value
        //    string connectionString27 = ConnectionSrings.Aria27; // TODO: Initialize to an appropriate value
        //    Aria4XPSchemaInformation target = new Aria4XPSchemaInformation(connectionString4xp, connectionString27); // TODO: Initialize to an appropriate value

        //    DataTable result = new DataTable();
        //    result = target.GetTableRelationFieldDataTable().Copy();


        //    if (MessageBox.Show("Do you like to save All Relations Fields", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
        //    {
        //        SaveFileDialog dialog = new SaveFileDialog();
        //        dialog.DefaultExt = "xml";
        //        if (dialog.ShowDialog() == DialogResult.OK)
        //        {
        //            result.WriteXml(dialog.FileName);
        //        }
        //    }

        //    Assert.IsTrue(result.Rows.Count == 0);

        //    Assert.Inconclusive("Verify the correctness of this no All Relations Fields.");

        //}


        [TestMethod()]
        public void Agant()
        {
            AriaDbCommand command7 = new AriaDbCommand("Select ObjectName , ObjectType  FROM AriaObject where ObjectType='Data' and ParentObjectID = 10355", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            DataTable table = command7.GetDataTable();
            table.Columns.Add("EXP");

            DataColumn col = new DataColumn();
            col.ColumnName = "ModeficationType";
            col.DefaultValue = "Remove";
            table.Columns.Add(col);

            DataSet dt = new DataSet();
            dt.Tables.Add(table);
            SaveFileDialog dialog = new SaveFileDialog();
            if (MessageBox.Show("Do you like to save  Agant", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }


            foreach (DataRow row in table.Rows)
            {
                AriaConditionList x = new AriaConditionList();
                if (row["ObjectName"].ToString() == "ARIA4XP.SalesOrderHeader")
                {
                    int qq = 0;
                }
                if (!string.IsNullOrEmpty(Agent.GetAgentRequest(new AriaDbConnection(), row["ObjectName"].ToString(), x, "99")))
                {
                    row["EXP"] = Agent.GetAgentRequest(new AriaDbConnection(), row["ObjectName"].ToString(), x, "99");
                    dt.WriteXml(dialog.FileName);
                }
                else
                {
                    int ff = 0;
                }
            }




            Assert.IsTrue(dt.Tables[0].Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this no Agant.");

        }

    }
}
