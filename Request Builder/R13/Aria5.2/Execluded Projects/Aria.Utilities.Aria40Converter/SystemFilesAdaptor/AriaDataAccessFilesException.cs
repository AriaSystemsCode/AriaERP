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
using System.Collections;
using Aria.Utilities.Aria40Converter.Helpers;
using System.Windows.Forms;
using System.IO;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public class AriaDataAccessFilesException
    {
        private string _connectionString, _mergePath, _exceptionPath;

        public AriaDataAccessFilesException(string connectionString, string mergePath, string exceptionPath)
        {
            _connectionString = connectionString;
            _mergePath = mergePath;
            _exceptionPath = exceptionPath;
        }

        public DataTable GetDuplicatedRows(DataTable Source, string sortBy, string effect)
        {
            Source.Columns.Add("_Doublicated");

            DataRow previousRow = null;
            string previousSum = "";

            foreach (DataRow row in Source.Select("", sortBy))
            {
                string currentSum = "";
                for (int i = 0; i < sortBy.Split(',').Count(); i++)
                {
                    if (effect.Split(',')[i].Trim() == "RSC")
                    {
                        currentSum += row[sortBy.Split(',')[i].Trim()].ToString().RemoveSpecialChar();
                    }
                    else
                    {
                        currentSum += row[sortBy.Split(',')[i].Trim()].ToString();
                    }
                }

                if (currentSum.Trim().ToUpper() == previousSum.Trim().ToUpper())
                {
                    if (previousRow != null) previousRow["_Doublicated"] = "Yes";
                    row["_Doublicated"] = "Yes";
                }

                previousRow = row;
                previousSum = currentSum;
            }

            DataTable result = Source.Clone();

            foreach (DataRow row in Source.Select("_Doublicated = 'Yes'"))
            {
                result.Rows.Add(row.ItemArray);
            }

            Source.Columns.Remove("_Doublicated");
            result.Columns.Remove("_Doublicated");

            return result;
        }

        // SYDFILES-Duplicated Tables Name.xml
        public void CreateDoublicateTableNameXml()
        {
            AriaDataAccessFiles dataAccess = new AriaDataAccessFiles(_connectionString, _mergePath);

            DataTable selected = GetDuplicatedRows(dataAccess.GetTableDataTable(), "CFILE_NAM", "");
            selected.TableName = "SYDFILES";

            if (selected.Rows.Count > 0) MessageBox.Show("Doublicated Tables Name Exists.", "Exception", MessageBoxButtons.OK, MessageBoxIcon.Error);

            foreach (DataRow row in selected.Rows)
            {
                Helpers.AppendRecordToException(Path.Combine(Path.Combine(Application.StartupPath, _exceptionPath), "SYDFILES.XML"), row, "CFILE_NAM", "Doblicated Table Name");
            }
        }

        // SYDFILES-Duplicated Tables Title.xml
        public void CreateDoublicateTablesTitleXml()
        {
            AriaDataAccessFiles dataAccess = new AriaDataAccessFiles(_connectionString, _mergePath);

            DataTable selected = GetDuplicatedRows(dataAccess.GetTableDataTable(), "CFILE_TTL", "RSC");
            selected.TableName = "SYDFILES";

            if (selected.Select("CFILE_NAM = 'CODES'").Count() > 0)
            {
                selected.Rows.Remove(selected.Select("CFILE_NAM = 'CODES'")[0]);
            }

            if (selected.Rows.Count > 0) MessageBox.Show("Doublicated Tables Title Exists.", "Exception", MessageBoxButtons.OK, MessageBoxIcon.Error);

            foreach (DataRow row in selected.Rows)
            {
                Helpers.AppendRecordToException(Path.Combine(Path.Combine(Application.StartupPath, _exceptionPath), "SYDFILES.XML"), row, "CFILE_NAM", "Doblicated Table Title");
            }
        }

        // SYDFILES-Missing Tables Primary Key.xml
        public void CreateMissingTablePKXml()
        {
            AriaDataAccessFiles dataAccess = new AriaDataAccessFiles(_connectionString, _mergePath);

            DataRow[] rows = dataAccess.GetTableDataTable().Select("", "CFILE_TTL");

            DataTable indexRows = dataAccess.GetTableIndexDataTable();
            
            DataTable selected = dataAccess.GetTableDataTable().Clone();
            selected.TableName = "SYDFILES";

            foreach (DataRow row in rows)
            {
                string tableName = row["CFILE_NAM"].ToString().Trim();
                string pkIndexName = "";
                pkIndexName = row["CFILE_TAG"].ToString().ToUpper().Trim();

                if (string.IsNullOrEmpty(pkIndexName.Trim()))
                {
                    selected.Rows.Add(row.ItemArray);
                }
                else
                {
                    if (indexRows.Select("CFILE_NAM = '" + tableName + "' AND CFILE_TAG = '" + pkIndexName + "'").Count() != 1)
                    {
                        selected.Rows.Add(row.ItemArray);
                    }
                }
            }

            if (selected.Rows.Count > 0) MessageBox.Show("Tables Missing PK Exists.", "Exception", MessageBoxButtons.OK, MessageBoxIcon.Error);

            foreach (DataRow row in selected.Rows)
            {
                Helpers.AppendRecordToException(Path.Combine(Path.Combine(Application.StartupPath, _exceptionPath), "SYDFILES.XML"), row, "CFILE_NAM", "Missing Primary Key");
            }
        }

        // SYDFLFLD-Duplicated Table Fields Name.xml
        public void CreateDuplicateTableFieldsNameXml()
        {
            AriaDataAccessFiles dataAccess = new AriaDataAccessFiles(_connectionString, _mergePath);

            DataTable selected = GetDuplicatedRows(dataAccess.GetTableFieldDataTable(), "CFILE_NAM,CFLD_NAME", ",");
            selected.TableName = "SYDFLFLD";

            if (selected.Rows.Count > 0) MessageBox.Show("Doublicated Table Field Name Exists.", "Exception", MessageBoxButtons.OK, MessageBoxIcon.Error);

            foreach (DataRow row in selected.Rows)
            {
                Helpers.AppendRecordToException(Path.Combine(Path.Combine(Application.StartupPath, _exceptionPath), "SYDFLFLD.XML"), row, "CFILE_NAM,CFLD_NAME", "Doublicated Field Name");
            }
        }

        // SYDFLFLD-Duplicated Table Fields Title.xml
        public void CreateDuplicateTableFieldsTitleXml()
        {
            AriaDataAccessFiles dataAccess = new AriaDataAccessFiles(_connectionString, _mergePath);

            //DataTable fields = dataAccess.GetFieldDataTable();

            //DataTable selected = dataAccess.GetTableFieldDataTable();
            //selected.Columns.Add("CFLD_HEAD");

            //foreach (DataRow row in selected.Rows)
            //{
            //    if (fields.Select("CFLD_NAME = '" + row["CFLD_NAME"] + "'").Length == 1)
            //    {
            //        row["CFLD_HEAD"] = fields.Select("CFLD_NAME = '" + row["CFLD_NAME"] + "'")[0]["CFLD_HEAD"];
            //    }
            //}

            DataTable selected = GetDuplicatedRows(dataAccess.GetTableFieldDataTable(), "CFILE_NAM, CFLD_HEAD", ",RSC");
            selected.TableName = "SYDFLFLD";

            //DataTable selected2 = GetDuplicatedRows(selected, "CFILE_NAM, CFLD_HEAD", ",RSC");

            //foreach (DataRow row in selected.Rows)
            //{
            //    if (row["CFLD_HEAD"].ToString().Trim() == "")
            //    {
            //        selected2.Rows.Add(row.ItemArray);
            //    }
            //}

            //selected2.Columns.Remove("CFLD_HEAD");
            //selected2.TableName = "SYDFLFLD";

            if (selected.Rows.Count > 0) MessageBox.Show("Doublicated Table Field Title Exists.", "Exception", MessageBoxButtons.OK, MessageBoxIcon.Error);

            foreach (DataRow row in selected.Rows)
            {
                Helpers.AppendRecordToException(Path.Combine(Path.Combine(Application.StartupPath, _exceptionPath), "SYDFLFLD.XML"), row, "CFILE_NAM,CFLD_NAME", "Doublicated Field Title");
            }
        }

        // SYDREPRT-All Reports.xml
        public void CreateAllReportsXml()
        {
            AriaDataAccessFiles dataAccess = new AriaDataAccessFiles(_connectionString, _mergePath);

            DataTable selected = dataAccess.GetOptionGridDataTable();
            selected.TableName = "SYDREPRT";

            Helpers.AppendRecordToException(Path.Combine(Path.Combine(Application.StartupPath, _exceptionPath), "SYDREPRT.XML"), selected, "CREP_ID", "", true);
        }

        // SYREPUVR-All Reports Veriables.xml
        public void CreateAllReportsVariablesXml()
        {
            AriaDataAccessFiles dataAccess = new AriaDataAccessFiles(_connectionString, _mergePath);

            DataTable selected = dataAccess.GetOptionGridVariableDataTable();
            selected.TableName = "SYREPUVR";

            Helpers.AppendRecordToException(Path.Combine(Path.Combine(Application.StartupPath, _exceptionPath), "SYREPUVR.XML"), selected, "CREP_ID, CFLD_HEAD", "", true);
        }

        // Custom.xml
        public void CreateCustomTableXml()
        {
            AriaDataAccessFiles dataAccess = new AriaDataAccessFiles(_connectionString, _mergePath);

            DataTable selected = dataAccess.GetTableDataTable();
            selected.TableName = "SYDFILES";

            if (selected.Select("cupgrdlvl = 'U'").Length > 0) MessageBox.Show("Custom Table Exists.", "Exception", MessageBoxButtons.OK, MessageBoxIcon.Error);

            foreach (DataRow row in selected.Rows)
            {
                if (row["cupgrdlvl"].ToString().ToUpper().Trim() == "U")
                {
                    Helpers.AppendRecordToException(Path.Combine(Path.Combine(Application.StartupPath, _exceptionPath), "SYDFILES.XML"), row, "CFILE_NAM", "Custom Table");
                }
            }
        }

        public void CreateCustomFieldXml()
        {
            AriaDataAccessFiles dataAccess = new AriaDataAccessFiles(_connectionString, _mergePath);

            DataTable selected = dataAccess.GetFieldDataTable();
            selected.TableName = "SYDFIELD";

            if (selected.Select("cupgrdlvl = 'U'").Length > 0) MessageBox.Show("Custom Field Exists.", "Exception", MessageBoxButtons.OK, MessageBoxIcon.Error);

            foreach (DataRow row in selected.Rows)
            {
                if (row["cupgrdlvl"].ToString().ToUpper().Trim() == "U")
                {
                    Helpers.AppendRecordToException(Path.Combine(Path.Combine(Application.StartupPath, _exceptionPath), "SYDFIELD.XML"), row, "cfld_name", "Custom Field");
                }
            }
        }

        public void CreateCustomIndexXml()
        {
            AriaDataAccessFiles dataAccess = new AriaDataAccessFiles(_connectionString, _mergePath);

            DataTable selected = dataAccess.GetTableIndexDataTable();
            selected.TableName = "SYDINDEX";

            if (selected.Select("cupgrdlvl = 'U'").Length > 0) MessageBox.Show("Custom Index Exists.", "Exception", MessageBoxButtons.OK, MessageBoxIcon.Error);

            foreach (DataRow row in selected.Rows)
            {
                if (row["cupgrdlvl"].ToString().ToUpper().Trim() == "U")
                {
                    Helpers.AppendRecordToException(Path.Combine(Path.Combine(Application.StartupPath, _exceptionPath), "SYDINDEX.XML"), row, "CFILE_NAM, CFILE_TAG", "Custom Index");
                }
            }
        }

        public void CreateCustomTableFieldXml()
        {
            AriaDataAccessFiles dataAccess = new AriaDataAccessFiles(_connectionString, _mergePath);

            DataTable selected = dataAccess.GetTableFieldDataTable();
            selected.TableName = "SYDFLFLD";

            if (selected.Select("cupgrdlvl = 'U'").Length > 0) MessageBox.Show("Custom Table Field Exists.", "Exception", MessageBoxButtons.OK, MessageBoxIcon.Error);

            foreach (DataRow row in selected.Rows)
            {
                if (row["cupgrdlvl"].ToString().ToUpper().Trim() == "U")
                {
                    Helpers.AppendRecordToException(Path.Combine(Path.Combine(Application.StartupPath, _exceptionPath), "SYDFLFLD.XML"), row, "cfld_name, cfile_nam", "Custom Table Field");
                }
            }
        }
    }
}
