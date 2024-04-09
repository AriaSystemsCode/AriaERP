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
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using System.Data.Odbc;
using System.Xml;
using System.IO;
using System.Windows.Forms;


namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public class Aria27SchemaInformation : SchemaInformation
    {

        private AriaDataAccessFiles _dataAccess;
        public AriaDataAccessFiles DataAccess
        {
            get { return _dataAccess; }
            set { _dataAccess = value; }
        }

        private Dictionary<string, List<TableField>> _fieldHasCode = new Dictionary<string, List<TableField>>();
        public Dictionary<string, List<TableField>> FieldHasCode
        {
            get { return _fieldHasCode; }
            set { _fieldHasCode = value; }
        }

        public Aria27SchemaInformation(string connectionString, string mergePath)
            : base(connectionString, mergePath)
        {
            _dataAccess = new AriaDataAccessFiles(connectionString, mergePath);
        }


        private List<Table> _cachedTables = null;
        public override List<Table> GetTables()
        {
            if (_cachedTables != null) return _cachedTables;

            _cachedTables = new List<Table>();


            DataTable table = new DataTable();

            table = _dataAccess.GetTableDataTable().Copy();


            List<Table> tables = new List<Table>();
            foreach (DataRow row in table.Rows)
            {
                
                string tableName = row["CFILE_NAM"].ToString().Trim();

                string description = row["CFILE_TTL"].ToString().Trim();
                string upgradeLevelStr = row["CUPGRDLVL"].ToString().Trim().ToUpper();
                UpgradeLevelTypes upgradeLevel = (upgradeLevelStr.Equals("S") || upgradeLevelStr.Equals("A") ? UpgradeLevelTypes.Standard : UpgradeLevelTypes.Custom);
                DatabaseTypes databaseType = (row["LSQLFILE"].ToString().Trim().ToUpper().Equals("F") ? DatabaseTypes.Aria27Data : DatabaseTypes.Aria4Data);

                string pkIndexName = "";

                pkIndexName = row["CFILE_TAG"].ToString().ToUpper().Trim();


                Index pk = null;
                for (int i = 0; i < GetTableIndexes(tableName).Count; i++)
                {
                    if (GetTableIndexes(tableName)[i].IndexName.Trim() == pkIndexName)
                    {
                        bool allKeysFound = true;
                        for (int j = 0; j < GetTableIndexes(tableName)[i].IndexFields.Count; j++)
                        {
                            object yy = GetTableFields(tableName).Where(r => r.FieldName.Trim() == GetTableIndexes(tableName)[i].IndexFields[j].FieldName.Trim());

                            if(GetTableFields(tableName).Where(r => r.FieldName.Trim() == GetTableIndexes(tableName)[i].IndexFields[j].FieldName.Trim()).Count() == 0)
                            {
                                allKeysFound = false;
                            }
                        }

                        if (allKeysFound)
                        {
                            pk = GetTableIndexes(tableName)[i];
                        }

                        break;
                    }
                }

                if (pk != null)
                {
                    tables.Add(new Table(tableName, description, upgradeLevel, databaseType, pk));
                    tables[tables.Count - 1].Row = row;
                }
            }

            _cachedTables = tables;

            return tables;
        }

        public override Table GetTable(string tableName)
        {
            return GetTables().First(r => r.TableName.Trim() == tableName.Trim());
        }

        private Dictionary<string, List<TableField>> _cachedTableFields = null;
        public override List<TableField> GetTableFields(string tableName)
        {
            if (_cachedTableFields != null) return _cachedTableFields[tableName.TrimEnd()];

            _cachedTableFields = new Dictionary<string, List<TableField>>();

            DataTable table = _dataAccess.GetTableFieldDataTable();

            DataTable fieldsTable = _dataAccess.GetFieldDataTable();

            List<TableField> tableColumns = new List<TableField>();

            foreach (DataRow row in table.Rows)
            {
                if (fieldsTable.Select("cfld_name = '" + row["cfld_name"].ToString() + "'").Length > 0)
                {
                    DataRow fieldRow = fieldsTable.Select("cfld_name = '" + row["cfld_name"].ToString() + "'")[0];

                    TableField column = new TableField();

                    column.TableName = row["cfile_nam"].ToString().Trim();
                    column.FieldName = row["CFLD_NAME"].ToString().Trim();
                    column.Description = fieldRow["MFLD_DES"].ToString().Trim();

                    string dataTypeStr = fieldRow["CDATA_TYP"].ToString().ToUpper().Trim();

                    if (dataTypeStr.Equals("N")) column.DataType = FieldDataTypes.Number;
                    else if (dataTypeStr.Equals("C")) column.DataType = FieldDataTypes.String;
                    else if (dataTypeStr.Equals("L")) column.DataType = FieldDataTypes.Logical;
                    else if (dataTypeStr.Equals("M")) column.DataType = FieldDataTypes.Memo;
                    else if (dataTypeStr.Equals("D")) column.DataType = FieldDataTypes.Date;
                    else if (dataTypeStr.Equals("G")) column.DataType = FieldDataTypes.General;
                    else column.DataType = FieldDataTypes.NotSet;
                    
                    column.NumericScale = Convert.ToInt32(fieldRow["NFLD_WDTH"].ToString().Trim());
                    column.NumericPrecision = Convert.ToInt32(fieldRow["NFLD_DEC"].ToString().Trim());
                    column.IsRquired = false;
                    string upgradeLevelStr = row["CUPGRDLVL"].ToString().ToUpper().Trim();
                    column.UpgradeLevel = (upgradeLevelStr.Equals("S") || upgradeLevelStr.Equals("A") ? UpgradeLevelTypes.Standard : UpgradeLevelTypes.Custom);

                    column.Code = (string)fieldRow["mcodeinfo"];

                    column.DecimalPlaces = (int)(Decimal)fieldRow["nfld_dec"];
                    column.FieldName = (string)fieldRow["cfld_name"];
                    column.HasReleatedField = (bool)fieldRow["lrltfields"];
                    column.Head = (string)row["cfld_head"];

                    column.IsReleatedField = (bool)fieldRow["lrelated"];
                    column.Mask = (string)fieldRow["cpict_str"];
                    column.Message = (string)fieldRow["cfld_msg"];
                    column.ReleatedFields = ((string)fieldRow["mrltfields"]).Split('|');

                    if (((string)fieldRow["mventries"]).IndexOf("~") > 0)
                    {
                        string[] description = ((string)fieldRow["mventries"]).Substring(0, ((string)fieldRow["mventries"]).IndexOf("~")).Split('|');
                        string[] values = ((string)fieldRow["mventries"]).Substring(((string)fieldRow["mventries"]).IndexOf("~") + 1).Split('|');

                        for (int validIndex = 0; validIndex < values.Length; validIndex++)
                        {
                            values[validIndex] += "|" + description[validIndex];
                        }

                        column.ValidEntries = values;
                    }

                    column.IsValidEntery =(bool) fieldRow["LVLDENTRY"];

                    column.ValidExpression = (string)fieldRow["mvald_str"];
                    column.Width = (int)(decimal)fieldRow["nfld_wdth"];

                    tableColumns.Add(column);                  
                }
            }

            foreach (DataRow row in _dataAccess.GetTableDataTable().Rows)
            {
                _cachedTableFields.Add(row["cfile_nam"].ToString().TrimEnd(), tableColumns.Where(r => r.TableName.TrimEnd() == row["CFILE_NAM"].ToString().TrimEnd()).ToList());

                if (tableColumns.Where(r => r.TableName.TrimEnd() == row["CFILE_NAM"].ToString().TrimEnd() && r.IsValidEntery == true).Count() > 0)
                    _fieldHasCode.Add(row["cfile_nam"].ToString().TrimEnd(), tableColumns.Where(r => r.TableName.TrimEnd() == row["CFILE_NAM"].ToString().TrimEnd() &&  r.IsValidEntery == true).ToList());
            }

            return _cachedTableFields[tableName.TrimEnd()];
        }

        private Dictionary<string, List<Index>> _cachedTableIndexes = null;
        public override List<Index> GetTableIndexes(string tableName)
        {
            if (_cachedTableIndexes != null) return _cachedTableIndexes[tableName.TrimEnd()];

            DataTable _GetIndexesTableCaching = null;


            DataTable table = new DataTable();
            table = _dataAccess.GetTableIndexDataTable().Copy();

            _GetIndexesTableCaching = table;

            List<Index> indexes = new List<Index>();
            table.TableName = "Indexes";

            foreach (DataRow row in table.Rows)
            {
                Index index = new Index();
                index.TableName = row["CFILE_NAM"].ToString().Trim();
                index.IndexName = row["CFILE_TAG"].ToString().Trim();
                index.Description = row["MINDX_DES"].ToString().Trim();
                string upgradeLevelStr = row["CUPGRDLVL"].ToString().ToUpper().Trim();
                index.UpgradeLevel = (upgradeLevelStr.Equals("S") || upgradeLevelStr.Equals("A") ? UpgradeLevelTypes.Standard : UpgradeLevelTypes.Custom);
                index.Expression = row["CINDX_EXP"].ToString().ToUpper().Trim();

                Dictionary<string, string> resultDic = new Dictionary<string, string>();


                string sortStr = row["LASCEND"].ToString().ToUpper().Trim();
                index.Sort = (sortStr.Equals("T") ? SortTypes.Ascending : SortTypes.Descending);

                index.IndexFields = VFPSchemaHelper.GetIndexFields(index.Expression, index.Sort).ToList();

                indexes.Add(index);
            }

            _cachedTableIndexes = new Dictionary<string, List<Index>>();

            foreach (DataRow row in _dataAccess.GetTableDataTable().Rows)
            {
                _cachedTableIndexes.Add(row["CFILE_NAM"].ToString().TrimEnd(), indexes.Where(r => r.TableName.TrimEnd() == row["CFILE_NAM"].ToString().TrimEnd()).ToList());
            }

            return _cachedTableIndexes[tableName.TrimEnd()];
        }

        private List<OptionGrid> _cachedOptionGrids = null;
        public override List<OptionGrid> GetOptionGrids()
        {
            if (_cachedOptionGrids != null) return _cachedOptionGrids;


            DataTable table = new DataTable();

            table = _dataAccess.GetOptionGridDataTable().Copy();
            table.Rows.Clear();

            Helpers.MergeTables(table, System.IO.Path.Combine(Path.Combine(Application.StartupPath, @"MergeData\Aria5\"), "SYDREPRT.xml"), new string[] { "CREP_ID" });

            List<OptionGrid> optionGrids = new List<OptionGrid>();
            foreach (DataRow row in table.Rows)
            {
                OptionGrid optionGrid = new OptionGrid();

                optionGrid.ReportID = row["crep_id"].ToString();
                optionGrid.ReportName = row["crep_name"].ToString();

                optionGrid.Type = row["capobjtyp"].ToString().Trim() == "R" ? OptionGridTypes.Report : OptionGridTypes.Program;
                optionGrid.UpgradeLevel = (row["CUPGRDLVL"].ToString().Trim().ToUpper().Equals("S") ||
                                               row["CUPGRDLVL"].ToString().Trim().ToUpper().Equals("A") ?
                                               UpgradeLevelTypes.Standard : UpgradeLevelTypes.Custom);


                optionGrids.Add(optionGrid);
            }

            _cachedOptionGrids = optionGrids;

            return _cachedOptionGrids;
        }

        private Dictionary<string, List<OptionGridVariable>> _cachedOptionGridVariables = null;
        public override List<OptionGridVariable> GetOptionGridVariables(string optionGirdID)
        {
            if (_cachedOptionGridVariables != null) return _cachedOptionGridVariables[optionGirdID.Trim()];

            DataTable table = new DataTable();
            table = _dataAccess.GetOptionGridVariableDataTable().Copy();
            table.Rows.Clear();

            Helpers.MergeTables(table, System.IO.Path.Combine(Path.Combine(Application.StartupPath, @"MergeData\Aria5\"), "SYREPUVR.xml"), new string[] { "CREP_ID", "CFLD_HEAD" });

            _cachedOptionGridVariables = new Dictionary<string, List<OptionGridVariable>>();

            foreach (DataRow optionGridRow in _dataAccess.GetOptionGridDataTable().Rows)
            {
                DataRow[] rows = table.Select("crep_ID = '" + optionGridRow["crep_id"].ToString() + "'", "nVarPos");
                List<OptionGridVariable> optionGridVariables = new List<OptionGridVariable>();

                foreach (DataRow row in rows)
                {
                    OptionGridVariable optionGridVariable = new OptionGridVariable();

                    optionGridVariable.BrowseField = (string)row["cbrwselfld"];
                    optionGridVariable.BrowseFields = (string)row["mbrwfields"];
                    optionGridVariable.BrowseFilter = (string)row["mbrwfltexp"];
                    if (((string)row["cdefa_typ"]) == "V") optionGridVariable.DefaultType = OptionGridVariableValueTypes.Fixed;
                    if (((string)row["cdefa_typ"]) == "E") optionGridVariable.DefaultType = OptionGridVariableValueTypes.Expression;
                    optionGridVariable.DefaultValue = (string)row["mdata_def"];
                    optionGridVariable.Editor = (string)row["cassociate"];
                    optionGridVariable.HideExpression = (string)row["msupexpr"];
                    optionGridVariable.BrowseOpenFunction = (string)row["csetfunc"];
                    optionGridVariable.BrowseSelectFunction = (string)row["csetfunc"];
                    optionGridVariable.BrowseUnselectFunction = (string)row["csetfunc"];

                    optionGridVariable.Code = (string)row["cCodes_fld"];

                    if ((string)row["cdata_typ"] == "C") optionGridVariable.DataType = FieldDataTypes.String;
                    if ((string)row["cdata_typ"] == "N") optionGridVariable.DataType = FieldDataTypes.Number;
                    if ((string)row["cdata_typ"] == "D") optionGridVariable.DataType = FieldDataTypes.Date;
                    if ((string)row["cdata_typ"] == "L") optionGridVariable.DataType = FieldDataTypes.Logical;

                    optionGridVariable.DecimalPlaces = (int)(Decimal)row["nfld_dec"];
                    optionGridVariable.Description = (string)row["mfld_des"];
                    optionGridVariable.FieldName = (string)row["mfld_name"];
                    optionGridVariable.Head = (string)row["cfld_head"];
                    optionGridVariable.Mask = (string)row["cpict_str"];
                    optionGridVariable.Message = (string)row["cfld_msg"];
                    if (((string)row["mventries"]).IndexOf("~") > 0) optionGridVariable.ValidEntries = ((string)row["mventries"]).Substring(0, ((string)row["mventries"]).IndexOf("~")).Split('|');
                    optionGridVariable.ValidEntry = (bool)row["lvldentry"];
                    optionGridVariable.ValidExpression = (string)row["mvald_str"];
                    optionGridVariable.Width = (int)(decimal)row["nfld_wdth"];

                    optionGridVariable.UpgradeLevel = (row["CUPGRDLVL"].ToString().Trim().ToUpper().Equals("S") ||
                                                   row["CUPGRDLVL"].ToString().Trim().ToUpper().Equals("A") ?
                                                   UpgradeLevelTypes.Standard : UpgradeLevelTypes.Custom);

                    optionGridVariables.Add(optionGridVariable);
                }

                _cachedOptionGridVariables.Add(optionGridRow["crep_id"].ToString().TrimEnd(), optionGridVariables);
            }

            return _cachedOptionGridVariables[optionGirdID.TrimEnd()];
        }
    }
}
