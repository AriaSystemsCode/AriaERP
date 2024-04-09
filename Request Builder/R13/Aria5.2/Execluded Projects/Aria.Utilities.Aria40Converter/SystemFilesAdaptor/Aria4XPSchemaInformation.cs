using System;
using System.Collections.Generic;
using System.Text;
using System.Data.Odbc;
using System.Data;
using System.Collections;
using System.Linq;
using System.Linq.Expressions;
using Aria.Utilities.Aria40Converter.Helpers;
using System.IO;
using System.Windows.Forms;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    class Aria4XPSchemaInformation : SchemaInformation
    {
        private Aria27SchemaInformation a27SchemaInfo;
        private Aria27SchemaInformation a4xpSchemaInfo;
        private string _mergePath50;

        private string _mergePath27;
        public string MergePath27
        {
            get { return _mergePath27; }
            set { _mergePath27 = value; }
        }

        private string _mergePath4Xp;
        public string MergePath4Xp
        {
            get { return _mergePath4Xp; }
            set { _mergePath4Xp = value; }
        }

        public Aria4XPSchemaInformation(string connectionString4xp, string connectionString27, string mergePath4Xp, string mergePath27)
            : base(connectionString4xp, mergePath4Xp)
        {
            _mergePath27 = mergePath27;
            _mergePath4Xp = mergePath4Xp;

            a27SchemaInfo = new Aria27SchemaInformation(connectionString27, mergePath27);
            a4xpSchemaInfo = new Aria27SchemaInformation(connectionString4xp, mergePath4Xp);
            _mergePath50 = Path.Combine(Application.StartupPath, @"MergeData\Aria5\");
        }

        private List<Table> _cachedGetTables = null;
        public override List<Table> GetTables()
        {
            if (_cachedGetTables != null) return _cachedGetTables;

            _cachedGetTables = new List<Table>();
            List<Table> aria4XpTables = a4xpSchemaInfo.GetTables();
            List<Table> aria27Tables = a27SchemaInfo.GetTables();

            foreach (Table table in aria4XpTables)
                table.DatabaseType = DatabaseTypes.Aria4Data;

            foreach (Table table in aria27Tables)
            {
                if (table.TableName.Trim().ToUpper().StartsWith("SY"))
                {
                    table.DatabaseType = DatabaseTypes.Aria27SystemFiles;
                }
                else
                {
                    table.DatabaseType = DatabaseTypes.Aria27Data;
                }
            }

            List<Table> aria27Exclusive = aria27Tables.Where(p => !(aria4XpTables.Select(x => x.TableName).Contains(p.TableName))).ToList();
            _cachedGetTables = aria27Exclusive.Union(aria4XpTables).ToList();

            return _cachedGetTables;
        }

        public override Table GetTable(string tableName)
        {


            return GetTables().First(r => r.TableName.Trim() == tableName.Trim());
        }

        public override List<TableField> GetTableFields(string tableName)
        {


            if (a4xpSchemaInfo.GetTables().Exists(r => r.TableName == tableName))
            {
                return a4xpSchemaInfo.GetTableFields(tableName);
            }
            else
            {
                return a27SchemaInfo.GetTableFields(tableName);
            }
        }

        public override List<Index> GetTableIndexes(string tableName)
        {


            if (a4xpSchemaInfo.GetTables().Exists(r => r.TableName == tableName))
            {
                return a4xpSchemaInfo.GetTableIndexes(tableName);
            }
            else
            {
                return a27SchemaInfo.GetTableIndexes(tableName);
            }
        }

        public override List<OptionGrid> GetOptionGrids()
        {
            return a4xpSchemaInfo.GetOptionGrids();
        }

        public override List<OptionGridVariable> GetOptionGridVariables(string optionGirdID)
        {
            return a4xpSchemaInfo.GetOptionGridVariables(optionGirdID);
        }

        public Dictionary<string, string> GetTableRelationFields(string primaryKeyTableName, string foreignKeyTableName)
        {
            List<TableField> table1Columns = GetTableFields(primaryKeyTableName).OrderBy(p => p.FieldName.Trim()).ToList();

            Dictionary<string, string> result = new Dictionary<string, string>();

            if (foreignKeyTableName.Trim() == "SALESREP" && primaryKeyTableName.Trim() == "ORDHDR")
            {
                int ii = 0;
            }

            Table table2 = GetTable(foreignKeyTableName);

            List<IndexField> table2IndexColumns = table2.PrimaryKey.IndexFields;

            foreach (IndexField table2IndexColumn in table2IndexColumns)
            {
                string flagDuplicatedFiled = "";
                foreach (TableField table1Column in table1Columns)
                {
                    if (table1Column.FieldName.Trim() == table2IndexColumn.FieldName.Trim() && table1Column.FieldName.Trim() != flagDuplicatedFiled)
                    {
                        result.Add(table1Column.FieldName.Trim(), table2IndexColumn.FieldName.Trim());
                    }

                    flagDuplicatedFiled = table1Column.FieldName.Trim();
                }
            }

            return result;
        }

        public List<Table> GetChildrenTables(string tableName)
        {
            string filter = GetTable(tableName).PrimaryKey.Expression;

            List<Table> childrenTables = GetTables().Where(p => p.PrimaryKey.Expression.StartsWith(filter) && p.PrimaryKey.IndexFields.Count - 1 == GetTable(tableName).PrimaryKey.IndexFields.Count).ToList();
            return childrenTables.ToList();
        }

        public List<Table> GetRelatedTables(string tableName)
        {
            List<TableField> tableColumnsParent = GetTableFields(tableName);
            List<Table> relatedTable = new List<Table>();

            foreach (Table table in GetTables().Where(r => r.TableName.TrimEnd() != tableName.TrimEnd()))
            {
                List<IndexField> tableColumnsChild = table.PrimaryKey.IndexFields;

                if (GetTableRelationFields(tableName, table.TableName).Count == tableColumnsChild.Count()) relatedTable.Add(table);
            }
            return relatedTable;
        }

        public int GetParentCount(string tableName)
        {
            int result = 0;

            string filter = GetTable(tableName).PrimaryKey.Expression;
            foreach (Table Parent in GetTables())
            {
                if (tableName != Parent.TableName && filter.Length > Parent.PrimaryKey.Expression.Length && filter.Trim().StartsWith(Parent.PrimaryKey.Expression.Trim()))
                {
                    result++;
                }
            }

            return result;
        }

        private bool CheckIfChildTable(string tableName)
        {
            string filter = GetTable(tableName).PrimaryKey.Expression;
            foreach (Table Parent in GetTables())
                if (tableName != Parent.TableName && filter.Length > Parent.PrimaryKey.Expression.Length && filter.Trim().StartsWith(Parent.PrimaryKey.Expression.Trim()))
                    return true;

            return false;
        }

        public bool IsChildTable(string tableName)
        {
            GetTableRelations(tableName);
            object x = _cachedTableRelations.Where(r => r.RelationName == "ParentChild" && r.TableName == tableName);

            return _cachedTableRelations.Where(r => r.RelationName == "ParentChild" && r.TableName == tableName).Count() > 0;
        }

        public DataTable GetTableRelationDataTable()
        {
            List<Table> actual;
            actual = GetTables();

            DataTable selected = new DataTable();
            selected.Columns.Add("CFILE");
            selected.Columns.Add("CRELFILE");
            selected.Columns.Add("CREL_NAME");
            selected.Columns.Add("CREL_TTL");
            selected.Columns.Add("Filter");
            selected.TableName = "RELFILES";
            foreach (Table table in actual)
            {
                foreach (Table ChildTable in GetRelatedTables(table.TableName).OrderBy(p => p.TableName))
                {
                    selected.Rows.Add(table.TableName, ChildTable.TableName, "", ChildTable.Description);
                }
                foreach (Table ChildTable in GetChildrenTables(table.TableName))
                {
                    selected.Rows.Add(table.TableName, ChildTable.TableName, "ParentChild", "Parent to Childern Relation.");
                }
            }
            return selected;
        }

        public DataTable GetTableRelationFieldDataTable()
        {
            List<Table> actual;

            actual = GetTables();

            DataTable selected = new DataTable();
            selected.Columns.Add("CFILE");
            selected.Columns.Add("CRELFILE");
            selected.Columns.Add("CREL_NAME");
            selected.Columns.Add("CFLD");
            selected.Columns.Add("CRELFLD");

            selected.TableName = "RLFILFLD";
            foreach (Table table in actual)
            {
                foreach (Table ChildTable in GetRelatedTables(table.TableName))
                {
                    foreach (KeyValuePair<string, string> item in
                            GetTableRelationFields(table.TableName, ChildTable.TableName))
                    {
                        selected.Rows.Add(table.TableName,
                                          ChildTable.TableName,
                                          "",
                                          item.Key,
                                          item.Value);
                    }
                }

                foreach (Table ChildTable in GetChildrenTables(table.TableName))
                {
                    foreach (KeyValuePair<string, string> item in
                            GetTableRelationFields(table.TableName, ChildTable.TableName))
                    {
                        selected.Rows.Add(table.TableName,
                                          ChildTable.TableName,
                                          "ParentChild",
                                          item.Key,
                                          item.Value);
                    }
                }
            }

            return selected;
        }

        private List<TableRelation> _cachedTableRelations;
        public List<TableRelation> GetTableRelations(string tableName)
        {
            if (_cachedTableRelations != null) return _cachedTableRelations.Where(r => r.RelatedTableName.Trim() == tableName.Trim()).Distinct().ToList();

            // fill tables
            DataTable tableRelations = GetTableRelationDataTable().Copy();
            DataTable tableRelationFields = GetTableRelationFieldDataTable().Copy();

            // merger
            string[] primaryKeys;
            primaryKeys = new string[3];
            primaryKeys[0] = "CFILE";
            primaryKeys[1] = "CRELFILE";
            primaryKeys[2] = "CREL_NAME";

            Helpers.MergeTables(tableRelations, System.IO.Path.Combine(_mergePath50, "RELFILES.xml"), primaryKeys);

            primaryKeys = new string[5];
            primaryKeys[0] = "CFILE";
            primaryKeys[1] = "CRELFILE";
            primaryKeys[2] = "CREL_NAME";
            primaryKeys[3] = "CFLD";
            primaryKeys[4] = "CRELFLD";


            Helpers.MergeTables(tableRelationFields, System.IO.Path.Combine(_mergePath50, "RLFILFLD.xml"), primaryKeys);

            _cachedTableRelations = new List<TableRelation>();
            foreach (DataRow tableRelationRow in tableRelations.Rows)
            {
                _cachedTableRelations.Add(new TableRelation());

                _cachedTableRelations.Last().RelatedTableName = tableRelationRow["CFILE"].ToString();
                _cachedTableRelations.Last().TableName = tableRelationRow["CRELFILE"].ToString();
                _cachedTableRelations.Last().RelationName = tableRelationRow["CREL_NAME"].ToString();
                _cachedTableRelations.Last().RelationTitle = tableRelationRow["CREL_TTL"].ToString();
                if (tableRelationRow["Filter"] != DBNull.Value)
                {
                    _cachedTableRelations.Last().Filter = tableRelationRow["Filter"].ToString();
                }
                _cachedTableRelations.Last().RelationType = tableRelationRow["CREL_NAME"].ToString().Trim() == "ParentChild" ? RelationTypes.ParentChild : RelationTypes.Related;

                string filter = "CFILE = '" + tableRelationRow["CFILE"].ToString() + "' and " +
                                "CRELFILE = '" + tableRelationRow["CRELFILE"].ToString() + "' and " +
                                "CREL_NAME = '" + tableRelationRow["CREL_NAME"].ToString() + "'";

                foreach (DataRow tableRelationFieldRow in tableRelationFields.Select(filter))
                {
                    _cachedTableRelations.Last().RelatedTablePrimaryKeyFields.Add(tableRelationFieldRow["CFLD"].ToString().Trim());
                    _cachedTableRelations.Last().TablePrimaryKeyFields.Add(tableRelationFieldRow["CRELFLD"].ToString().Trim());
                }
            }

            foreach (Table table in GetTables())
            {
                if (!CheckIfChildTable(table.TableName.Trim()))
                {
                    if (GetTableFields(table.TableName).Where(r => r.FieldName.Trim().ToUpper() == "Cadd_user".ToUpper()).Count() > 0)
                    {
                        _cachedTableRelations.Add(new TableRelation());

                        _cachedTableRelations.Last().RelatedTableName = table.TableName.Trim();
                        _cachedTableRelations.Last().TableName = "SYUUSER";
                        _cachedTableRelations.Last().RelationName = "";
                        _cachedTableRelations.Last().RelationType = RelationTypes.Related;
                        _cachedTableRelations.Last().RelationTitle = "(Add_User)";
                        _cachedTableRelations.Last().RelatedTablePrimaryKeyFields.Add("Cadd_user");
                        _cachedTableRelations.Last().TablePrimaryKeyFields.Add("Cuser_id");
                    }

                    if (GetTableFields(table.TableName).Where(r => r.FieldName.Trim().ToUpper() == "CEdit_User".ToUpper()).Count() > 0)
                    {
                        _cachedTableRelations.Add(new TableRelation());

                        _cachedTableRelations.Last().RelatedTableName = table.TableName.Trim();
                        _cachedTableRelations.Last().TableName = "SYUUSER";
                        _cachedTableRelations.Last().RelationName = "";
                        _cachedTableRelations.Last().RelationType = RelationTypes.Related;
                        _cachedTableRelations.Last().RelationTitle = "(Edit_User)";
                        _cachedTableRelations.Last().RelatedTablePrimaryKeyFields.Add("Cedit_user");
                        _cachedTableRelations.Last().TablePrimaryKeyFields.Add("Cuser_id");
                    }
                }
            }

            foreach (var fieldCode in a27SchemaInfo.FieldHasCode)
            {
                if (a4xpSchemaInfo.GetTables().Where(r => r.TableName == fieldCode.Key).Count() == 0)
                {
                    foreach (TableField code in fieldCode.Value)
                    {
                        _cachedTableRelations.Add(new TableRelation());

                        _cachedTableRelations.Last().RelatedTableName = fieldCode.Key;

                        _cachedTableRelations.Last().TableName = "CODES";
                        _cachedTableRelations.Last().RelationName = "";
                        _cachedTableRelations.Last().RelationType = RelationTypes.Related;
                        _cachedTableRelations.Last().RelationTitle = code.Head.TrimEnd() + " (_Code)";
                        _cachedTableRelations.Last().RelatedTablePrimaryKeyFields.Add(code.FieldName);
                        _cachedTableRelations.Last().TablePrimaryKeyFields.Add(code.FieldName);
                    }
                }
            }

            foreach (var fieldCode in a4xpSchemaInfo.FieldHasCode)
            {
                foreach (TableField code in fieldCode.Value)
                {
                    _cachedTableRelations.Add(new TableRelation());

                    _cachedTableRelations.Last().RelatedTableName = fieldCode.Key;

                    _cachedTableRelations.Last().TableName = "CODES";
                    _cachedTableRelations.Last().RelationName = "";
                    _cachedTableRelations.Last().RelationType = RelationTypes.Related;
                    _cachedTableRelations.Last().RelationTitle = code.Head.TrimEnd() + " (_Code)";
                    _cachedTableRelations.Last().RelatedTablePrimaryKeyFields.Add(code.FieldName);
                    _cachedTableRelations.Last().TablePrimaryKeyFields.Add(code.FieldName);
                }
            }

            return _cachedTableRelations.Where(r => r.RelatedTableName.Trim() == tableName.Trim()).Distinct().ToList();
        }

        public DataRow GetSpecialFieldsInformation(string tableName, String fieldName)
        {
            DataTable table = new DataTable();
            table.ReadXml(System.IO.Path.Combine(_mergePath50, "SYDFLFLD-EXT.xml"));
            if (table.Select("TableName = '" + tableName + "' AND FiledName = '" + fieldName + "'").Length == 0)
            {
                return null;
            }
            else
            {
                return table.Select("TableName = '" + tableName + "' AND FiledName = '" + fieldName + "'")[0];
            }
        }
    }
}