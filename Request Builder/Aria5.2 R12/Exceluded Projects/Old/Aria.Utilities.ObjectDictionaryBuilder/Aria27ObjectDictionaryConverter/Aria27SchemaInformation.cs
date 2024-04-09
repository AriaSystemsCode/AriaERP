using System;
using System.Collections.Generic;
using System.Text;
using System.Data.Odbc;
using System.Data;
using System.Collections;

namespace Aria.Utilities.ObjectDictionaryBuilder.Aria27ObjectDictionaryConverter
{
    public class Aria27SchemaInformation : SchemaInformation
    {
        public Aria27SchemaInformation(string connectionString)
            : base(connectionString)
        {
        }

        public override Table[] GetTables()
        {
            OdbcCommand command = new OdbcCommand();
            command.CommandText = "SELECT CFILE_NAM, CFILE_TTL, CUPGRDLVL, LSQLFILE FROM SYDFILES WHERE CUPGRDLVL != 'U'";
            command.Connection = new OdbcConnection(this.ConnectionString);
            command.Connection.Open();

            OdbcDataAdapter adapter = new OdbcDataAdapter(command);
            DataTable table = new DataTable();

            adapter.Fill(table);
            command.Connection.Close();

            ArrayList tables = new ArrayList();

            foreach (DataRow row in table.Rows)
            {
                string tableName = row["CFILE_NAM"].ToString().Trim();
                string description = row["CFILE_TTL"].ToString().Trim();
                string upgradeLevelStr = row["CUPGRDLVL"].ToString().Trim().ToUpper();
                EnumUpgradeLevel upgradeLevel = (upgradeLevelStr.Equals("S") || upgradeLevelStr.Equals("A") ? EnumUpgradeLevel.Standard : EnumUpgradeLevel.Custom);
                DatabaseType databaseType = (row["LSQLFILE"].ToString().Trim().ToUpper().Equals("F") ? DatabaseType.Aria27Data : DatabaseType.Aria4Data);

                tables.Add(new Table(tableName, description, upgradeLevel,databaseType));
            }

            return (Table[])tables.ToArray(typeof(Table));
        }

        public override Table GetTable(string tableName)
        {
            OdbcCommand command = new OdbcCommand();
            command.CommandText = "SELECT CFILE_NAM, CFILE_TTL, CUPGRDLVL FROM SYDFILES";
            command.CommandText += " WHERE CFILE_NAM = '" + tableName + "'";
            command.Connection = new OdbcConnection(this.ConnectionString);
            command.Connection.Open();

            OdbcDataAdapter adapter = new OdbcDataAdapter(command);
            DataTable table = new DataTable();

            adapter.Fill(table);
            command.Connection.Close();

            Table resultTable = new Table();

            foreach (DataRow row in table.Rows)
            {
                resultTable.TableName = row["CFILE_NAM"].ToString().Trim();
                resultTable.Description = row["CFILE_TTL"].ToString().Trim();

                string upgradeLevelStr = row["CUPGRDLVL"].ToString().Trim().ToUpper();
                EnumUpgradeLevel upgradeLevel = (upgradeLevelStr.Equals("S") ? EnumUpgradeLevel.Standard : EnumUpgradeLevel.Custom);

                resultTable.UpgradeLevel = upgradeLevel;
            }

            return resultTable;
        }

        public override TableColumn[] GetTableColumns(string tableName)
        {
            OdbcCommand command = new OdbcCommand();
            command.CommandText = "SELECT CFILE_NAM, CFLD_NAME, MFLD_DES, CDATA_TYP, NFLD_WDTH, NFLD_DEC, CUPGRDLVL FROM SYDFIELD LEFT JOIN SYDFLFLD ON SYDFIELD.CFLD_NAME = SYDFLFLD.CFLD_NAME";
            command.CommandText += " WHERE CFILE_NAM = '" + tableName + "'";
            command.Connection = new OdbcConnection(this.ConnectionString);
            command.Connection.Open();

            OdbcDataAdapter adapter = new OdbcDataAdapter(command);
            DataTable table = new DataTable();

            adapter.Fill(table);
            command.Connection.Close();

            ArrayList tableColumns = new ArrayList();

            foreach (DataRow row in table.Rows)
            {
                TableColumn column = new TableColumn();

                column.ColumnName = row["CFLD_NAME"].ToString().Trim();
                column.Description = row["MFLD_DES"].ToString().Trim();
                string dataTypeStr = row["CDATA_TYP"].ToString().ToUpper().Trim();

                if (dataTypeStr.Equals("N"))
                    column.DataType = EnumDataType.Number;

                else if (dataTypeStr.Equals("C"))
                    column.DataType = EnumDataType.String;

                else if (dataTypeStr.Equals("L"))
                    column.DataType = EnumDataType.Logical;

                else if (dataTypeStr.Equals("M"))
                    column.DataType = EnumDataType.Memo;

                else if (dataTypeStr.Equals("D"))
                    column.DataType = EnumDataType.Date;

                else if (dataTypeStr.Equals("G"))
                    column.DataType = EnumDataType.General;

                else
                    column.DataType = EnumDataType.NotSet;

                column.NumericScale = Convert.ToInt32(row["NFLD_WDTH"].ToString().Trim());
                column.NumericPrecision = Convert.ToInt32(row["NFLD_DEC"].ToString().Trim());
                column.IsRquired = false;
                string upgradeLevelStr = row["CUPGRDLVL"].ToString().ToUpper().Trim();
                column.UpgradeLevel = (upgradeLevelStr.Equals("S") || upgradeLevelStr.Equals("A") ? EnumUpgradeLevel.Standard : EnumUpgradeLevel.Custom);

                tableColumns.Add(column);
            }

            return (TableColumn[])tableColumns.ToArray(typeof(TableColumn));
        }

        public override Index[] GetIndexes(string tableName)
        {
            OdbcCommand command = new OdbcCommand();
            command.CommandText = "SELECT CFILE_TAG, MINDX_DES, CFILE_TAG, CUPGRDLVL FROM SYDINDEX";
            command.CommandText += " WHERE CFILE_NAM = '" + tableName + "'";
            command.Connection = new OdbcConnection(this.ConnectionString);
            command.Connection.Open();

            OdbcDataAdapter adapter = new OdbcDataAdapter(command);
            DataTable table = new DataTable();

            adapter.Fill(table);
            command.Connection.Close();

            ArrayList indexes = new ArrayList();

            foreach (DataRow row in table.Rows)
            {
                Index index = new Index();

                index.IndexName = row["CFILE_TAG"].ToString().Trim();
                index.Description = row["MINDX_DES"].ToString().Trim();

                string indexTypeStr = row["CFILE_TAG"].ToString().ToUpper().Trim();
                command.CommandText = "SELECT CFILE_TAG FROM SYDFILES WHERE CFILE_NAM = '" + tableName + "' AND CFILE_TAG = '" + indexTypeStr + "'";
                command.Connection.Open();
                DataTable tempTable = new DataTable();
                adapter.Fill(tempTable);
                command.Connection.Close();
                index.Type = (tempTable.Rows.Count > 0 ? EnumIndexType.PrimaryKey : EnumIndexType.Index);

                string upgradeLevelStr = row["CUPGRDLVL"].ToString().ToUpper().Trim();
                index.UpgradeLevel = (upgradeLevelStr.Equals("S") || upgradeLevelStr.Equals("A") ? EnumUpgradeLevel.Standard : EnumUpgradeLevel.Custom);

                indexes.Add(index);
            }

            return (Index[])indexes.ToArray(typeof(Index));
        }

        public string getIndexExpression(string tableName, string indexName)
        {
            OdbcCommand command = new OdbcCommand();
            command.CommandText = "SELECT CINDX_EXP, LASCEND FROM SYDINDEX";
            command.CommandText += " WHERE CFILE_NAM = '" + tableName + "' AND CFILE_TAG = '" + indexName + "'";
            command.Connection = new OdbcConnection(this.ConnectionString);
            command.Connection.Open();

            OdbcDataAdapter adapter = new OdbcDataAdapter(command);
            DataTable table = new DataTable();

            adapter.Fill(table);
            command.Connection.Close();
            string expression = "";

            foreach (DataRow row in table.Rows)
            {
                expression = row["CINDX_EXP"].ToString().Trim();
            }

            return expression;
        }

        public override IndexColumn[] GetIndexColumns(string tableName, string indexName)
        {
            OdbcCommand command = new OdbcCommand();
            command.CommandText = "SELECT CINDX_EXP, LASCEND FROM SYDINDEX";
            command.CommandText += " WHERE CFILE_NAM = '" + tableName + "' AND CFILE_TAG = '" + indexName + "'";
            command.Connection = new OdbcConnection(this.ConnectionString);
            command.Connection.Open();  

            OdbcDataAdapter adapter = new OdbcDataAdapter(command);
            DataTable table = new DataTable();

            adapter.Fill(table);
            command.Connection.Close();

            ArrayList indexes = new ArrayList();

            foreach (DataRow row in table.Rows)
            {
                string expression = row["CINDX_EXP"].ToString().Trim();
                string sortStr = row["LASCEND"].ToString().ToUpper().Trim();
                EnumSort sort = (sortStr.Equals("T") ? EnumSort.Ascending : EnumSort.Descending);

                VFPSchemaAdapter schemaAdapter = new VFPSchemaAdapter();
                indexes.AddRange(schemaAdapter.GetIndexColumns(expression, sort));
            }

            return (IndexColumn[])indexes.ToArray(typeof(IndexColumn));
        }
    }
}
