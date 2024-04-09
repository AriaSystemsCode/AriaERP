using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public abstract class SchemaInformation
    {
        public SchemaInformation(string connectionString,string mergePath)
        {
            _connectionString = connectionString;
            _mergePath = mergePath;
        }

        private string _connectionString;
        public string ConnectionString
        {
            get { return _connectionString; }
            set { _connectionString = value; }
        }
        private string _mergePath;

        public string MergePath
        {
            get { return _mergePath; }
            set { _mergePath = value; }
        }


        public abstract List<Table> GetTables();

        public abstract Table GetTable(string tableName);

        public abstract List<TableField> GetTableFields(string tableName);

        public abstract List<Index> GetTableIndexes(string tableName);

        public abstract List<OptionGrid> GetOptionGrids();

        public abstract List<OptionGridVariable> GetOptionGridVariables(string optionGirdID);
    }
}
