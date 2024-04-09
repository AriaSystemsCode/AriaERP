using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.ObjectDictionaryBuilder.Aria27ObjectDictionaryConverter
{
    public abstract class SchemaInformation
    {
        public SchemaInformation(string connectionString)
        {
            _connectionString = connectionString;
        }

        private string _connectionString;
        public string ConnectionString
        {
            get { return _connectionString; }
            set { _connectionString = value; }
        }

        public abstract Table[] GetTables();

        public abstract Table GetTable(string tableName);

        public abstract TableColumn[] GetTableColumns(string tableName);

        public abstract Index[] GetIndexes(string tableName);

        public abstract IndexColumn[] GetIndexColumns(string tableName, string indexName);
    }
}
