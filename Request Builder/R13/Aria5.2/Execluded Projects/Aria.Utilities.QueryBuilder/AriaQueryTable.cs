using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.QueryBuilder
{
    public class AriaQueryTable
    {
        public AriaQueryTable(string tableName, DatabaseType databaseType,string connectionString)
        {
            TableName = tableName;
            DatabaseType = databaseType;
            ConnectionString = connectionString;
        }

        public AriaQueryTable(string tableName, DatabaseType databaseType, string connectionString, string filter):this(tableName,databaseType,connectionString)
        {
            Filter = filter;
        }

        private string _tableName;
        public string TableName
        {
            get { return _tableName; }
            set { _tableName = value; }
        }

        private string _tableAlias;
        public string TableAlias
        {
            get { return _tableAlias; }
            set { _tableAlias = value; }
        }

        private DatabaseType _databaseType;
        public DatabaseType DatabaseType
        {
            get { return _databaseType; }
            set { _databaseType = value; }
        }

        private string _connectionString;
        public string ConnectionString
        {
            get { return _connectionString; }
            set { _connectionString = value; }
        }

        private string _filter = "";
        public string Filter
        {
            get { return _filter; }
            set { _filter = value; }
        }
    }
}
