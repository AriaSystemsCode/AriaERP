using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Data;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public abstract class SchemaDataAccess
    {
        public SchemaDataAccess(string connectionString)
        {
            _connectionString = connectionString;
        }

        public SchemaDataAccess(string connectionString, string mergePath)
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


        public abstract DataTable GetTableDataTable();
        public abstract DataTable GetTableIndexDataTable();
        public abstract DataTable GetFieldDataTable();
        public abstract DataTable GetTableFieldDataTable();
        public abstract DataTable GetOptionGridDataTable();
        public abstract DataTable GetOptionGridVariableDataTable();
    }
}
