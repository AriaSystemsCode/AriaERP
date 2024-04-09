using System;
using System.Collections.Generic;
using System.Text;
using System.Data;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public class Table : IEqualityComparer<Table>
    {
        public Table()
        {
        }

        public Table(string tableName, string description, UpgradeLevelTypes upgradeLevel, DatabaseTypes databaseType, Index primaryIndex)
        {
            TableName = tableName;
            Description = description;
            UpgradeLevel = upgradeLevel;
            PrimaryKey = primaryIndex;
        }

        private string _tableName;
        public string TableName
        {
            get { return _tableName; }
            set { _tableName = value; }
        }

        private string _description;
        public string Description
        {
            get { return _description; }
            set { _description = value; }
        }

        private UpgradeLevelTypes _upgradeLevel;
        public UpgradeLevelTypes UpgradeLevel
        {
            get { return _upgradeLevel; }
            set { _upgradeLevel = value; }
        }

        private DatabaseTypes _databaseType;
        public DatabaseTypes DatabaseType
        {
            get { return _databaseType; }
            set { _databaseType = value; }
        }

        private Index _primaryKey;
        public Index PrimaryKey
        {
            get { return _primaryKey; }
            set { _primaryKey = value; }
        }

        private DataRow _row;
        public DataRow Row
        {
            get { return _row; }
            set { _row = value; }
        }

        #region IEqualityComparer<Table> Members

        public bool Equals(Table x, Table y)
        {
            return x.TableName == y.TableName;
        }

        public int GetHashCode(Table obj)
        {
            return obj.GetHashCode();
        }

        public static Table Copy(Table table)
        {
            return (Table)table.MemberwiseClone();
        }
        #endregion
    }
}
