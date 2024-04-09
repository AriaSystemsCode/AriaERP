using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.ObjectDictionaryBuilder.Aria27ObjectDictionaryConverter
{
    public class Table
    {
        public Table()
        {
        }

        public Table(string tableName, string description, EnumUpgradeLevel upgradeLevel,DatabaseType databaseType)
        {
            this.TableName = tableName;
            this.Description = description;
            this.UpgradeLevel = upgradeLevel;
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

        private EnumUpgradeLevel _upgradeLevel;
        public EnumUpgradeLevel UpgradeLevel
        {
            get { return _upgradeLevel; }
            set { _upgradeLevel = value; }
        }

        private DatabaseType _databaseType;
        public DatabaseType DatabaseType
        {
            get { return _databaseType; }
            set { _databaseType = value; }
        }
    }
}
