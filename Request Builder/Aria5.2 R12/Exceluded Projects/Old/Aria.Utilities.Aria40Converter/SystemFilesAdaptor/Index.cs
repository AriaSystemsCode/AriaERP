using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public class Index
    {
        private string _tableName;
        public string TableName
        {
            get { return _tableName; }
            set { _tableName = value; }
        }

        private string _indexName;
        public string IndexName
        {
            get { return _indexName; }
            set { _indexName = value; }
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

        private string _Expression;
        public string Expression
        {
            get { return _Expression; }
            set { _Expression = value; }
        }

        private SortTypes _sort;
        public SortTypes Sort
        {
            get { return _sort; }
            set { _sort = value; }
        }

        private List<IndexField> _indexFields;
        public List<IndexField> IndexFields
        {
            get { return _indexFields; }
            set { _indexFields = value; }
        }
    }
}
