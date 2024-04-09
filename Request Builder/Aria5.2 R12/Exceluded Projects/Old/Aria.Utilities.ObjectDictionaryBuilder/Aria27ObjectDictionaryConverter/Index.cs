using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.ObjectDictionaryBuilder.Aria27ObjectDictionaryConverter
{
    public class Index
    {
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

        private EnumIndexType _type;
        public EnumIndexType Type
        {
            get { return _type; }
            set { _type = value; }
        }

        private EnumUpgradeLevel _upgradeLevel;
        public EnumUpgradeLevel UpgradeLevel
        {
            get { return _upgradeLevel; }
            set { _upgradeLevel = value; }
        }

    }
}
