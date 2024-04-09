using System;
using System.Collections.Generic;
using System.Text;
using Aria.Environment;
using Aria.Xml;

namespace Aria.DataTypes.ObjectDictionary.Settings.Object
{
    [Serializable, AriaSerializableAttribute]
    public class AriaDataObjectSettings : AriaObjectRevisionSettings
    {
        private string _tableName;
        public string TableName
        {
            get { return _tableName; }
            set { _tableName = value; }
        }

        private string _filter;
        public string Filter
        {
            get { return _filter; }
            set { _filter = value; }
        }

        private AriaDatabaseTypes _databaseType;
        public AriaDatabaseTypes DatabaseType
        {
            get { return _databaseType; }
            set { _databaseType = value; }
        }

        private string _fixedFilter;
        public string FixedFilter
        {
            get { return _fixedFilter; }
            set { _fixedFilter = value; }
        }
    }
}
