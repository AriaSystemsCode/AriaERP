using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.ObjectDictionaryBuilder.Aria27ObjectDictionaryConverter
{
    public class TableColumn
    {
        private string _columnName;
        public string ColumnName
        {
            get { return _columnName; }
            set { _columnName = value; }
        }

        private string _description;
        public string Description
        {
            get { return _description; }
            set { _description = value; }
        }

        private EnumDataType _dataType;
        public EnumDataType DataType
        {
            get { return _dataType; }
            set { _dataType = value; }
        }

        private int _numericScale;
        public int NumericScale
        {
            get { return _numericScale; }
            set { _numericScale = value; }
        }

        private int _numericPrecision;
        public int NumericPrecision
        {
            get { return _numericPrecision; }
            set { _numericPrecision = value; }
        }

        private bool _isRquired;
        public bool IsRquired
        {
            get { return _isRquired; }
            set { _isRquired = value; }
        }

        private EnumUpgradeLevel _upgradeLevel;
        public EnumUpgradeLevel UpgradeLevel
        {
            get { return _upgradeLevel; }
            set { _upgradeLevel = value; }
        }
    }
}
