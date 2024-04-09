using System;
using System.Collections.Generic;
using System.Text;
using Aria.DataTypes.Settings;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public class TableField
    {
        private string _tableName;
        public string TableName
        {
            get { return _tableName; }
            set { _tableName = value; }
        }

        private string _fieldName;
        public string FieldName
        {
            get { return _fieldName; }
            set { _fieldName = value; }
        }

        private string _description;
        public string Description
        {
            get { return _description; }
            set { _description = value; }
        }

        private FieldDataTypes _dataType;
        public FieldDataTypes DataType
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

        private UpgradeLevelTypes _upgradeLevel;
        public UpgradeLevelTypes UpgradeLevel
        {
            get { return _upgradeLevel; }
            set { _upgradeLevel = value; }
        }

        private string _head;
        public string Head
        {
            get { return _head; }
            set { _head = value; }
        }

        private string _message;
        public string Message
        {
            get { return _message; }
            set { _message = value; }
        }

        private int _width;
        public int Width
        {
            get { return _width; }
            set { _width = value; }
        }

        private int _decimalPlaces;
        public int DecimalPlaces
        {
            get { return _decimalPlaces; }
            set { _decimalPlaces = value; }
        }

        private string _validExpression;
        public string ValidExpression
        {
            get { return _validExpression; }
            set { _validExpression = value; }
        }

        private string _mask;
        public string Mask
        {
            get { return _mask; }
            set { _mask = value; }
        }

        private bool _validEntry;
        public bool ValidEntry
        {
            get { return _validEntry; }
            set { _validEntry = value; }
        }

        private string[] _validEntries;
        public string[] ValidEntries
        {
            get { return _validEntries; }
            set { _validEntries = value; }
        }

        private string _code;
        public string Code
        {
            get { return _code; }
            set { _code = value; }
        }

        private bool _hasReleatedField;
        public bool HasReleatedField
        {
            get { return _hasReleatedField; }
            set { _hasReleatedField = value; }
        }

        private string[] _releatedFields;
        public string[] ReleatedFields
        {
            get { return _releatedFields; }
            set { _releatedFields = value; }
        }

        private bool _isReleatedField;
        public bool IsReleatedField
        {
            get { return _isReleatedField; }
            set { _isReleatedField = value; }
        }

        private bool _isValidEntery;
        public bool IsValidEntery
        {
            get { return _isValidEntery; }
            set { _isValidEntery = value; }
        }

        public static TableField Copy(TableField tableField)
        {
            return (TableField)tableField.MemberwiseClone();
        }
    }
}
