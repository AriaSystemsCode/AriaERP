using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public class OptionGridVariable
    {
        private string _variableName;
        public string VariableName
        {
            get { return _variableName; }
            set { _variableName = value; }
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

        private FieldDataTypes _dataType;
        public FieldDataTypes DataType
        {
            get { return _dataType; }
            set { _dataType = value; }
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

        private string _defaultValue;
        public string DefaultValue
        {
            get { return _defaultValue; }
            set { _defaultValue = value; }
        }

        private OptionGridVariableValueTypes _defaultType;
        public OptionGridVariableValueTypes DefaultType
        {
            get { return _defaultType; }
            set { _defaultType = value; }
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

        private string _hideExpression;
        public string HideExpression
        {
            get { return _hideExpression; }
            set { _hideExpression = value; }
        }

        private string _editor;
        public string Editor
        {
            get { return _editor; }
            set { _editor = value; }
        }

        private string _browseDataObject;
        public string BrowseDataObject
        {
            get { return _browseDataObject; }
            set { _browseDataObject = value; }
        }

        private string _browseDataObjectRevision;
        public string BrowseDataObjectRevision
        {
            get { return _browseDataObjectRevision; }
            set { _browseDataObjectRevision = value; }
        }

        private string _browseFields;
        public string BrowseFields
        {
            get { return _browseFields; }
            set { _browseFields = value; }
        }

        private string _browseField;
        public string BrowseField
        {
            get { return _browseField; }
            set { _browseField = value; }
        }

        private string _browseFilter;
        public string BrowseFilter
        {
            get { return _browseFilter; }
            set { _browseFilter = value; }
        }

        private string _browseOpenFunction;
        public string BrowseOpenFunction
        {
            get { return _browseOpenFunction; }
            set { _browseOpenFunction = value; }
        }

        private string _browseSelectFunction;
        public string BrowseSelectFunction
        {
            get { return _browseSelectFunction; }
            set { _browseSelectFunction = value; }
        }

        private string _browseUnselectFunction;
        public string BrowseUnselectFunction
        {
            get { return _browseUnselectFunction; }
            set { _browseUnselectFunction = value; }
        }

        private UpgradeLevelTypes _upgradeLevel;
        public UpgradeLevelTypes UpgradeLevel
        {
            get { return _upgradeLevel; }
            set { _upgradeLevel = value; }
        }

        private Dictionary<string, string> _aria5 = new Dictionary<string, string>();
        public Dictionary<string, string> Aria5
        {
            get { return _aria5; }
            set { _aria5 = value; }
        }
    }
}
