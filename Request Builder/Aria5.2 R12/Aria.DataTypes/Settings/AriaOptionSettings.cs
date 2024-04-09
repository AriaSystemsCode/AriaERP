using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using Aria.Xml;


namespace Aria.DataTypes.Settings
{
    [Serializable, AriaSerializableAttribute]
    public class AriaOptionSettings : AriaDataTypeSettings
    {
        private string _variableName;
        [Category("General")]
        public string VariableName
        {
            get { return _variableName; }
            set { _variableName = value; }
        }

        private string _fieldName;
        [Category("General")]
        public string FieldName
        {
            get { return _fieldName; }
            set { _fieldName = value; }
        }

        private string _description;
        [Category("General")]
        public string Description
        {
            get { return _description; }
            set { _description = value; }
        }

        private string _head;
        [Category("General")]
        public string Head
        {
            get { return _head; }
            set { _head = value; }
        }

        private string _message;
        [Category("General")]
        public string Message
        {
            get { return _message; }
            set { _message = value; }
        }

        private AriaStandardDataTypes _dataType;
        [Category("Data")]
        public AriaStandardDataTypes DataType
        {
            get { return _dataType; }
            set { _dataType = value; }
        }

        private int _width;
        [Category("Data")]
        public int Width
        {
            get { return _width; }
            set { _width = value; }
        }

        private int _decimalPlaces;
        [Category("Data")]
        public int DecimalPlaces
        {
            get { return _decimalPlaces; }
            set { _decimalPlaces = value; }
        }

        private string _defaultValue;
        [Category("Default Value")]
        public string DefaultValue
        {
            get { return _defaultValue; }
            set { _defaultValue = value; }
        }

        private AriaOptionDefaultValueTypes _defaultType;
        [Category("Default Value")]
        public AriaOptionDefaultValueTypes DefaultType
        {
            get { return _defaultType; }
            set { _defaultType = value; }
        }

        private string _validExpression;
        [Category("Validation")]
        public string ValidExpression
        {
            get { return _validExpression; }
            set { _validExpression = value; }
        }

        private string _mask;
        [Category("Validation")]
        public string Mask
        {
            get { return _mask; }
            set { _mask = value; }
        }

        private bool _validEntry;
        [Category("Validation")]
        public bool ValidEntry
        {
            get { return _validEntry; }
            set { _validEntry = value; }
        }

        private string[] _validEntries;
        [Category("Validation")]
        public string[] ValidEntries
        {
            get { return _validEntries; }
            set { _validEntries = value; }
        }

        private string _code;
        [Category("Validation")]
        public string Code
        {
            get { return _code; }
            set { _code = value; }
        }

        private string _hideExpression;
        [Category("Appearance")]
        public string HideExpression
        {
            get { return _hideExpression; }
            set { _hideExpression = value; }
        }

        private string _editor;
        [Category("Editor")]
        public string Editor
        {
            get { return _editor; }
            set { _editor = value; }
        }

        private string _browseDataObject;
        [Category("Browse")]
        public string BrowseDataObject
        {
            get { return _browseDataObject; }
            set { _browseDataObject = value; }
        }

        private string _browseDataObjectRevision;
        [Category("Browse")]
        public string BrowseDataObjectRevision
        {
            get { return _browseDataObjectRevision; }
            set { _browseDataObjectRevision = value; }
        }

        private string _browseFields;
        [Category("Browse")]
        public string BrowseFields
        {
            get { return _browseFields; }
            set { _browseFields = value; }
        }

        private string _browseField;
        [Category("Browse")]
        public string BrowseField
        {
            get { return _browseField; }
            set { _browseField = value; }
        }

        private string _browseFilter;
        [Category("Browse")]
        public string BrowseFilter
        {
            get { return _browseFilter; }
            set { _browseFilter = value; }
        }

        private string _browseOpenFunction;
        [Category("Browse")]
        public string BrowseOpenFunction
        {
            get { return _browseOpenFunction; }
            set { _browseOpenFunction = value; }
        }

        private string _browseSelectFunction;
        [Category("Browse")]
        public string BrowseSelectFunction
        {
            get { return _browseSelectFunction; }
            set { _browseSelectFunction = value; }
        }

        private string _browseUnselectFunction;
        [Category("Browse")]
        public string BrowseUnselectFunction
        {
            get { return _browseUnselectFunction; }
            set { _browseUnselectFunction = value; }
        }

        private AriaConditionOperators _operator;
        [Category("Selection")]
        public AriaConditionOperators Operator
        {
            get { return _operator; }
            set { _operator = value; }
        }
    }
}
