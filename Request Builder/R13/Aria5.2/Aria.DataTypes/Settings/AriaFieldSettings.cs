using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;


namespace Aria.DataTypes.Settings
{
    [Serializable]
    public class AriaFieldSettings : AriaDataTypeSettings
    {
        private string _fieldName;
        [Category("General")]
        public string FieldName
        {
            get { return _fieldName; }
            set { _fieldName = value; }
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

        private bool _isPrimaryKey;
        [Category("Data")]
        public bool IsPrimaryKey
        {
            get { return _isPrimaryKey; }
            set { _isPrimaryKey = value; }
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


        private bool _hasReleatedField;
        [Category("Related Information")]
        public bool HasReleatedField
        {
            get { return _hasReleatedField; }
            set { _hasReleatedField = value; }
        }

        private string[] _releatedFields;
        [Category("Related Information")]
        public string[] ReleatedFields
        {
            get { return _releatedFields; }
            set { _releatedFields = value; }
        }

        private bool _isReleatedField;
        [Category("Related Information")]
        public bool IsReleatedField
        {
            get { return _isReleatedField; }
            set { _isReleatedField = value; }
        }

        private bool _isEmail;
        [Category("Misc")]
        public bool IsEmail
        {
            get { return _isEmail; }
            set { _isEmail = value; }
        }

        private bool _InternalEmail;
        [Category("Misc")]
        public bool InternalEmail
        {
            get { return _InternalEmail; }
            set { _InternalEmail = value; }
        }

        private string _emailAddressSeparator = "";
        [Category("Misc")]
        public string EmailAddressSeparator
        {
            get { return _emailAddressSeparator; }
            set { _emailAddressSeparator = value; }
        }

        //[Add]Ahmed Maher Date: 09/04/2010 -Notify
        private string _internalEmailSelect = "";
        [Category("Misc")]
        public string InternalEmailSelect
        {
            get { return _internalEmailSelect; }
            set { _internalEmailSelect = value; }
        }

        private string _internalEmailParameter = "";
        [Category("Misc")]
        public string InternalEmailParameter
        {
            get { return _internalEmailParameter; }
            set { _internalEmailParameter = value; }
        }

        private string _internalEmailConnectionType = "";
        [Category("Misc")]
        public string InternalEmailConnectionType
        {
            get { return _internalEmailConnectionType; }
            set { _internalEmailConnectionType = value; }
        }
        //[END]
    }
}
