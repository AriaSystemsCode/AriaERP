using System;
using Aria.DataTypes;
using Aria.DataTypes.Settings;
using System.Collections;
using Aria.Xml;

namespace Aria.DataTypes.ObjectDictionary
{
    [Serializable, AriaSerializableAttribute]
    public class AriaArgument
    {
        private string _parameterName = "";
        public string ParameterName
        {
            get { return _parameterName; }
            set { _parameterName = value; }
        }

        private AriaDataTypeSettings _settings = null;
        public AriaDataTypeSettings Settings
        {
            get { return _settings; }
            set { _settings = value; }
        }

        private AriaDataType _value = null;
        public AriaDataType Value
        {
            get { return _value; }
            set { _value = value; }
        }

        private ArrayList _parameterizedDataList = null;
        public ArrayList ParameterizedValues
        {
            get { return _parameterizedDataList; }
            set { _parameterizedDataList = value; }
        }
    }
}
