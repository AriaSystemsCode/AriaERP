using System;
using Aria.DataTypes;
using Aria.DataTypes.Settings;
using Aria.Xml;

namespace Aria.DataTypes.ObjectDictionary
{
    [Serializable, AriaSerializableAttribute]
    public class AriaObjectMethodParameter
    {        
        private int _objectID = 0;
        public int ObjectID
        {
            get { return _objectID; }
            set { _objectID = value; }
        }

        private string _objectRevision = "";
        public string ObjectRevision
        {
            get { return _objectRevision; }
            set { _objectRevision = value; }
        }

        private string _methodName = "";
        public string MethodName
        {
            get { return _methodName; }
            set { _methodName = value; }
        }

        private int _parameterNo = 0;
        public int ParameterNo
        {
            get { return _parameterNo; }
            set { _parameterNo = value; }
        }

        private string _parameterName = "";
        public string ParameterName
        {
            get { return _parameterName; }
            set { _parameterName = value; }
        }

        private AriaDataTypes _parameterType;
        public AriaDataTypes ParameterType
        {
            get { return _parameterType; }
            set { _parameterType = value; }
        }

        private AriaDataTypeSettings _parameterSettings = null;
        public AriaDataTypeSettings ParameterSettings
        {
            get { return _parameterSettings; }
            set { _parameterSettings = value; }
        }
    }
}