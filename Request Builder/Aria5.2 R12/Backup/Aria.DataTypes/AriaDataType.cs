using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Collections;
using Aria.DataTypes.Settings;
using Aria.Xml;

namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute]
    public abstract class AriaDataType
    {
        private AriaDataTypeSettings _settings;
        public AriaDataTypeSettings Settings
        {
            get { return _settings; }
            set { _settings = value; }
        }

        private Hashtable _propertyDataPathDictionary = new Hashtable();
        public Hashtable PropertyDataPathDictionary
        {
            get { return _propertyDataPathDictionary; }
            set { _propertyDataPathDictionary = value; }
        }

        public void AddPropertyDataPathEntry(string key, string value)
        {
            PropertyDataPathDictionary[key] = value;
        }
    }
}
