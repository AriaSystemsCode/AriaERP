using System;
using System.Data;
using System.Collections;
using System.Collections.Generic;
using Aria.DataTypes.Settings;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute,
     AriaDataTypeSettings("Aria.DataTypes.Settings.AriaDataObjectPointerSettings"),
     AriaDataTypeDataAdapter("Aria.Data.DataTypes.AriaDataObjectPointerAdapter")]
    public class AriaDataObjectPointer : AriaDataType 
    {
        private ArrayList _keyFields = new ArrayList();
        public ArrayList KeyFields
        {
            get { return _keyFields; }
            set { _keyFields = value; }
        }

        public void AddKeyField(string fieldName, object value)
        {
            
            _keyFields.Add(new AriaDataObjectPointerKeyField(fieldName, value));
        }
    }
}