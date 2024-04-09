using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute]
    public class AriaDataObjectPointerKeyField 
    {
        private string _fieldName;
        public string FieldName
        {
            get { return _fieldName; }
            set { _fieldName = value; }
        }
        
        private object _value;
        public object Value
        {
            get { return _value; }
            set { _value = value; }
        }

        public AriaDataObjectPointerKeyField()
        {
        }

        public AriaDataObjectPointerKeyField(string fieldName, object value)
        {
            _fieldName = fieldName;
            _value = value;
        }

    }
}
