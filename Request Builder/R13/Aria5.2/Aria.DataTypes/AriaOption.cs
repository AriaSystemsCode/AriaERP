using System;
using System.Data;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute,
    AriaDataTypeSettings("Aria.DataTypes.Settings.AriaOptionSettings")]
    public class AriaOption : AriaDataType
    {
        private object _value;
        public object Value
        {
            get { return _value; }
            set { _value = value; }
        }
    }
}