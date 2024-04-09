    using System;
using System.Data;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute,
    AriaDataTypeSettings("Aria.DataTypes.Settings.AriaStandardDataTypeSettings")]
    public class AriaStandardDataType : AriaDataType 
    {
        private object _value;
        public object Value
        {
            get { return _value; }
            set { _value = value; }
        }
    }
}