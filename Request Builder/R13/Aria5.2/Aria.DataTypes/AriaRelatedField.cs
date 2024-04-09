using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute,
    AriaDataTypeSettings("Aria.DataTypes.Settings.AriaRelatedFieldSettings")]
    public class AriaRelatedField
    {
        private object _value;
        public object Value
        {
            get { return _value; }
            set { _value = value; }
        }
    }
}
