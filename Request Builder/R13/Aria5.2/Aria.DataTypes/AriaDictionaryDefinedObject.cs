using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute]
    public class AriaDictionaryDefinedObject : AriaDataType 
    {
        private object _value;
        public object Value
        {
            get { return _value; }
            set { _value = value; }
        }
    }
}
