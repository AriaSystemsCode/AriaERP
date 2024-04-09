using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute,
    AriaDataTypeSettings("Aria.DataTypes.Settings.AriaRangeSettings")]
    public class AriaRange : AriaDataType 
    {
        private AriaDataType _from = new AriaStandardDataType();
        public AriaDataType From
        {
            get { return _from; }
            set { _from = value; }
        }

        private AriaDataType _to = new AriaStandardDataType();
        public AriaDataType To
        {
            get { return _to; }
            set { _to = value; }
        }
    }
}
