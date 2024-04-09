using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;


namespace Aria.DataTypes.Settings
{
    [Serializable, AriaSerializableAttribute]
    public class AriaStandardDataTypeSettings : AriaDataTypeSettings
    {
        private AriaStandardDataType _dataType;
        public AriaStandardDataType DataType
        {
            get { return _dataType; }
            set { _dataType = value; }
        }
    }
}
