using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;


namespace Aria.DataTypes.Settings
{
    [Serializable, AriaSerializableAttribute]
    public class AriaRangeSettings : AriaDataTypeSettings
    {
        private AriaDataTypes _dataType;
        public AriaDataTypes DataType
        {
            get { return _dataType; }
            set { _dataType = value; }
        }

        private AriaDataTypeSettings _settings;
        public AriaDataTypeSettings Settings
        {
            get { return _settings; }
            set { _settings = value; }
        }
    }
}
