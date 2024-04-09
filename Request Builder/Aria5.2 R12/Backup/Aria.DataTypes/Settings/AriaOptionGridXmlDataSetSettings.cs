using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;

namespace Aria.DataTypes.Settings
{
    [Serializable, AriaSerializableAttribute]
    public class AriaOptionGridXmlDataSetSettings : AriaDataTypeSettings
    {
        private string _optionGridObjectName;
        public string OptionGridObjectName
        {
            get { return _optionGridObjectName; }
            set { _optionGridObjectName = value; }
        }

        private string _optionGridRevision;
        public string OptionGridRevision
        {
            get { return _optionGridRevision; }
            set { _optionGridRevision = value; }
        }
    }
}
