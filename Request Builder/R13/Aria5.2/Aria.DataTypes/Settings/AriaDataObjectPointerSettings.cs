using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;

namespace Aria.DataTypes.Settings
{
    [Serializable, AriaSerializableAttribute]
    public class AriaDataObjectPointerSettings : AriaDataTypeSettings
    {
        private string _dataObjectName;
        public string DataObjectName
        {
            get { return _dataObjectName; }
            set { _dataObjectName = value; }
        }

        private string _dataObjectRevision;
        public string DataObjectRevision
        {
            get { return _dataObjectRevision; }
            set { _dataObjectRevision = value; }
        }
    }
}
