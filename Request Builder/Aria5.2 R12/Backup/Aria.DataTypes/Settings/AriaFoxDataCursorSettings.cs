using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.DataTypes.Settings
{
    public class AriaFoxDataCursorSettings : AriaDataTypeSettings
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
