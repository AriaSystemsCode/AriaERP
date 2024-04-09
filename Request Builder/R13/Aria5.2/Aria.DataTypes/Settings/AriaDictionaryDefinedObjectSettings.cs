using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.DataTypes.Settings
{
    public class AriaDictionaryDefinedObjectSettings : AriaDataTypeSettings
    {
        private string _objectName;
        public string ObjectName
        {
            get { return _objectName; }
            set { _objectName = value; }
        }

        private string _objectRevision;
        public string ObjectRevision
        {
            get { return _objectRevision; }
            set { _objectRevision = value; }
        }
    }
}
