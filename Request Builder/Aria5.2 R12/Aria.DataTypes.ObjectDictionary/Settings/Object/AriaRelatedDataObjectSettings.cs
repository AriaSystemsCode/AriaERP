using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;

namespace Aria.DataTypes.ObjectDictionary.Settings.Object
{
    [Serializable, AriaSerializableAttribute]
    public class AriaRelatedDataObjectSettings : AriaObjectRevisionSettings
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

        private string _filter;
        public string Filter
        {
            get { return _filter; }
            set { _filter = value; }
        }
    }
}
