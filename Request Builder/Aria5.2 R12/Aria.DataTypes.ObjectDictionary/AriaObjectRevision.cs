using System;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Xml;

namespace Aria.DataTypes.ObjectDictionary
{
    [Serializable, AriaSerializableAttribute]
    public class AriaObjectRevision
    {
        private int _objectID = 0;
        public int ObjectID
        {
            get { return _objectID; }
            set { _objectID = value; }
        }

        private string _objectRevision = "";
        public string ObjectRevision
        {
            get { return _objectRevision; }
            set { _objectRevision = value; }
        }

        private AriaObjectRevisionSettings _objectRevisionSettings = null;
        public AriaObjectRevisionSettings ObjectRevisionSettings
        {
            get { return _objectRevisionSettings; }
            set { _objectRevisionSettings = value; }
        }
    }
}
