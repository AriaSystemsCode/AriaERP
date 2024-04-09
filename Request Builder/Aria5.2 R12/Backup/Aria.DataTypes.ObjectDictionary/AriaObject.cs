using System;
using Aria.Xml;

namespace Aria.DataTypes.ObjectDictionary
{
    [Serializable, AriaSerializableAttribute]
    public class AriaObject
    {           
        private int _objectID = 0;
        public int ObjectID
        {
            get { return _objectID; }
            set { _objectID = value; }
        }

        private string _objectName = "";
        public string ObjectName
        {
            get { return _objectName; }
            set { _objectName = value; }
        }

        private string _objectDescription = "";
        public string ObjectDescription
        {
            get { return _objectDescription; }
            set { _objectDescription = value; }
        }

        private AriaObjectTypes _objectType;
        public AriaObjectTypes ObjectType
        {
            get { return _objectType; }
            set { _objectType = value; }
        }

        private int _parentObjectID = 0;

        public int ParentObjectID
        {
            get { return _parentObjectID; }
            set { _parentObjectID = value; }
        }

        public string ActiveRevision
        {
            get { return _activeRevision; }
            set { _activeRevision = value; }
        }

        private string _activeRevision = "";



    }
}