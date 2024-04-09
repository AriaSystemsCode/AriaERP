using System;
using Aria.DataTypes;
using Aria.DataTypes.Settings;
using Aria.Xml;


namespace Aria.DataTypes.ObjectDictionary
{
    [Serializable, AriaSerializableAttribute]
    public class AriaObjectProperty
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

        private string _propertyName = "";
        public string PropertyName
        {
            get { return _propertyName; }
            set { _propertyName = value; }
        }


        private string _propertyDescription = "";
        public string PropertyDescription
        {
            get { return _propertyDescription; }
            set { _propertyDescription = value; }
        }

        private AriaModificationTypes _modificationType;
        public AriaModificationTypes ModificationType
        {
            get { return _modificationType; }
            set { _modificationType = value; }
        }

        private AriaDataTypes _propertyType;
        public AriaDataTypes PropertyType
        {
            get { return _propertyType; }
            set { _propertyType = value; }
        }

        private AriaDataTypeSettings _propertySettings = null;
        public AriaDataTypeSettings PropertySettings
        {
            get { return _propertySettings; }
            set { _propertySettings = value; }
        }
    }
}