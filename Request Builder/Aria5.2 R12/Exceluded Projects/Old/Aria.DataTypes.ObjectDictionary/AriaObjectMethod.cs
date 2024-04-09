using System;
using Aria.Xml;

namespace Aria.DataTypes.ObjectDictionary
{
    [Serializable, AriaSerializableAttribute]
    public class AriaObjectMethod
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

        private string _methodName = "";
        public string MethodName
        {
            get { return _methodName; }
            set { _methodName = value; }
        }

        private AriaModificationTypes _modificationType;
        public AriaModificationTypes ModificationType
        {
            get { return _modificationType; }
            set { _modificationType = value; }
        }

        private string _businessObjectParameterName = "";
        public string BusinessObjectParameterName
        {
            get { return _businessObjectParameterName; }
            set { _businessObjectParameterName = value; }
        }
    }
}