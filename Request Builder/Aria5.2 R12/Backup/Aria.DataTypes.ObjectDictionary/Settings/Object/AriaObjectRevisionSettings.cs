using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;
using System.ComponentModel;

namespace Aria.DataTypes.ObjectDictionary.Settings.Object
{
    [Serializable, AriaSerializableAttribute,TypeConverter(typeof(ExpandableObjectConverter))]
    public abstract class AriaObjectRevisionSettings 
    {
        private string _ParentDataObjectRevision;
        public string ParentDataObjectRevision
        {
            get { return _ParentDataObjectRevision; }
            set { _ParentDataObjectRevision = value; }
        }

        private AriaModificationTypes _modificationType;
        public AriaModificationTypes ModificationType
        {
            get { return _modificationType; }
            set { _modificationType = value; }
        }
    }
}
