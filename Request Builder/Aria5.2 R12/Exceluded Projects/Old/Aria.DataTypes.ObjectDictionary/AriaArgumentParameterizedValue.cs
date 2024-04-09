using System;
using Aria.DataTypes;
using Aria.Xml;

namespace Aria.DataTypes.ObjectDictionary
{
    [Serializable, AriaSerializableAttribute]
    public class AriaArgumentParameterizedData : AriaParameterizedData
    {
        private string _argumentDataPath = "";
        public string ArgumentDataPath
        {
            get { return _argumentDataPath; }
            set { _argumentDataPath = value; }
        }
    }
}
