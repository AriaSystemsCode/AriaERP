using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;

namespace Aria.DataTypes.ObjectDictionary.Settings.Object
{
    [AriaSerializableAttribute, Serializable]
    public class AriaServerObjectSettings : AriaObjectRevisionSettings
    {
        private string _className = "";
        public string ClassName
        {
            get { return _className; }
            set { _className = value; }
        }
    }
}
