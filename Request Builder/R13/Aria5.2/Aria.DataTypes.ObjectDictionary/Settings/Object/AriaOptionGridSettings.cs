using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;

namespace Aria.DataTypes.ObjectDictionary.Settings.Object
{
    [Serializable, AriaSerializableAttribute]
    public class AriaOptionGridSettings : AriaObjectRevisionSettings
    {
        private string _optionGridId;
        public string OptionGridId
        {
            get { return _optionGridId; }
            set { _optionGridId = value; }
        }
    }
}
