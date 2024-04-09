using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;


namespace Aria.DataTypes.ObjectDictionary.Settings.Object
{
    [AriaSerializableAttribute, Serializable]
    public class AriaReportObjectSettings : AriaObjectRevisionSettings
    {
        private string _className = "";
        public string ClassName
        {
            get { return _className; }
            set { _className = value; }
        }

        private AriaOutputFormatTypes[] _supportedFormats;
        public AriaOutputFormatTypes[] SupportedFormats
        {
            get { return _supportedFormats; }
            set { _supportedFormats = value; }
        }
    }
}
