using System;
using System.Data;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute,
    AriaDataTypeSettings("Aria.DataTypes.Settings.AriaFoxDataCursorSettings")]
    public class AriaFoxDataCursor : AriaDataType
    {
        private string _fileName;
        public string FileName
        {
            get { return _fileName; }
            set { _fileName = value; }
        }
    }
}