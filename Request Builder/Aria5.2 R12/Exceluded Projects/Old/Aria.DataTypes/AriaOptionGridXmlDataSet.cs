using System;
using System.Data;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute,
    AriaDataTypeSettings("Aria.DataTypes.Settings.AriaOptionGridXmlDataSetSettings"),
    AriaDataTypeDataAdapter("Aria.Data.DataTypes.AriaOptionGridXmlDataSetAdapter")]
    public class AriaOptionGridXmlDataSet : AriaDataType
    {
        private string _fileName;
        public string FileName
        {
            get { return _fileName; }
            set { _fileName = value; }
        }
    }
}