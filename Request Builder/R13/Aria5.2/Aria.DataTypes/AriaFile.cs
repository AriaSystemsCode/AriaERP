using System;
using System.Data;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute]
    public class AriaFile : AriaDataType
    {
        private String _fileName;
        public String FileName
        {
            get { return _fileName; }
            set { _fileName = value; }
        }
    }
}