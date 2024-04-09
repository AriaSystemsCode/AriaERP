using System;
using System.Collections.Generic;
using System.Text;
using Aria.DataTypes.Settings;
using System.Collections;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute]
    public class AriaDataPath 
    {
        private string _dataPath;
        public string DataPath
        {
            get { return _dataPath; }
            set { _dataPath = value; }
        }


        private string _dataDescription;
        public string DataDescription
        {
            get { return _dataDescription; }
            set { _dataDescription = value; }
        }


        private AriaDataTypes _dataType;
        public AriaDataTypes DataType
        {
            get { return _dataType; }
            set { _dataType = value; }
        }

        private AriaDataTypeSettings  _settings;
        public AriaDataTypeSettings Settings
        {
            get { return _settings; }
            set { _settings = value; }
        }

        private bool _readOnly;
        public bool ReadOnly
        {
            get { return _readOnly; }
            set { _readOnly = value; }
        }

        private ArrayList _childDataPaths = new ArrayList();
        public ArrayList ChildDataPaths
        {
            get { return _childDataPaths; }
            set { _childDataPaths = value; }
        }
    }
}
