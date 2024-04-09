using System;
using System.Data;
using System.Collections;
using System.Collections.Generic;
using Aria.DataTypes.Settings;


namespace Aria.DataTypes
{
    [AriaSetting("Aria.DataTypes.Settings.DataPointerSetting")]
    public class DataPointer : AriaDataType
    {
        private List<DataPointerKeyField> _keyFields;
        public List<DataPointerKeyField> KeyFields
        {
            get { return _keyFields; }
            set { _keyFields = value; }
        }
   }
}