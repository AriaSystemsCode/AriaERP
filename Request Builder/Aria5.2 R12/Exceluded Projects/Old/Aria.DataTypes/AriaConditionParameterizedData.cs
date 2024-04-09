using System;
using System.Collections.Generic;
using System.Text;
using Aria.DataTypes;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute]
    public class AriaConditionParameterizedData : AriaParameterizedData
    {
        private string _conditionSideDataPath;
        public string ConditionSideDataPath
        {
            get { return _conditionSideDataPath; }
            set { _conditionSideDataPath = value; }
        }
    }
}
