using System;
using System.Collections.Generic;
using System.Text;
using Aria.DataTypes;

namespace Aria.DataTypes
{
    public class AriaCondtionParameterizedValue : AriaParameterizedValue
    {
        private string _conditionSideDataPath;
        public string ConditionSideDataPath
        {
            get { return _conditionSideDataPath; }
            set { _conditionSideDataPath = value; }
        }
    }
}
