using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.DataTypes
{
    public class AriaParameterizedValue
    {
        private AriaParameterizedValueTypes _type;
        public AriaParameterizedValueTypes Type
        {
            get { return _type; }
            set { _type = value; }
        }

        private string _parameterName;
        public string ParameterName
        {
            get { return _parameterName; }
            set { _parameterName = value; }
        }

        private string _parameterDataPath;
        public string ParameterDataPath
        {
            get { return _parameterDataPath; }
            set { _parameterDataPath = value; }
        }

        private string _expression;
        public string Expression
        {
            get { return _expression; }
            set { _expression = value; }
        }

        public override string ToString()
        {
            switch (Type)
            {
                case AriaParameterizedValueTypes.Expression:
                    return "Expression:" + Expression;

                case AriaParameterizedValueTypes.Parameter:
                    return ParameterName + ":" + ParameterDataPath;

                default:
                    return "";
            }
        }
    }
}
