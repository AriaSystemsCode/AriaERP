using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;

namespace Aria.DataTypes
{

    [Serializable, AriaSerializableAttribute]
    public class AriaParameterizedData
    {
        private string _expression;
        public string Expression
        {
            get { return _expression; }
            set { _expression = value; }
        }
    }
}
