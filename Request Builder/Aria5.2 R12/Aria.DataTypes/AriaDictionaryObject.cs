using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.DataTypes
{
    public class AriaObjectReference : AriaDataType 
    {
        private object _value;
        public object Value
        {
            get { return _value; }
            set { _value = value; }
        }
    }
}
