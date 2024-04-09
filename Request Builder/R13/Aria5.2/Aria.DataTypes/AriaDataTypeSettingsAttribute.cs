using System;
using System.Collections.Generic;
using System.Text;


namespace Aria.DataTypes
{
    public class AriaDataTypeSettingsAttribute :  Attribute 
    {
        public AriaDataTypeSettingsAttribute(string className)
        {
            _className = className;
        }

        private string _className;
        public string ClassName
        {
            get { return _className; }
            set { _className = value; }
        }
    }
}
