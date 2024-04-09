using System;

namespace Aria.Data
{
    /// <summary>
    /// This class contains properties that handle parameters (name and value)
    /// </summary>
    public class AriaDbParameter
    {        
        private string _parameterName;
        public string ParameterName
        {
            get { return _parameterName; }
            set { _parameterName = value; }
        }
        
        private object _parameterValue;
        public object ParameterValue
        {
            get { return _parameterValue; }
            set { _parameterValue = value; }
        }

        public AriaDbParameter(string parameterName, object parameterValue)
        {
            _parameterName = parameterName;
            _parameterValue = parameterValue;
        }
    }
}
