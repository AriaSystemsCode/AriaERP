using System;
using System.Collections;

namespace Aria.Data
{
    /// <summary>
    /// This class is collection type where used to save list of parameters and retrieve this parameter depend on index and enable from retrieve count of parameters
    /// </summary>
    public class AriaDbParameterCollection
    {
        private ArrayList _parameterList = new ArrayList();

        public int Add(string parameterName, object parameterValue)
        {
            return _parameterList.Add(new AriaDbParameter(parameterName, parameterValue));
        }

        public int Add(AriaDbParameter value)
        {
            return _parameterList.Add(value);
        }

        public int Count
        {
            get { return _parameterList.Count; }
        }
        
        public AriaDbParameter this[int index]
        {
            get { return (AriaDbParameter)_parameterList[index]; }
        }

        public void Clear()
        {
            _parameterList.Clear();
        }
    }
}
