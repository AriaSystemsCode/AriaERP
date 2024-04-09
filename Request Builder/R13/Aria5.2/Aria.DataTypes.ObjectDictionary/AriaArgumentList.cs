using System;
using System.Collections;
using Aria.DataTypes;
using Aria.Xml;

namespace Aria.DataTypes.ObjectDictionary
{
    [Serializable, AriaSerializableAttribute]
    public class AriaArgumentList : ArrayList
    {
        private string _businessObjectParameterName = "";
        public string BusinessObjectParameterName
        {
            get { return _businessObjectParameterName; }
            set { _businessObjectParameterName = value; }
        }

        public override int Add(object value)
        {
            if (value.GetType().Equals(typeof(AriaArgument)))
                return base.Add(value);
            else
                throw new Exception("Not AriaArgument Object");
        }
        
        public void AddArgument(string name, AriaDataType value)
        {
            AriaArgument argument = new AriaArgument();
            
            argument.ParameterName = name;
            
            argument.Value = value;
            
            this.Add(argument);
        }

        public void AddParameterizedData(string parameterName, string dataPath, string expression)
        {
            AriaArgumentParameterizedData parameterizedData;

            for (int index = 0; index < this.Count; index++)
            {
                if (((AriaArgument)this[index]).ParameterName == parameterName)
                {
                    parameterizedData = new AriaArgumentParameterizedData();
                    
                    parameterizedData.ArgumentDataPath = dataPath;
                    parameterizedData.Expression = expression;
                    

                    ((AriaArgument)this[index]).ParameterizedValues.Add(parameterizedData);
                }
            }
        }
    }
}
