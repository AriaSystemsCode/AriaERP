using Aria.DataTypes.ObjectDictionary;
using System.Collections;

namespace Aria.ParameterSubstitution
{
    public class AriaParameterizedArguments : AriaParameterizedItem
    {
        public AriaParameterizedArguments(AriaArgumentList sourceArguments)
            : base(sourceArguments)
        {
        }

        public void Substitute(AriaArgumentList targetArguments)
        {
            for(int index = 0; index < targetArguments.Count ; index ++)
            {
                AriaArgument argument = (AriaArgument)targetArguments[index];

                ArrayList ParameterizedValues = argument.ParameterizedValues;
                
                for (int innerIndex = 0; innerIndex < ParameterizedValues.Count; innerIndex++)
                {
                    AriaArgumentParameterizedValue parameterizedValue = (AriaArgumentParameterizedValue)ParameterizedValues[innerIndex];

                    argument.Value.SetValue(parameterizedValue.ArgumentDataPath, GetValue(parameterizedValue.ToString()));
                }

            }
        }
    }
}
