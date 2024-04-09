using Aria.DataTypes.ObjectDictionary;
using Aria.DataTypes;

namespace Aria.ParameterSubstitution
{
    public class AriaParameterizedConditions : AriaParameterizedItem
    {
        public AriaParameterizedConditions(AriaArgumentList arguments)
            : base(arguments)
        {
        }

        public void Substitute(AriaConditions condtions)
        {
            for (int index = 0; index < condtions.Items.Count; index++)
            {
                AriaCondtion condition = (AriaCondtion)condtions.Items[index];

                for (int leftIndex = 0; leftIndex < condition.LeftSideParameterizedValues.Count; leftIndex++)
                {
                    AriaCondtionParameterizedValue parameterizedValue = 
                            (AriaCondtionParameterizedValue)condition.LeftSideParameterizedValues[leftIndex];
                    AriaDataType leftSide = (AriaDataType)condition.LeftHandSide;

                    leftSide.SetValue(parameterizedValue.ConditionSideDataPath, GetValue(parameterizedValue.ToString()));
                }

                for (int RightIndex = 0; RightIndex < condition.RightSideParameterizedValues.Count; RightIndex++)
                {
                    AriaCondtionParameterizedValue parameterizedValue = 
                            (AriaCondtionParameterizedValue)condition.RightSideParameterizedValues[RightIndex];
                    AriaDataType RightSide = (AriaDataType)condition.RightHandSide;

                    RightSide.SetValue(parameterizedValue.ConditionSideDataPath, GetValue(parameterizedValue.ToString()));
                }
            }
        }
    }
}
