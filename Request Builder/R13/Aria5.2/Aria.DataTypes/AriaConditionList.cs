using System;
using System.Data;
using System.Collections.Generic;
using System.Collections;
using Aria.Xml;


namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute]
    public class AriaConditionList : AriaList
    {
        public bool AreConditionsValid()
        {
            for (int conditionIndex = 0; conditionIndex < Items.Count; conditionIndex++)
            {
                AriaCondition condition = (AriaCondition)Items[conditionIndex];

                if (!condition.IsConditionValid())
                {
                    return false;
                }
            }
            return true;
        }
    }
}