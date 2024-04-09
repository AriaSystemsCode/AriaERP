using System;
using System.Collections.Generic;
using System.Text;


namespace Aria.DataTypes
{
    public enum AriaDataTypes
    {
        AriaDataObjectPointer,
        AriaField,
        AriaRelatedField,
        AriaDictionaryDefinedObject,
        AriaOption,
        AriaOptionGridXmlDataSet
    }

    public enum AriaStandardDataTypes
    {
        String,
        Int,
        Logical,
        Numeric,
        Date,
        Memo,
        Binary
    }

    public enum AriaConditionOperators
    {
        Like,
        GreaterThan,
        LessThan,
        GreaterOrEqual,
        LessOrEqual,
        Between,
        Contains,
        InList
    }

    public enum AriaOptionDefaultValueTypes
    {
        Fixed,
        Expression
    }
}
