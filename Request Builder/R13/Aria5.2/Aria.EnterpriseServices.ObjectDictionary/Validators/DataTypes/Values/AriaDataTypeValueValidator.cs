using System;
using System.Collections.Generic;
using System.Text;
using Aria.DataTypes;
using Aria.DataTypes.Settings;

namespace Aria.EnterpriseServices.ObjectDictionary.Validators.DataTypes.Values
{
    public abstract class AriaDataTypeValueValidator
    {
        public abstract bool IsValueValid(AriaDataTypeSettings settings, AriaDataType value);
    }
}
