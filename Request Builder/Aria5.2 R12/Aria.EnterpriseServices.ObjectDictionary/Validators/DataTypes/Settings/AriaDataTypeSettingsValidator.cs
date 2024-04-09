using System;
using System.Collections.Generic;
using System.Text;
using Aria.DataTypes.Settings;

namespace Aria.EnterpriseServices.ObjectDictionary.Validators.DataTypes.Settings
{
    public abstract class AriaDataTypeSettingsValidator
    {
        public abstract bool IsValueValid(AriaDataTypeSettings settings);
    }
}
