using System;
using System.Collections.Generic;
using System.Text;
using Aria.DataTypes;
using Aria.DataTypes.Settings;
using System.Collections;

namespace Aria.EnterpriseServices.ObjectDictionary.DataPathsExplorers
{
    public abstract class AriaDataPathsExplorer
    {
        public abstract AriaDataPath GetDataPathsTree(AriaDataTypeSettings settings);
    }
}
