using System;
using System.Collections.Generic;
using System.Text;
using Aria.Xml;
using System.ComponentModel;

namespace Aria.DataTypes.Settings
{
    [Serializable, AriaSerializableAttribute, TypeConverter(typeof(ExpandableObjectConverter))]
    public abstract class AriaDataTypeSettings
    {
    }
}
