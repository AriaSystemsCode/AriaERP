using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using Aria.Xml;

namespace Aria.DataTypes
{
    [Serializable, AriaSerializableAttribute,
    AriaDataTypeSettings("Aria.DataTypes.Settings.AriaListSettings")]
    public class AriaList : AriaDataType 
    {
        private ArrayList _items = new ArrayList();
        public ArrayList Items
        {
            get { return _items; }
            set { _items = value; }
        }
    }
}
