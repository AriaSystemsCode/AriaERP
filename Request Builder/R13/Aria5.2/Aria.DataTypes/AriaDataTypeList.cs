using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;

namespace Aria.DataTypes
{
    public class AriaDataTypeList : AriaDataType 
    {
        private ArrayList _items = new ArrayList();
        public ArrayList Items
        {
            get { return _items; }
            set { _items = value; }
        } 
    }
}
