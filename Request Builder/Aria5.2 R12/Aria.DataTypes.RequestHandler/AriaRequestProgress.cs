using System;
using System.Collections;
using Aria.Xml;
using System.ComponentModel;

namespace Aria.DataTypes.RequestHandler
{
    [Serializable, AriaSerializableAttribute, TypeConverter(typeof(ExpandableObjectConverter))]
    public class AriaRequestProgress
    {
        private string _description = "";
        public string Description
        {
            get { return _description; }
            set { _description = value; }
        }

        private double _percent = 0.0;
        public double Percent
        {
            get { return _percent; }
            set { _percent = value; }
        }
    }
}
