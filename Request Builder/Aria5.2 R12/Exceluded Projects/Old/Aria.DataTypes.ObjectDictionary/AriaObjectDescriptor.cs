using System;
using Aria.Xml;

namespace Aria.DataTypes.ObjectDictionary
{
    [Serializable, AriaSerializableAttribute]
    public class AriaObjectDescriptor
    {
        AriaObjectTypes _type;
        public AriaObjectTypes Type
        {
            get { return _type; }
            set { _type = value; }
        }

        public AriaObjectDescriptor(AriaObjectTypes type)
        {
            _type = type;
        }

        public string GetSettingsClassName()
        {
            switch (_type)
            {
                case AriaObjectTypes.Data:
                    return "Aria.DataTypes.ObjectDictionary.Settings.Object.AriaDataObjectSetting";
                    
                case AriaObjectTypes.RelatedData:
                    return "Aria.DataTypes.ObjectDictionary.Settings.Object.AriaRelatedDataObjectSetting";

                case AriaObjectTypes.Presenatation:
                    return "";

                case AriaObjectTypes.Server:
                    return "";

                case AriaObjectTypes.OptionGrid:
                    return "";

                case AriaObjectTypes.Report:
                    return "";

                case AriaObjectTypes.Utility:
                    return "";

                case AriaObjectTypes.Framework:
                    return "";

                default:
                    return "";
            }
        }
    }
}
