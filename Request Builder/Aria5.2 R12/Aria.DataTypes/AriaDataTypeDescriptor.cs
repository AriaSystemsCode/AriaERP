using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.DataTypes
{
    public class AriaDataTypeDescriptor
    {
        AriaDataTypes _type;
        public AriaDataTypes Type
        {
            get { return _type; }
            set { _type = value; }
        }

        public AriaDataTypeDescriptor(AriaDataTypes type)
        {
            _type = type;
        }

        public AriaDataTypeDescriptor(AriaDataType dataType)
        {
            switch (dataType.GetType().Name)
            {
                case "AriaDataObjectPointer":
                    Type = AriaDataTypes.DataObjectPointer;
                    break;
            }
        }

        public string GetClassName()
        {
            switch (_type)
            {
                case AriaDataTypes.DataObjectPointer:
                    return "Aria.DataTypes.AriaDataObjectPointer";

                default:
                    return "";
                    
            }
        }

        public string GetSettingsClassName()
        {
            switch (_type)
            {
                case AriaDataTypes.DataObjectPointer:
                    return "Aria.DataTypes.Settings.AriaDataObjectPointerSetting";

                case AriaDataTypes.Field:
                    return "Aria.DataTypes.Settings.AriaField";

                default:
                    return "";

            }
        }

        public string GetDataPoviderClassName()
        {
            switch (_type)
            {
                case AriaDataTypes.DataObjectPointer:
                    return "Aria.DataObjectProvider.AriaDataTypes.AriaDataObjectPointerDataProvider";

                default:
                    return "";

            }
        }

        public string GetDataPathsExplorerClassName()
        {
            switch (_type)
            {
                case AriaDataTypes.DataObjectPointer:
                    return "Aria.Server.ObjectDictionary.AriaDataObjectPointerDataPathExplorer";

                default:
                    return "";

            }
        }

        public string GetDataAccessClassName()
        {
            throw new Exception("The method or operation is not implemented.");
        }
    }
}
