using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.DataTypes.ObjectDictionary;
using Aria.Environment;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.Xml;
using System.Text.RegularExpressions;
using System.Data;
using Aria.DataTypes.Settings;
using Aria.DataTypes;
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;

namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    abstract class AriaDictionaryObject
    {
        public static string CurrentRevision = "000001";

        public static Aria50SchemaInformation AriaSchema;

        public AriaDictionaryObject Parent;

        public AriaDictionaryObject()
        {
        }

        public AriaDictionaryObject(AriaDictionaryObject obj)
        {
            Parent = obj;
        }

        public abstract void Save();

        public static AriaDictionaryObject GetParent(AriaDictionaryObject current, Type type)
        {
            if (current.Parent == null) return null;

            if (current.Parent.GetType().ToString() == type.ToString())
            {
                return current.Parent;
            }
            else
            {
                return GetParent(current.Parent, type);
            }
        }

        public static bool cutCycle(AriaDictionaryObject current, Type type,string objectName)
        {
            if (current.Parent == null) return true;
            else if (current.Parent.GetType().ToString() == type.ToString())
            {
                if(((AriaRelatedDataObject)current.Parent).AriaObject.ObjectName.Contains(objectName)) return false;
                else  return cutCycle(current.Parent, type, objectName);  
            }
            else return cutCycle(current.Parent, type, objectName); 

         return false;
        }
    }
}
