using System;
using System.Collections.Generic;
using System.Text;
using Aria.DataTypes.Settings;
using Aria.DataTypes;
using Aria.DataTypes.ObjectDictionary;
using System.Collections;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Data;


namespace Aria.EnterpriseServices.ObjectDictionary
{
    /// <summary>
    /// Used to get all data paths as Email Paths and data paths
    /// </summary>
    public class AriaObjectDataPathsExplorer : MarshalByRefObject
    {
        public ArrayList GetAriaObjectTypes()
        {
            ArrayList objectTypesNamesList = new ArrayList(Enum.GetNames(typeof(AriaObjectTypes)));

            return objectTypesNamesList;
        }


        public ArrayList GetDataPathsTree(string objectName,string  clientId)
        {
            ArrayList result = new ArrayList(); 

            AriaDbConnection conection = new AriaDbConnection("Aria", "");
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaObjectRevision objectRevision = objectDictionary.LoadActiveRevision(conection, objectName, clientId);

            ArrayList childObjects = objectDictionary.LoadAriaObjectChildObjectsOfType(conection, objectName, AriaObjectTypes.RelatedData, clientId);
            for (int index = 0; index < childObjects.Count; index++)
            {
                AriaDataPath objectDataPath = new AriaDataPath();
                objectDataPath.DataPath = ((AriaObject)childObjects[index]).ObjectName.Remove(0, objectName.Length + 1).Trim();
                objectDataPath.ChildDataPaths = GetDataPathsTree( ((AriaObject)childObjects[index]).ObjectName, clientId);
                objectDataPath.DataDescription = ((AriaObject)childObjects[index]).ObjectDescription.Trim();
              
                result.Add(objectDataPath);
            }

            ArrayList properties = objectDictionary.LoadAriaObjectProperties(conection, objectName, objectRevision.ObjectRevision, false, clientId);

            for (int index = 0; index < properties.Count; index++)
            {
                AriaDataPath propertyDataPath = new AriaDataPath();
                propertyDataPath.DataPath = ((AriaObjectProperty)properties[index]).PropertyName.Trim();
                propertyDataPath.DataType = ((AriaObjectProperty)properties[index]).PropertyType ;
                propertyDataPath.Settings =  ((AriaObjectProperty)properties[index]).PropertySettings;
                propertyDataPath.DataDescription = ((AriaObjectProperty)properties[index]).PropertyDescription;

                result.Add(propertyDataPath);
            }

            return result;
        }

        public ArrayList GetEmalisDataPaths(string objectName,string clientId)
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();

            ArrayList result = dictionary.LoadAriaObjectProperties(new AriaDbConnection("Aria", ""), objectName, dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), objectName, clientId).ObjectRevision, false, clientId);
            result.AddRange(dictionary.LoadAriaObjectRelatedProperties(new AriaDbConnection("Aria", ""), objectName, dictionary.LoadActiveRevision(new AriaDbConnection("Aria", ""), objectName, clientId).ObjectRevision, false, clientId));

            for (int index = result.Count - 1; index > -1; index--)
            {
                if (((AriaObjectProperty)result[index]).PropertyType == AriaDataTypes.AriaField)
                {
                    if (!((AriaFieldSettings)((AriaObjectProperty)result[index]).PropertySettings).IsEmail)
                    {
                        result.RemoveAt(index);
                    }
                }
                else if (((AriaObjectProperty)result[index]).PropertyType == AriaDataTypes.AriaRelatedField)
                {
                    if (!((AriaRelatedFieldSettings)((AriaObjectProperty)result[index]).PropertySettings).IsEmail)
                    {
                        result.RemoveAt(index);
                    }
                }
            }

            ArrayList dataPaths = new ArrayList();
            for (int index = 0; index < result.Count; index++)
            {
                if (((AriaObjectProperty)result[index]).PropertyType == AriaDataTypes.AriaField)
                {
                    string pathObjectName = dictionary.LoadAriaObjectById(new AriaDbConnection("Aria", ""), ((AriaObjectProperty)result[index]).ObjectID, clientId).ObjectName;
                    pathObjectName = pathObjectName.Substring(objectName.TrimEnd().Length).TrimEnd();

                    if (pathObjectName.StartsWith("."))
                    {
                        pathObjectName = pathObjectName.Substring(1);
                    }

                    if (pathObjectName.TrimEnd().Length > 0)
                    {
                        pathObjectName += ".";
                    }

                    dataPaths.Add(pathObjectName + ((AriaObjectProperty)result[index]).PropertyName.TrimEnd());
                }
                else if (((AriaObjectProperty)result[index]).PropertyType == AriaDataTypes.AriaRelatedField)
                {
                    string pathObjectName = dictionary.LoadAriaObjectById(new AriaDbConnection("Aria", ""), ((AriaObjectProperty)result[index]).ObjectID, clientId).ObjectName;
                    pathObjectName = pathObjectName.Substring(objectName.TrimEnd().Length).TrimEnd();

                    if (pathObjectName.StartsWith("."))
                    {
                        pathObjectName = pathObjectName.Substring(1);
                    }

                    if (pathObjectName.TrimEnd().Length > 0)
                    {
                        pathObjectName += ".";
                    }

                    dataPaths.Add(pathObjectName + ((AriaObjectProperty)result[index]).PropertyName.TrimEnd());
                }
            }

            return dataPaths;
        }

        public override object InitializeLifetimeService()
        {
            return null;
        }
    }
}
