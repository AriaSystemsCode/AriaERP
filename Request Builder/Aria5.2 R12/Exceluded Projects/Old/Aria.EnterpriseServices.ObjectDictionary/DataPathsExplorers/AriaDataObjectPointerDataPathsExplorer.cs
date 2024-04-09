using System;
using System.Collections.Generic;
using System.Text;
using Aria.DataTypes.Settings;
using Aria.DataTypes;
using Aria.DataTypes.ObjectDictionary;
using System.Collections;
using Aria.DataTypes.ObjectDictionary.Settings.Object;

namespace Aria.EnterpriseServices.ObjectDictionary.DataPathsExplorers
{
    public class AriaDataObjectPointerDataPathsExplorer : AriaDataPathsExplorer
    {
        public override AriaDataPath GetDataPathsTree(AriaDataTypeSettings settings)
        {
            return GetDataPathsTree((AriaDataObjectPointerSettings)settings);
        }

        public AriaDataPath GetDataPathsTree(string objectName, string eventName,string clientId)
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaObjectRevision objectRevision = objectDictionary.LoadActiveRevision(new Aria.Data.AriaDbConnection("Aria", ""), objectName, clientId);
            AriaObjectEvent objectEvent = objectDictionary.LoadAriaObjectEvent(new Aria.Data.AriaDbConnection("Aria", ""), objectName, objectRevision.ObjectRevision, eventName, clientId);
            AriaObjectEventParameter eventParameter = objectDictionary.LoadAriaObjectEventParameter(new Aria.Data.AriaDbConnection("Aria", ""), objectName, eventName, objectRevision.ObjectRevision, objectEvent.BusinessObjectParameterName, clientId);

            return GetDataPathsTree(eventParameter.ParameterSettings);
        }

        public AriaDataPath GetDataPathsTree(AriaDataObjectPointerSettings settings,string clientId)
        {
            AriaDataPath rootDataPath = new AriaDataPath();
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            rootDataPath.DataPath = settings.DataObjectName.Split('.')[settings.DataObjectName.Split('.').Length - 1];
            rootDataPath.DataType = AriaDataTypes.AriaDataObjectPointer;
            rootDataPath.Settings = new AriaDictionaryDefinedObjectSettings();
            ((AriaDictionaryDefinedObjectSettings)rootDataPath.Settings).ObjectName = settings.DataObjectName;
            ((AriaDictionaryDefinedObjectSettings)rootDataPath.Settings).ObjectRevision = objectDictionary.LoadActiveRevision(new Aria.Data.AriaDbConnection("Aria", ""), settings.DataObjectName, clientId).ObjectRevision;

            ArrayList properties = objectDictionary.LoadAriaObjectProperties(new Aria.Data.AriaDbConnection("Aria", ""), settings.DataObjectName,
                                        objectDictionary.LoadActiveRevision(new Aria.Data.AriaDbConnection("Aria", ""), settings.DataObjectName, clientId).ObjectRevision, false, clientId);

            for (int propertyIndex = 0; propertyIndex < properties.Count; propertyIndex++)
            {
                AriaDataPath dataPath = new AriaDataPath();
                AriaObjectProperty property = (AriaObjectProperty)properties[propertyIndex];

                dataPath.DataPath = property.PropertyName;
                dataPath.DataType = property.PropertyType;
                dataPath.Settings = property.PropertySettings;

                rootDataPath.ChildDataPaths.Add(dataPath);
            }

            ArrayList relatedDataObjects = objectDictionary.LoadAriaObjectChildObjectsOfType(new Aria.Data.AriaDbConnection("Aria", ""), settings.DataObjectName, AriaObjectTypes.RelatedData, clientId);

            for (int relatedDataObjectIndex = 0; relatedDataObjectIndex < properties.Count; relatedDataObjectIndex++)
            {
                AriaDataPath dataPath = new AriaDataPath();
                AriaObject relatedDataObject = (AriaObject)relatedDataObjects[relatedDataObjectIndex];

                dataPath.DataPath = relatedDataObject.ObjectName;
                dataPath.DataType = AriaDataTypes.AriaDictionaryDefinedObject;
                dataPath.Settings = new AriaDictionaryDefinedObjectSettings();
                ((AriaDictionaryDefinedObjectSettings)dataPath.Settings).ObjectName = relatedDataObject.ObjectName;
                ((AriaDictionaryDefinedObjectSettings)dataPath.Settings).ObjectRevision =
                            objectDictionary.LoadActiveRevision(new Aria.Data.AriaDbConnection("Aria", ""), relatedDataObject.ObjectName, clientId).ObjectRevision;

                LoadRelatedDataObjectPathsTree(dataPath, clientId);

                rootDataPath.ChildDataPaths.Add(dataPath);
            }


            return rootDataPath;
            return null;
        }

        private void LoadRelatedDataObjectPathsTree(AriaDataPath relatedObjectDataPath,string clientId)
        {
            AriaDictionaryDefinedObjectSettings settings = (AriaDictionaryDefinedObjectSettings)relatedObjectDataPath.Settings;
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            ArrayList properties = objectDictionary.LoadAriaObjectProperties(new Aria.Data.AriaDbConnection("Aria", ""), settings.ObjectName,
                                        objectDictionary.LoadActiveRevision(new Aria.Data.AriaDbConnection("Aria", ""), settings.ObjectName, clientId).ObjectRevision, false, clientId);

            for (int propertyIndex = 0; propertyIndex < properties.Count; propertyIndex++)
            {
                AriaDataPath dataPath = new AriaDataPath();
                AriaObjectProperty property = (AriaObjectProperty)properties[propertyIndex];

                dataPath.DataPath = property.PropertyName;
                dataPath.DataType = property.PropertyType;
                dataPath.Settings = property.PropertySettings;

                relatedObjectDataPath.ChildDataPaths.Add(dataPath);
            }

            ArrayList relatedDataObjects = objectDictionary.LoadAriaObjectChildObjectsOfType(new Aria.Data.AriaDbConnection("Aria", ""), settings.ObjectName, AriaObjectTypes.RelatedData, clientId);

            for (int relatedDataObjectIndex = 0; relatedDataObjectIndex < properties.Count; relatedDataObjectIndex++)
            {
                AriaDataPath dataPath = new AriaDataPath();
                AriaObject relatedDataObject = (AriaObject)relatedDataObjects[relatedDataObjectIndex];

                dataPath.DataPath = relatedDataObject.ObjectName;
                dataPath.DataType = AriaDataTypes.AriaDictionaryDefinedObject;
                dataPath.Settings = new AriaDictionaryDefinedObjectSettings();
                ((AriaDictionaryDefinedObjectSettings)dataPath.Settings).ObjectName = relatedDataObject.ObjectName;
                ((AriaDictionaryDefinedObjectSettings)dataPath.Settings).ObjectRevision =
                                    objectDictionary.LoadActiveRevision(new Aria.Data.AriaDbConnection("Aria", ""), relatedDataObject.ObjectName, clientId).ObjectRevision;

                LoadRelatedDataObjectPathsTree(dataPath, clientId);

                relatedObjectDataPath.ChildDataPaths.Add(dataPath);
            }
        }
    }
}
