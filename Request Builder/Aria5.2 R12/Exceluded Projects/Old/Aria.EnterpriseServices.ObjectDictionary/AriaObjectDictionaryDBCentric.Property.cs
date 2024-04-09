using System;
using Aria.DataTypes;
using Aria.DataTypes.ObjectDictionary;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using System.Collections;
using Aria.Data;
using Aria.DataTypes.Settings;
using Aria.Environment;
using Aria.Data;
using Aria.Xml;
using Aria.Reflection;

namespace Aria.EnterpriseServices.ObjectDictionary
{
    public partial class AriaObjectDictionaryDBCentric
    {
        #region Loading

        public ArrayList LoadAriaObjectPropertiesByID(AriaDbConnection connection, int objectID, string objectRevision, bool differenceOnly, string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectProperty WHERE ObjectID = @objectID AND ObjectRevision " +
                                    (differenceOnly ? "=" : "<=") + " @ObjectRevision ORDER BY PropertyName, ObjectRevision";
            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectID", objectID));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));

            AriaDataProvider dataProvider = new AriaDataProvider();
            ArrayList Properties = dataProvider.GetObjectList(command, typeof(AriaObjectProperty));

            if (!differenceOnly)
            {
                MergeModification(Properties, "PropertyName", "ModificationType");
            }

            return Properties;
        }

        public ArrayList LoadAriaObjectProperties(AriaDbConnection connection, string objectName, string objectRevision, bool differenceOnly, string clientId)
        {
            string commandText = "SELECT AriaObjectProperty.* FROM AriaObjectProperty LEFT JOIN AriaObject ON " +
                                        "(AriaObjectProperty.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE AriaObject.ObjectName = @ObjectName AND " +
                                                "ObjectRevision " + (differenceOnly ? "=" : "<=") + " @ObjectRevision " +
                                                    "ORDER BY AriaObjectProperty.PropertyName, AriaObjectProperty.ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));

            AriaDataProvider dataProvider = new AriaDataProvider();
            ArrayList properties = dataProvider.GetObjectList(command, typeof(AriaObjectProperty));

            if (!differenceOnly)
            {
                MergeModification(properties, "PropertyName", "ModificationType");
            }

            return properties;
        }

        public ArrayList LoadAriaObjectRelatedPropertiesByID(AriaDbConnection connection, int objectID, string objectRevision, bool differenceOnly, string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectProperty WHERE ObjectID = @objectID AND "
                                    + "ObjectRevision " + (differenceOnly ? "=" : "<=") + " @ObjectRevision "
                                      + " ORDER BY PropertyName, ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectID", objectID));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));

            AriaDataProvider dataProvider = new AriaDataProvider();
            ArrayList properties = dataProvider.GetObjectList(command, typeof(AriaObjectProperty));

            ArrayList targetProperties = new ArrayList();

            for (int index = 0; index < properties.Count; index++)
            {
                AriaObjectProperty property = (AriaObjectProperty)properties[index];
                //if (((AriaRelatedFieldSettings)property.PropertySettings).Message.Trim() == "InHeader")
                {
                    targetProperties.Add(properties[index]);
                }
            }

            MergeModification(targetProperties, "PropertyName", "ModificationType");

            return targetProperties;
        }

        public ArrayList LoadAriaObjectRelatedProperties(AriaDbConnection connection, string objectName, string objectRevision, bool differenceOnly, string clientId)
        {
            string commandText = "SELECT AriaObjectProperty.* FROM AriaObjectProperty LEFT JOIN AriaObject ON " +
                                        "(AriaObjectProperty.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE SUBSTRING(ObjectName, 1, " + objectName.Length + ") = @ObjectName AND " +
                                                "ObjectName <> @ObjectName AND " +
                                                    "ObjectRevision " + (differenceOnly ? "=" : "<=") + " @ObjectRevision " +
                                                        "ORDER BY AriaObjectProperty.PropertyName, AriaObjectProperty.ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));

            AriaDataProvider dataProvider = new AriaDataProvider();
            ArrayList properties = dataProvider.GetObjectList(command, typeof(AriaObjectProperty));

            ArrayList targetProperties = new ArrayList();

            for (int index = 0; index < properties.Count; index++)
            {
                AriaObjectProperty property = (AriaObjectProperty)properties[index];
                //if (((AriaRelatedFieldSettings)property.PropertySettings).Message.Trim() == "InHeader")
                {
                    targetProperties.Add(properties[index]);
                }
            }

            MergeModification(targetProperties, "PropertyName", "ModificationType");

            return targetProperties;
        }

        public AriaObjectProperty LoadAriaObjectPropertyByID(AriaDbConnection connection, int objectID, string objectRevision, string propertyName, string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectProperty WHERE ObjectID = @objectID AND "
                                   + "ObjectRevision <= @objectRevision AND PropertyName = @propertyName"
                                    + " ORDER BY ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectID", objectID));
            command.Parameters.Add(new AriaDbParameter("objecRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("propertyName", propertyName));

            AriaDataProvider dataProvider = new AriaDataProvider();

            ArrayList properties = dataProvider.GetObjectList(command, typeof(AriaObjectProperty));


            MergeModification(properties, "PropertyName", "ModificationType");

            return (AriaObjectProperty)properties[0];
        }

        public AriaObjectProperty LoadAriaObjectProperty(AriaDbConnection connection, string objectName, string objectRevision, string propertyName, string clientId)
        {
            string commandText = "SELECT AriaObjectProperty.* FROM AriaObjectProperty LEFT JOIN AriaObject ON " +
                                        "(AriaObjectProperty.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE AriaObject.ObjectName = @ObjectName AND " +
                                                "ObjectRevision <= @ObjectRevision AND " +
                                                    "PropertyName = @PropertyName " +
                                                           "ORDER BY AriaObjectProperty.ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("PropertyName", propertyName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            ArrayList properties = dataProvider.GetObjectList(command, typeof(AriaObjectProperty));

            MergeModification(properties, "PropertyName", "ModificationType");

            return (AriaObjectProperty)properties[0];
        }

        #endregion Loading

        #region Saving

        public void SaveAriaObjectProperty(AriaDbConnection connection, int objectID, string objectRevision, string propertyName,
                                            AriaModificationTypes modificationType, AriaDataTypes propertyType, string propertySettingsXml, string clientId, string PropertyDescription)
        {
            AriaObjectProperty property = new AriaObjectProperty();
            property.ObjectID = objectID;
            property.ObjectRevision = objectRevision;
            property.PropertyName = propertyName;
            property.PropertyType = propertyType;
            property.PropertyDescription = PropertyDescription;

            AriaXmlSerializer xml = new AriaXmlSerializer();
            property.PropertySettings = (AriaDataTypeSettings)xml.ConvertFromXml(propertySettingsXml);

            AriaDataProvider dataProvider = new AriaDataProvider();

            dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50SystemFiles, property, null, clientId);
        }

        #endregion Saving
    }
}
