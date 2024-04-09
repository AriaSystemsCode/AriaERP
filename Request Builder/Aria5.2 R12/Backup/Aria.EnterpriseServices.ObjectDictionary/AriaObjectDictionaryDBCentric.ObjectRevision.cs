using System;
using Aria.DataTypes;
using Aria.DataTypes.ObjectDictionary;
using System.Reflection;
using System.Collections;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Environment;
using Aria.Data;
using Aria.Xml;
using Aria.DataTypes.Settings;

namespace Aria.EnterpriseServices.ObjectDictionary
{
    public partial class AriaObjectDictionaryDBCentric
    {
        #region Loading

        public ArrayList LoadAriaObjectRevisionsByID(AriaDbConnection connection, int objectID,string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectRevision WHERE ObjectID = @objectID ORDER BY ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectID",objectID));

            AriaDataProvider dataProvider = new AriaDataProvider();

            return dataProvider.GetObjectList(command, typeof(AriaObjectRevision));
        }

        public ArrayList LoadAriaObjectRevisions(AriaDbConnection connection, string objectName, string clientId)
        {
            string commandText = "SELECT AriaObjectRevision.* FROM AriaObjectRevision LEFT JOIN AriaObject ON " +
                                        "(AriaObjectRevision.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE AriaObject.ObjectName = @ObjectName " +
                                                    "ORDER BY AriaObjectRevision.ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return dataProvider.GetObjectList(command, typeof(AriaObjectRevision));
        }

        public ArrayList GetAriaObjectKey(AriaDbConnection connection, string objectName, string clientId)
        {
            ArrayList result = new ArrayList();

            string actRev = LoadActiveRevision(connection, objectName, clientId).ObjectRevision;

            ArrayList properties = LoadAriaObjectProperties(connection, objectName, actRev, false, clientId);


            for (int i = 0; i < properties.Count; i++)
            {
                AriaFieldSettings setting = (AriaFieldSettings)((AriaObjectProperty)properties[i]).PropertySettings;
                if (setting.IsPrimaryKey)
                {
                    result.Add(setting.FieldName);
                }
            }


            return result;
        }

        public AriaObjectRevision LoadAriaObjectRevisionByID(AriaDbConnection connection, int objectID, string objectRevision, string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectRevision WHERE ObjectID = @objectID AND ObjectRevision = @objectRevision"
                                    + " ORDER BY ObjectRevision ";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectID",objectID));
            command.Parameters.Add(new AriaDbParameter("objectRevision",objectRevision));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return (AriaObjectRevision)dataProvider.GetObjectList(command, typeof(AriaObjectRevision))[0];
        }

        public AriaObjectRevision LoadAriaObjectRevision(AriaDbConnection connection, string objectName, string objectRevision,string clientId)
        {
            string commandText = "SELECT AriaObjectRevision.* FROM AriaObjectRevision LEFT JOIN AriaObject ON " +
                                        "(AriaObjectRevision.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE AriaObject.ObjectName = @ObjectName AND " +
                                                "ObjectRevision = @ObjectRevision " +
                                                    "ORDER BY AriaObjectRevision.ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return (AriaObjectRevision)dataProvider.GetObjectList(command, typeof(AriaObjectRevision))[0];
        }

        public AriaObjectRevision LoadActiveRevisionByID(AriaDbConnection connecction, int objectID,string clientId)
        {
            string commandText = "SELECT AriaObjectRevision.* FROM AriaObjectRevision LEFT JOIN AriaObject ON (AriaObjectRevision.ObjectID = AriaObject.ObjectID"
                                 + " WHERE AriaObjectRevision.ObjectID = @objectID AND AriaObjectRevision.ObjectRevision = AriaObject.ActiveRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connecction, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectID", objectID));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return (AriaObjectRevision)dataProvider.GetObjectList(command, typeof(AriaObjectRevision))[0];
        }

        public AriaObjectRevision LoadActiveRevision(AriaDbConnection connection, string objectName,string clientId)
        {
            string commandText = "SELECT AriaObjectRevision.* FROM AriaObjectRevision LEFT JOIN AriaObject ON " +
                                 "(AriaObjectRevision.ObjectID = AriaObject.ObjectID) " +
                                 "WHERE AriaObject.ObjectName = @ObjectName " +
                                 " AND AriaObject.ActiveRevision = AriaObjectRevision.ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return (AriaObjectRevision)dataProvider.GetObjectList(command, typeof(AriaObjectRevision))[0];
        }

        #endregion Loading

        #region Saving

        public void SaveAriaObjectRevision(AriaDbConnection connection, int objectID, string objectRevision, string revisionSettingsXml,string clientId)
        {
            AriaObjectRevision Revision = new AriaObjectRevision();
            Revision.ObjectID = objectID;
            Revision.ObjectRevision = objectRevision;
            AriaXmlSerializer xml = new AriaXmlSerializer();
            Revision.ObjectRevisionSettings = (AriaObjectRevisionSettings)xml.ConvertFromXml(revisionSettingsXml);

            AriaDataProvider dataProvider = new AriaDataProvider();
            dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50SystemFiles, Revision, null, clientId);
        }

        #endregion Saving
    }
}
