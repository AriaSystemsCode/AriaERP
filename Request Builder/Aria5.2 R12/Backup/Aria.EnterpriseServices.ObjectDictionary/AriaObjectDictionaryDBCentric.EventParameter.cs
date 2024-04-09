using System;
using Aria.DataTypes;
using Aria.DataTypes.ObjectDictionary;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Data;
using Aria.DataTypes.Settings;
using System.Collections;
using Aria.Environment;
using Aria.Xml;


namespace Aria.EnterpriseServices.ObjectDictionary
{
    public partial class AriaObjectDictionaryDBCentric
    {
        #region Loading

        public ArrayList LoadAriaObjectEventParametersByID(AriaDbConnection connection, int objectID, string objectRevision, string eventName,string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectEventParameter WHERE ObjectID = @objectID AND ObjectRevision = @objectRevision "
                                    + " EventName = @eventName ORDER BY ParameterNo";


            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectID", objectID));
            command.Parameters.Add(new AriaDbParameter("objectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("eventName", eventName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return dataProvider.GetObjectList(command, typeof(AriaObjectEventParameter));

        }

        public ArrayList LoadAriaObjectEventParameters(AriaDbConnection connection, string objectName, string objectRevision, string eventName,string clientId)
        {
            string AriaDbCommandText = "SELECT AriaObjectEventParameter.* FROM AriaObjectEventParameter LEFT JOIN AriaObject ON " +
                                        "(AriaObjectEventParameter.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE AriaObject.ObjectName = @ObjectName AND " +
                                                "AriaObjectEventParameter.ObjectRevision = @ObjectRevision AND " +
                                                    "AriaObjectEventParameter.EventName = @EventName " +
                                                        "ORDER BY AriaObjectEventParameter.ParameterNo";

            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("EventName", eventName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return dataProvider.GetObjectList(command, typeof(AriaObjectEventParameter));
        }

        public AriaObjectEventParameter LoadAriaObjectEventParameterByID(AriaDbConnection connection, int objectID, string eventName, string objectRevision, string ParameterName,string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectEventParameter WHERE ObjectID = @objectID AND "
                                    + " ObjectRevision = @objectRevision AND EventName = @eventName AND "
                                    + " ParameterName = @parameterName ORDER BY ParameterNo";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectID", objectID));
            command.Parameters.Add(new AriaDbParameter("objectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("eventName", eventName));
            command.Parameters.Add(new AriaDbParameter("parameterName", ParameterName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return (AriaObjectEventParameter)dataProvider.GetObjectList(command, typeof(AriaObjectEventParameter))[0];

        }

        public AriaObjectEventParameter LoadAriaObjectEventParameter(AriaDbConnection connection, string objectName, string eventName, string objectRevision, string ParameterName,string clientId)
        {
            string AriaDbCommandText = "SELECT AriaObjectEventParameter.* FROM AriaObjectEventParameter LEFT JOIN AriaObject ON " +
                                        "(AriaObjectEventParameter.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE AriaObject.ObjectName = @ObjectName AND " +
                                                "AriaObjectEventParameter.ObjectRevision = @ObjectRevision AND " +
                                                    "AriaObjectEventParameter.EventName = @EventName AND " +
                                                        "AriaObjectEventParameter.ParameterName = @ParameterName " +
                                                            "ORDER BY AriaObjectEventParameter.ParameterNo";

            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("EventName", eventName));
            command.Parameters.Add(new AriaDbParameter("ParameterName", ParameterName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return (AriaObjectEventParameter)dataProvider.GetObjectList(command, typeof(AriaObjectEventParameter))[0];
        }

        #endregion Loading

        #region Saving

        public void SaveAriaObjectEventParameter(AriaDbConnection connection, int objectID, string objectRevision, string eventName, int parameterNo, 
                                                    string parameterName, AriaDataTypes parameterType, string parameterSettingsXml,string clientId)
        {
            AriaObjectEventParameter eventParameter = new AriaObjectEventParameter();
            eventParameter.ObjectID = objectID;
            eventParameter.ObjectRevision = objectRevision;
            eventParameter.EventName = eventName;
            eventParameter.ParameterNo = parameterNo;
            eventParameter.ParameterName = parameterName;
            eventParameter.ParameterType  = parameterType;

            AriaXmlSerializer xml = new AriaXmlSerializer();
            eventParameter.ParameterSettings = (AriaDataTypeSettings)xml.ConvertFromXml(parameterSettingsXml);

            AriaDataProvider dataProvider = new AriaDataProvider();
            dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50SystemFiles, eventParameter, null, clientId);
        }

        #endregion Saving
    }
}
