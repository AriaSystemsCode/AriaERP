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

        public ArrayList LoadAriaObjectMethodParametersByID(AriaDbConnection connection, int objectID, string objectRevision, string methodName,string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectMethodParameter WHERE ObjectID = @ObjectID AND " +
                                  "ObjectRevision = @ObjectRevision AND MethodName = @MethodName ORDER BY ParameterNo";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectID",objectID));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("MethodName", methodName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return dataProvider.GetObjectList(command, typeof(AriaObjectMethodParameter));

        }
//
        public ArrayList LoadAriaObjectMethodParameters(AriaDbConnection connection, string objectName, string objectRevision, string methodName,string clientId)
        {
            string AriaDbCommandText = "SELECT AriaObjectMethodParameter.* FROM AriaObjectMethodParameter LEFT JOIN AriaObject ON " +
                                        "(AriaObjectMethodParameter.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE AriaObject.ObjectName = @ObjectName AND " +
                                                "AriaObjectMethodParameter.ObjectRevision = @ObjectRevision AND " +
                                                    "AriaObjectMethodParameter.MethodName = @MethodName " +
                                                        "ORDER BY AriaObjectMethodParameter.ParameterNo";

            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("MethodName", methodName));

            AriaDataProvider dataProvider = new AriaDataProvider();

            return dataProvider.GetObjectList( command, typeof(AriaObjectMethodParameter));
        }

        public AriaObjectMethodParameter LoadAriaObjectMethodParameterByID(AriaDbConnection connection, int objectID, string methodName, string objectRevision, string ParameterName,string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectMethodParameter WHERE ObjectID = @ObjectID AND " +
                                  "ObjectRevision = @ObjectRevision AND MethodName = @MethodName AND ParameterName = @ParameterName"
                                   + "ORDER BY ParameterNo";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectID", objectID));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("MethodName", methodName));
            command.Parameters.Add(new AriaDbParameter("ParameterName", ParameterName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return (AriaObjectMethodParameter)(dataProvider.GetObjectList(command, typeof(AriaObjectMethodParameter))[0]);
        }
//
        public AriaObjectMethodParameter LoadAriaObjectMethodParameter(AriaDbConnection connection, string objectName, string methodName, string objectRevision, string ParameterName,string clientId)
        {
            string AriaDbCommandText = "SELECT AriaObjectMethodParameter.* FROM AriaObjectMethodParameter LEFT JOIN AriaObject ON " +
                                        "(AriaObjectMethod.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE AriaObject.ObjectName = @ObjectName AND " +
                                                "AriaObjectMethodParameter.ObjectRevision = @ObjectRevision AND " +
                                                    "AriaObjectMethodParameter.MethodName = @MethodName " +
                                                        "AriaObjectMethodParameter.ParameterName = @ParameterName" +
                                                            "ORDER BY AriaObjectMethod.ParameterNo";

            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("MethodName", methodName));
            command.Parameters.Add(new AriaDbParameter("ParameterName", ParameterName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return (AriaObjectMethodParameter)dataProvider.GetObjectList( command, typeof(AriaObjectMethodParameter))[0];
        }

        #endregion Loading

        #region Saving

        public void SaveAriaObjectMethodParameter(AriaDbConnection connection, int objectID, string objectRevision, string methodName, int parameterNo,
                                                    string parameterName, AriaDataTypes parameterType, string parameterSettingsXml,string clientId)
        {
            AriaObjectMethodParameter methodParameter = new AriaObjectMethodParameter();
            methodParameter.ObjectID = objectID;
            methodParameter.ObjectRevision = objectRevision;
            methodParameter.MethodName = methodName;
            methodParameter.ParameterNo = parameterNo;
            methodParameter.ParameterName = parameterName;
            methodParameter.ParameterType = parameterType;

            AriaXmlSerializer xml = new AriaXmlSerializer();
            methodParameter.ParameterSettings = (AriaDataTypeSettings)xml.ConvertFromXml(parameterSettingsXml);

            AriaDataProvider dataProvider = new AriaDataProvider();
            dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50SystemFiles, methodParameter, null, clientId);
        }

        #endregion Saving
    }
}
