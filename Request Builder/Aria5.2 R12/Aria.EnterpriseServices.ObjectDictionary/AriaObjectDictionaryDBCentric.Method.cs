using System;
using Aria.DataTypes;
using Aria.DataTypes.ObjectDictionary;
using System.Reflection;
using System.Collections;
using Aria.Environment;
using Aria.Data;

namespace Aria.EnterpriseServices.ObjectDictionary
{
    public partial class AriaObjectDictionaryDBCentric
    {
        #region Loading

        public ArrayList LoadAriaObjectMethodsByID(AriaDbConnection connection, int objectID, string objectRevsion, bool differenceOnly,string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectMethod WHERE ObjectID = @objectID AND ObjectRevision " + (differenceOnly ? "=" : "<=") +
                                    "@objectRevision ORDER BY MethodName, ObjectRevision";
            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectID", objectID));
            command.Parameters.Add(new AriaDbParameter("objectRevision", objectRevsion));

            AriaDataProvider dataProvider = new AriaDataProvider();

            ArrayList Methods = dataProvider.GetObjectList(command, typeof(AriaObjectMethod));

            if (!differenceOnly)
            {
                MergeModification(Methods, "MethodName", "ModificationType");
            }

            return Methods;
        }

        public ArrayList LoadAriaObjectMethods(AriaDbConnection connection, string objectName, string objectRevision, bool differenceOnly,string clientId)
        {
            string commandText = "SELECT AriaObjectMethod.* FROM AriaObjectMethod LEFT JOIN AriaObject ON " +
                                        "(AriaObjectMethod.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE AriaObject.ObjectName = @ObjectName AND " +
                                                "ObjectRevision " + (differenceOnly ? "=" : "<=") + " @ObjectRevision " +
                                                    "ORDER BY AriaObjectMethod.MethodName, AriaObjectMethod.ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));

            AriaDataProvider dataProvider = new AriaDataProvider();
            ArrayList Methods = dataProvider.GetObjectList(command, typeof(AriaObjectMethod));

            if (!differenceOnly)
            {
                MergeModification(Methods, "MethodName", "ModificationType");
            }

            return Methods;
        }

        public AriaObjectMethod LoadAriaObjectMethodByID(AriaDbConnection connection, int objectID, string objectRevision, string methodName,string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectMethod WHERE ObjectID = @objectID AND ObjectRevision <= @objectRevision"
                                    + "MethodName = @MethodName ORDER BY ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectID",objectID));
            command.Parameters.Add(new AriaDbParameter("objectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("MethodName", methodName));

            AriaDataProvider dataProvider = new AriaDataProvider();

            ArrayList Methods = dataProvider.GetObjectList(command, typeof(AriaObjectMethod));

            MergeModification(Methods, "MethodName", "ModificationType");

            return (AriaObjectMethod)Methods[0];
        }

        public AriaObjectMethod LoadAriaObjectMethod(AriaDbConnection connection, string objectName, string objectRevision, string methodName,string clientId)
        {
            string commandText = "SELECT AriaObjectMethod.* FROM AriaObjectMethod LEFT JOIN AriaObject ON " +
                                        "(AriaObjectMethod.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE AriaObject.ObjectName = @ObjectName AND " +
                                                "ObjectRevision <= @ObjectRevision AND " +
                                                    "MethodName = @MethodName " +
                                                        "ORDER BY AriaObjectMethod.ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("MethodName", methodName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            ArrayList Methods = dataProvider.GetObjectList(command, typeof(AriaObjectMethod));

            MergeModification(Methods, "MethodName", "ModificationType");

            return (AriaObjectMethod)Methods[0];
        }

        #endregion Loading

        #region Saving

        public void SaveAriaObjectMethod(AriaDbConnection connection, int objectID, string objectRevision, string methodName, AriaModificationTypes modificationType,string clientId)
        {
            AriaObjectMethod Method = new AriaObjectMethod();
            Method.ObjectID = objectID;
            Method.ObjectRevision = objectRevision;
            Method.MethodName = methodName;
            Method.ModificationType = modificationType;

            AriaDataProvider dataProvider = new AriaDataProvider();
            dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50SystemFiles, Method, null, clientId);
        }

        #endregion Saving
    }
}
