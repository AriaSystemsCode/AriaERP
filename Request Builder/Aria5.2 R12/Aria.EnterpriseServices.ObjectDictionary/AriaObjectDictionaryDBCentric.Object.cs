using System;
using System.Collections;
using Aria.DataTypes;
using Aria.DataTypes.ObjectDictionary;
using Aria.Environment;
using Aria.Data;
using System.Data;
using System.IO;


namespace Aria.EnterpriseServices.ObjectDictionary
{
    public partial class AriaObjectDictionaryDBCentric
    {
        #region Loading

        public ArrayList LoadAriaObjects(AriaDbConnection connection,string  clientId)
        {
            string AriaDbCommandText = "SELECT * FROM AriaObject ORDER BY ObjectName";

            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);

            AriaDataProvider dataProvider = new AriaDataProvider();

            return dataProvider.GetObjectList(command, typeof(Aria.DataTypes.ObjectDictionary.AriaObject));
        }

        public ArrayList LoadAriaObjectsOfType(AriaDbConnection connection, AriaObjectTypes objectType,string clientId)
        {
            string AriaDbCommandText = "SELECT * FROM AriaObject WHERE ObjectType = @ObjectType ORDER BY ObjectName";

            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectType", objectType.ToString()));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return dataProvider.GetObjectList(command, typeof(Aria.DataTypes.ObjectDictionary.AriaObject));
        }

        public ArrayList LoadAriaObjectChildObjects(AriaDbConnection connection, string objectName,string clientId)
        {
            objectName += ".";
            string AriaDbCommandText = "SELECT * FROM AriaObject WHERE " +
                                        "SUBSTRING(ObjectName, 1, " + objectName.Length + ") = @ObjectName";

            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectName", objectName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return dataProvider.GetObjectList(command, typeof(Aria.DataTypes.ObjectDictionary.AriaObject));
        }

        public ArrayList LoadAriaObjectChildObjectsOfType(AriaDbConnection connection, string objectName, AriaObjectTypes objectType,string clientId)
        {
             
            objectName += ".";
            string AriaDbCommandText = "SELECT * FROM AriaObject WHERE " +
                                        "SUBSTRING(ObjectName, 1, " + objectName.Length + ") = @ObjectName AND " +
                                            "ObjectType = @ObjectType ORDER BY ObjectName";

            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectType", objectType.ToString()));
            
            AriaDataProvider dataProvider = new AriaDataProvider();
            
            return dataProvider.GetObjectList(command, typeof(Aria.DataTypes.ObjectDictionary.AriaObject));
        }

        public Aria.DataTypes.ObjectDictionary.AriaObject LoadAriaObjectById(AriaDbConnection connection, int objectId,string clientId)
        {
            string AriaDbCommandText = "SELECT * FROM AriaObject WHERE ObjectID = @ObjectID";

            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectID", objectId));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return (Aria.DataTypes.ObjectDictionary.AriaObject)dataProvider.GetObjectList(command, typeof(Aria.DataTypes.ObjectDictionary.AriaObject))[0];
        }

        public Aria.DataTypes.ObjectDictionary.AriaObject LoadAriaObjectByName(AriaDbConnection connection, string objectName,string clientId)
        {
            string AriaDbCommandText = "SELECT * FROM AriaObject WHERE ObjectName = @ObjectName";

            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return (Aria.DataTypes.ObjectDictionary.AriaObject)dataProvider.GetObjectList(command, typeof(AriaObject))[0];
        }

        #endregion Loading

        #region Saving

        public void SaveAriaObject(AriaDbConnection connection, string objectName, AriaObjectTypes objectType, int parentObjectID, string activeRevision,string clientId, string ObjectDescription)
        {
            AriaObject ariaObject = new AriaObject();
            ariaObject.ObjectType = objectType;
            ariaObject.ObjectName = objectName;
            ariaObject.ParentObjectID = parentObjectID;
            ariaObject.ActiveRevision = activeRevision;
            ariaObject.ObjectDescription = ObjectDescription;
            

            AriaDataProvider dataProvider = new AriaDataProvider();
            dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50SystemFiles, ariaObject, new string[] { "ObjectID" }, clientId);
        }

        public void SaveAriaObject(AriaDbConnection connection, string objectName, AriaObjectTypes objectType, string clientId, string ObjectDescription)
        {
            // Get New ID
            AriaDbCommand dbCommand = new AriaDbCommand("SELECT MAX(ObjectID) FROM AriaObject", new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50SystemFiles, clientId);
            AriaDataProvider dataProvider = new AriaDataProvider();
            System.Data.DataTable result = dataProvider.GetDataTable(dbCommand);

            AriaObject ariaObject = new AriaObject();
            ariaObject.ObjectID = Convert.ToInt32(result.Rows[0][0]) + 1;
            ariaObject.ObjectType = objectType;
            ariaObject.ObjectName = objectName;
            ariaObject.ObjectDescription = ObjectDescription;

            dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50SystemFiles, ariaObject, null, clientId);
        }

        #endregion Saving
    }
}
