using System;
using Aria.DataTypes;
using Aria.DataTypes.ObjectDictionary;
using System.Reflection;
using System.Collections;
using Aria.Environment;
using Aria.Data;
using Aria.Xml;

namespace Aria.EnterpriseServices.ObjectDictionary
{
    public partial class AriaObjectDictionaryDBCentric
    {
        #region Loading

        public ArrayList LoadAriaObjectEventsByID(AriaDbConnection connection, int objectID, string objectRevision, bool differenceOnly,string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectEvent WHERE ObjectID = @objectID AND ObjectRevision = @objectRevision " +
                                    (differenceOnly ? "=" : "<=") + " @ObjectRevision " +
                                        " ORDER BY EventName,ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectID", objectID));
            command.Parameters.Add(new AriaDbParameter("objectRevision", objectRevision));

            AriaDataProvider dataProvider = new AriaDataProvider();
            ArrayList Events = dataProvider.GetObjectList(command, typeof(AriaObjectEvent));

            if (!differenceOnly)
            {
                MergeModification(Events, "EventName", "ModificationType");
            }

            return Events;

        }

        public ArrayList LoadAriaObjectEvents(AriaDbConnection connection, string objectName, string objectRevision, bool differenceOnly,string clientId)
        {
            string AriaDbCommandText = "SELECT AriaObjectEvent.* FROM AriaObjectEvent LEFT JOIN AriaObject ON " +
                                        "(AriaObjectEvent.ObjectID = AriaObject.ObjectID) " +
                                            "WHERE AriaObject.ObjectName = @ObjectName AND " +
                                                "ObjectRevision " + (differenceOnly ? "=" : "<=") + " @ObjectRevision " +
                                                    "ORDER BY AriaObjectEvent.EventName, AriaObjectEvent.ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));

            AriaDataProvider dataProvider = new AriaDataProvider();
            ArrayList Events = dataProvider.GetObjectList(command, typeof(AriaObjectEvent));

            if (!differenceOnly)
            {
                MergeModification(Events, "EventName", "ModificationType");
            }

            return Events;
        }

        public AriaObjectEvent LoadAriaObjectEventByID(AriaDbConnection connection, int objectID, string objectRevision, string eventName,string clientId)
        {
            string commandText = "SELECT * FROM AriaObjectEvent WHERE ObjectID = @objectID AND ObjectRevision <= @objectRevision"
                                    + " AND EventName = @eventName ORDER BY ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("objectID", objectID));
            command.Parameters.Add(new AriaDbParameter("objectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("eventName", eventName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            ArrayList Events = dataProvider.GetObjectList(command, typeof(AriaObjectEvent));

            MergeModification(Events, "EventName", "ModificationType");

            return (AriaObjectEvent)Events[0];
        }

        public AriaObjectEvent LoadAriaObjectEvent(AriaDbConnection connection, string objectName, string objectRevision, string eventName,string clientId)
        {
            string AriaDbCommandText = "SELECT AriaObjectEvent.* FROM AriaObjectEvent LEFT JOIN AriaObject ON " +
                                        "(AriaObject.ObjectName = @ObjectName) WHERE " +
                                            "ObjectRevision <= @ObjectRevision AND " +
                                                "EventName = @EventName " +
                                                    "ORDER BY AriaObjectEvent.ObjectRevision";

            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50SystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));
            command.Parameters.Add(new AriaDbParameter("ObjectRevision", objectRevision));
            command.Parameters.Add(new AriaDbParameter("EventName", eventName));

            AriaDataProvider dataProvider = new AriaDataProvider();
            ArrayList Events = dataProvider.GetObjectList(command, typeof(AriaObjectEvent));

            MergeModification(Events, "EventName", "ModificationType");

            return (AriaObjectEvent)Events[0];
        }

        #endregion Loading

        #region Saving

        public void SaveAriaObjectEvent(AriaDbConnection connection, int objectID, string objectRevision, string eventName, AriaModificationTypes modificationType, string clientId, string EventDescription)
        {
            AriaObjectEvent Event = new AriaObjectEvent();
            Event.ObjectID = objectID;
            Event.ObjectRevision = objectRevision;
            Event.EventName = eventName;
            Event.ModificationType = modificationType;
            Event.EventDescription = EventDescription;


            AriaDataProvider dataProvider = new AriaDataProvider();
            dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50SystemFiles, Event, null, clientId);
        }

        #endregion Saving
    }
}
