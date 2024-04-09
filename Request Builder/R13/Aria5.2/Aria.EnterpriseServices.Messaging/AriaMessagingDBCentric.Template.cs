using System.Collections;
using Aria.Environment;
using Aria.Data;
using System;


namespace Aria.EnterpriseServices.Messaging
{
    /// <summary>
    /// This class used to load template from database where store in Aria.Master database.
    /// </summary>
    public partial class AriaMessagingDBCentric : MarshalByRefObject
    {
        #region Loading

        public ArrayList LoadAriaObjectsMessageTemplates(AriaDbConnection connection, string objectName,string clientId)
        {
            string commandText = "SELECT * FROM AriaMessageTemplate WHERE ObjectName = @ObjectName";

            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);

            command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));

            AriaDataProvider dataProvider = new AriaDataProvider();

            return dataProvider.GetObjectList(command, typeof(Aria.DataTypes.Messaging.AriaMessageTemplate));
        }

        #endregion Loading

        public override object InitializeLifetimeService()
        {
            return null;
        }
    }
}
