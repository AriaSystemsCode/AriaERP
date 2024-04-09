using System.Collections;
using Aria.Environment;
using System.EnterpriseServices;
using Aria.Data;


namespace Aria.EnterpriseServices.Messaging
{
    public class AriaMessagingDBCentric : ServicedComponent 
    {
        #region Loading

        //public ArrayList LoadAriaObjectsMessageTemplates(string objectName)
        //{
        //    string commandText = "SELECT * FROM MessageTemplate WHERE ObjectName = @ObjectName";

        //    AriaDbConnection connection = new AriaDbConnection(AriaDbTypes.Aria50SystemFiles, "", "");

        //    AriaDbCommand command = new AriaDbCommand(commandText, connection);
            
        //    command.Parameters.Add(new AriaDbParameter("ObjectName", objectName));

        //    AriaDataProvider dataProvider = new AriaDataProvider();
            
        //    return dataProvider.GetObjectList(command, typeof(Aria.DataTypes.Messaging.AriaMessageTemplate));
        //}

        #endregion Loading
    }
}
