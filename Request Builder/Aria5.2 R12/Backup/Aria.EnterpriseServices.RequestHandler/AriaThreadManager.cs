using System;
using System.Collections.Generic;
using System.Text;
using Aria.DataTypes.RequestHandler;
using Aria.Data;
using Aria.EnterpriseServices.Messaging;
using Aria.Environment;
using System.Collections;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.DataTypes;
using Aria.DataTypes.Settings;
using Aria.DataTypes.ObjectDictionary;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Reflection;
using System.Threading;
using Aria.Utilities.Log;

namespace Aria.EnterpriseServices.RequestHandler
{
    /// <summary>
    /// This class responsible on run request depends on Threading (Multi-Threading is the ability of a CPU to execute several threads of execution apparently at the same time).
    /// </summary>
    public class AriaThreadManager
    {
        Hashtable _threadHandle = new Hashtable();

        public AriaRequest GetRequest(string guid, string clientId)
        {
            string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);

            command.Parameters.Add(new AriaDbParameter("RequestID", new Guid(guid)));

            AriaDataProvider dataProvider = new AriaDataProvider();

            if (dataProvider.GetObjectList(command, typeof(AriaRequest)).Count == 0)
            {
                return null;
            }
            else
            {
                return (AriaRequest)dataProvider.GetObjectList(command, typeof(AriaRequest))[0];
            }
        }

        public void CancelRequest(string guid, string clientId)
        {
            AriaRequest request = GetRequest(guid,  clientId);
            request.Status = AriaRequestStatusTypes.Canceled;
            AriaDataProvider dataProvider = new AriaDataProvider();

            dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request, new string[] { "Status" }, "RequestID = @RequestID", clientId);

            //MAH Log
            AriaDatabaseLogManager.AriaAddToLog(clientId, new AriaDbConnection("Aria", ""), new Guid(guid), "Cancel Request");
            //MAH Log


        }

        public void StartThread(object requestObject,string  clientId)
        {
            AriaRequest request = (AriaRequest)requestObject;
            Thread thread = new Thread(new ParameterizedThreadStart(Run));
            _threadHandle[request.RequestID.ToString()] = thread;
            thread.Start(requestObject);
        }

        private void Run(object Data)
        {
         
            AriaRequestAgent agent = new AriaRequestAgent();
            agent.InvokeRequestMethod(Data, ((AriaRequest)Data).clientID);
        }

        public bool StopThread(string GUID, string clientId)
        {
            CancelRequest(GUID,clientId);

            Thread thread = (Thread)_threadHandle[GUID];

            if (thread == null)
            {
                return false;
            }
            thread.Abort();

            bool result = thread.Join(60000);

            if (result) _threadHandle.Remove(GUID);

            return result;
        }
    }
}
