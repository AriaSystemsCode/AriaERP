using System;
using Aria.DataTypes.RequestHandler;
using System.Data.OleDb;
using System.Collections;
using Aria.Data;
using Aria.Environment;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Printing;
using Aria.DataTypes.ObjectDictionary;
using System.Collections.Generic;
using System.Windows.Forms;
using Aria.Utilities.Log;


namespace Aria.EnterpriseServices.RequestHandler.Proxy
{
    /// <summary>
    /// This class contains some method to handle request from load, cancel, add, remove and execute.
    /// </summary>
    public class AriaRequestProxy : MarshalByRefObject
    {
        // T20110803.0001 MAH June 13 2011
        public void GenerateEventRequests(AriaDbConnection callerConnection, string eventObjectName, string eventName, AriaArgumentList eventArguments, string clientId)
        {
            AriaRequestAgent agent = new AriaRequestAgent();

            agent.GenerateEventRequests(callerConnection, eventObjectName, eventName, eventArguments, clientId);
            agent.ExecuteOneTimeOnlyRequests(clientId);

            //try
            //{
            //    AriaEnviromentVariables env = new AriaEnviromentVariables();
            //    env.ClientID = clientId;
            //    env.ConnectionsRefresh();
            //    string url = string.Format("tcp://{0}:{1}/{2}", env.RequestServerName.Trim(), env.RpcPort.ToString().Trim(), typeof(AriaRequestAgent).Name);

            //    AriaRequestAgent remoteAgent = (AriaRequestAgent)Activator.GetObject(typeof(AriaRequestAgent), url);

            //    remoteAgent.ExecuteOneTimeOnlyRequests(clientId);
            //}
            //catch
            //{
            //}

        }
        // T20110803.0001 MAH June 13 2011 End

        public void AddRequest(AriaRequest request, string clientId)
        {
            // T20110803.0001 MAH 9/13/2011
            AriaDataProvider dataProvider = new AriaDataProvider();
            AriaDbConnection connection = new AriaDbConnection("Aria", "");

            if (GetRequest(request.RequestID, clientId) != null)
            {
                List<string> fields = new List<string>();
                for (int propertyIndex = 0; propertyIndex < request.GetType().GetProperties().Length; propertyIndex++)
                {
                    if (request.GetType().GetProperties()[propertyIndex].Name != "RequestNumber" &&
                        request.GetType().GetProperties()[propertyIndex].Name != "clientID")
                    {
                        fields.Add(request.GetType().GetProperties()[propertyIndex].Name);
                    }
                }

                dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request, fields.ToArray(), " RequestNumber = @RequestNumber ", clientId);
            }
            else
            {
                // T20110803.0001 MAH 9/13/2011

                request.Context.RequestId = request.RequestID;
                request.Context.EventName = request.EventName;
                request.Context.MethodName = request.MethodName;
                request.Context.UserName = request.UserName;

                // T20110803.0001 MAH 9/13/2011
                // AriaDataProvider dataProvider = new AriaDataProvider();
                // T20110803.0001 MAH 9/13/2011

                if (request.RecurrenceType == AriaRequestRecurrenceTypes.Daily ||
                        request.RecurrenceType == AriaRequestRecurrenceTypes.Weekly ||
                            request.RecurrenceType == AriaRequestRecurrenceTypes.Monthly)
                {
                    AriaRequestScheduler scheduler = new AriaRequestScheduler();
                    scheduler.Schedule(request);
                    request.Occurence = 0;
                }

                // T20110803.0001 MAH 9/13/2011
                // AriaDbConnection connection = new AriaDbConnection("Aria", "");
                // T20110803.0001 MAH 9/13/2011

                dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, request, new string[] { "RequestNumber" }, clientId);

                // T20110803.0001 MAH June 13 2011
                //#region Execute Immediate Request
                //if (request.RecurrenceType == AriaRequestRecurrenceTypes.Immediate)
                //{
                //    //AriaRequestAgent agent = new AriaRequestAgent();
                //    //agent.ExecuteImmediateRequest(request.RequestID,clientId);
                //    //Look

                //    string computerName = "";
                //    string portNumber = "1500";

                //    AriaEnviromentVariables env = new AriaEnviromentVariables();
                //    env.ClientID = clientId;
                //    env.ConnectionsRefresh();

                //    # region get machine name
                //    computerName = env.Aria40SystemFilesConnectionString.ToString();

                //    //string computerName = @"\\ARIA-HASSAN\C\Aria4XP\Sqldictionary\";
                //    //computerName = @"C:\Aria4XP\Sqldictionary\";
                //    computerName = computerName.Substring(computerName.IndexOf("\\"));

                //    if (computerName.Substring(0, 1) == @"\")
                //    {
                //        if (computerName.Substring(0, 2) == @"\\")
                //        { computerName = computerName.ToString().Substring(2); }
                //        else
                //        {
                //            if (computerName.Substring(0, 1) == @"\")
                //            { computerName = computerName.ToString().Substring(1); }
                //        };

                //        computerName = computerName.Substring(0, computerName.IndexOf(@"\"));
                //    }
                //    else
                //    {
                //        computerName = System.Environment.MachineName.ToString();

                //    };

                //    # endregion get machine name
                //    EventLog.WriteEntry("Aria.EnterpriseServices.RequestHandler", computerName.ToString() , EventLogEntryType.Information);


                //    string s_URL = string.Format("tcp://{0}:{1}/AriaRequestAgent", computerName, portNumber);
                //    AriaRequestAgent agent;
                //    agent = (AriaRequestAgent)Activator.GetObject(typeof(AriaRequestAgent), s_URL);
                //    if (agent == null)
                //    {   Aria.EnterpriseServices.RequestHandler.AriaRequestAgent localagent = new AriaRequestAgent(); 
                //        Aria.DataTypes.RequestHandler.AriaRequestProgress AriaRequestProgress = new AriaRequestProgress();
                //        AriaRequestProgress.Percent = 0;
                //        AriaRequestProgress.Description = "Waiting Service Executeion...";
                //        localagent.UpdateObjectProgress(request.RequestID.ToString()  , AriaRequestProgress, clientId);
                //    }
                //    else
                //    {
                //        try
                //        {
                //            //EventLog.WriteEntry("Aria.EnterpriseServices.RequestHandler", request.RequestID.ToString() , EventLogEntryType.Information);
                //            agent.ExecuteImmediateRequest(request.RequestID, clientId);
                //            //EventLog.WriteEntry("Aria.EnterpriseServices.RequestHandler", "After", EventLogEntryType.Information);
                //            agent = null;
                //            //EventLog.WriteEntry("Aria.EnterpriseServices.RequestHandler", "Done", EventLogEntryType.Information);
                //        }
                //        catch (Exception ex)
                //        {
                //         EventLog.WriteEntry("Aria.EnterpriseServices.RequestHandler", ex.Message.ToString(), EventLogEntryType.Information);
                //        }
                //    };

                //}
                //#endregion Execute Immediate Request
                if (request.RecurrenceType == AriaRequestRecurrenceTypes.Immediate)
                {
                    //AriaEnviromentVariables env = new AriaEnviromentVariables();
                    //env.ClientID = clientId;
                    //env.ConnectionsRefresh();
                    //string url = string.Format("tcp://{0}:{1}/{2}", env.RequestServerName.Trim(), env.RpcPort.ToString().Trim(), typeof(AriaRequestAgent).Name);

                    //AriaRequestAgent remoteAgent = (AriaRequestAgent)Activator.GetObject(typeof(AriaRequestAgent), url);

                    //remoteAgent.ExecuteImmediateRequest(request.RequestID, clientId);

                    AriaRequestAgent agent = new AriaRequestAgent();
                    agent.ExecuteImmediateRequest(request.RequestID, clientId);
                }

                // T20110803.0001 MAH June 13 2011 End
                // T20110803.0001 MAH 9/13/2011
            }
            // T20110803.0001 MAH 9/13/2011
        }

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
            AriaThreadManager manager = new AriaThreadManager();
            manager.CancelRequest(guid,clientId);
        }

        public void RemoveRequest(string guid, string clientId)
        {
            AriaRequest request = GetRequest(guid, clientId);
            request.Status = AriaRequestStatusTypes.Removed;
            AriaDataProvider dataProvider = new AriaDataProvider();

            dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request, new string[] { "Status" }, "RequestID = @RequestID", clientId);

            //MAHMAH
            AriaDbCommand childCommand = new AriaDbCommand("Select * from AriaRequest Where ParentRequestID = @ParentRequestID AND " +
                                                        "Status = @status Order By RequestNumber",
                                                          new AriaDbConnection("", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            childCommand.Parameters.Add("ParentRequestID", request.RequestID);
            childCommand.Parameters.Add("Status", AriaRequestStatusTypes.OnHold.ToString());

            ArrayList list = dataProvider.GetObjectList(childCommand, typeof(AriaRequest));

            for (int i = 0; i < list.Count; i++)
            {
                AriaRequest childRequest = (AriaRequest)list[i];
                childRequest.Status = AriaRequestStatusTypes.Removed;
                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", childRequest, new string[] { "Status" }, "RequestID = @RequestID", clientId);

                AriaDbCommand childChildCommand = new AriaDbCommand("Select * from AriaRequest Where ParentRequestID = @ParentRequestID AND " +
                                                            "Status = @status Order By RequestNumber",
                                                              new AriaDbConnection("", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                childChildCommand.Parameters.Add("ParentRequestID", childRequest.RequestID);
                childChildCommand.Parameters.Add("Status", AriaRequestStatusTypes.Removed.ToString());

                ArrayList childList = dataProvider.GetObjectList(childChildCommand, typeof(AriaRequest));

                for (int j = 0; j < childList.Count; j++)
                {
                    AriaRequest childChildRequest = (AriaRequest)childList[j];
                    childChildRequest.Status = AriaRequestStatusTypes.Canceled;
                    dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", childChildRequest, new string[] { "Status" }, "RequestID = @RequestID", clientId);
                }
            }
            //MAHMAH

            //MAH Log
            AriaDatabaseLogManager.AriaAddToLog(clientId, new AriaDbConnection("Aria", ""), new Guid(guid), "Remove Request");
            //MAH Log

        }

        public void ReExcuteRequest(string guid, string clientId)
        {
            try
            {
                AriaRequest request = GetRequest(guid, clientId);
                request.Status = AriaRequestStatusTypes.OnHold;
                AriaDataProvider dataProvider = new AriaDataProvider();

                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request, new string[] { "Status" }, "RequestID = @RequestID", clientId);

                if (request.RecurrenceType == AriaRequestRecurrenceTypes.Immediate)
                {
                    //AriaEnviromentVariables env = new AriaEnviromentVariables();
                    //env.ClientID = clientId;
                    //env.ConnectionsRefresh();
                    //string url = string.Format("tcp://{0}:{1}/{2}", env.RequestServerName.Trim(), env.RpcPort.ToString().Trim(), typeof(AriaRequestAgent).Name);

                    //AriaRequestAgent remoteAgent = (AriaRequestAgent)Activator.GetObject(typeof(AriaRequestAgent), url);
                    //remoteAgent.ExecuteImmediateRequest(request.RequestID, clientId);

                    AriaRequestAgent agent = new AriaRequestAgent();
                    agent.ExecuteImmediateRequest(request.RequestID, clientId);
                }
            }
            catch (Exception ex)
            {
                Aria.Xml.AriaXmlSerializer x = new Aria.Xml.AriaXmlSerializer();
            }
        }

        public ArrayList LoadImmediateRequests(string  clientId)
        {
               
            AriaDbCommand command = new AriaDbCommand("Select * from AriaRequest Where RecurrenceType = @RecurrenceType AND " + 
                                                        "Status <> @status Order By RequestNumber",
                                                          new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add("RecurrenceType", AriaRequestRecurrenceTypes.Immediate.ToString());
            command.Parameters.Add("Status", AriaRequestStatusTypes.Removed.ToString());

            AriaDataProvider dataProvider = new AriaDataProvider();
            return dataProvider.GetObjectList(command, typeof(AriaRequest));
        }

        //MAHMAH
        public string[] GetPrinters(string clientId)
        {
            //AriaEnviromentVariables env = new AriaEnviromentVariables();
            //env.ClientID = clientId;
            //env.ConnectionsRefresh();
            //string url = string.Format("tcp://{0}:{1}/{2}", env.RequestServerName.Trim(), env.RpcPort.ToString().Trim(), typeof(AriaRequestAgent).Name);

            //AriaRequestAgent remoteAgent = (AriaRequestAgent)Activator.GetObject(typeof(AriaRequestAgent), url);

            //return remoteAgent.GetPrinters();

            AriaRequestAgent agent = new AriaRequestAgent();

            return agent.GetPrinters();
        }
        //MAHMAH

        public override object InitializeLifetimeService()
        {
            return null;
        }
    }
}
