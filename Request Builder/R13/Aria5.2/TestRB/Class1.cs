using System;
using Aria.DataTypes.ObjectDictionary;
using Aria.DataTypes.Messaging;
using Aria.Xml;
using Aria.DataTypes.RequestHandler;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Runtime.Remoting;
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Tcp;
using System.Runtime.Remoting.Services;
using System.Runtime.Serialization.Formatters;
using System.ServiceProcess;
using System.Text;
using System.Threading;
using System.Timers;
using System.Windows.Forms;
using System.Xml;
using Aria.EnterpriseServices.Messaging;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.EnterpriseServices.RequestHandler;
using Aria.EnterpriseServices.RequestHandler.Proxy;
using Aria.Environment;
using Aria.Utilities.Log;
using Aria.Utilities.ParameterSubstitution;
using Aria.Data;
using Aria.DataTypes;


namespace TarekTeks
{

    public partial class AriaRequestHandler 
    {

        public string info;
        public string server;
        public static TextBox myTxt;

        private string _serviceName = null;

        private TcpChannel mi_Channel = null;

        private ObjRef objRefEnviromentVariables = null;
        private AriaEnviromentVariables enviromentVariables = null;

        private ObjRef objRefRequestAgent = null;
        private AriaRequestAgent requestAgent = null;

        private ObjRef objRefObjectDictionaryDBCentric = null;
        private AriaObjectDictionaryDBCentric objectDictionaryDBCentric = null;

        private ObjRef objRefObjectDataPathsExplorer = null;
        private AriaObjectDataPathsExplorer objectDataPathsExplorer = null;

        private ObjRef objRefMessagingDBCentric = null;
        private AriaMessagingDBCentric messagingDBCentric = null;

        private ObjRef objRefMessagingManager = null;
        private AriaMessagingManager messagingManager = null;

        private ObjRef objRefRequestProxy = null;
        private AriaRequestProxy requestProxy = null;

        private ObjRef objRefParameterSubstituter = null;
        private AriaParameterSubstituter parameterSubstituter = null;

        private bool _flag = false;

        //SAB 12-25-2013 Fix RB performance issue [Start]
        private int lstExcQrtr = -1;
        //SAB 12-25-2013 Fix RB performance issue [End]

        private string _instanceName = "";

        public AriaRequestHandler()
        {
            Init();
            OnStart(null);
        }

        private void Init()
        {
            XmlDocument xmlInstanceDocument = new XmlDocument();

            xmlInstanceDocument.Load(Path.Combine(Application.StartupPath, "instance settings.xml"));
            for (int index = 0; index < xmlInstanceDocument.ChildNodes.Count; index++)
            {
                if (xmlInstanceDocument.ChildNodes[index].Name.Trim().ToUpper() == "InstanceName".ToUpper())
                {
                    _instanceName = xmlInstanceDocument.ChildNodes[index].InnerText;
                }
            }

            // T20110803.0001 MAH JULY 7 2011
            _serviceName = "AriaRequestHandler" + _instanceName;
            return;
            // T20110803.0001 MAH JULY 7 2011 End
        }

        protected void OnStart(string[] args)
        {
            //try
            {
                // Mah Log
                AriaWindowsLogManager.AriaAddToLog("Test@ERP OnStart", "Start", EventLogEntryType.Information);
                // Mah Log


                #region Open AriaRequestAgent Channel

                //try
                {

                    // MAH
                    //int s32_Port = int.Parse("1500");
                    //RemotingConfiguration.CustomErrorsMode = CustomErrorsModes.Off;
                    //mi_Channel = new TcpChannel(s32_Port);
                    //ChannelServices.RegisterChannel(mi_Channel);

                    //requestAgent = new AriaRequestAgent();
                    //objRefRequestAgent = RemotingServices.Marshal(requestAgent, "AriaRequestAgent");

                    //objectDictionaryDBCentric = new AriaObjectDictionaryDBCentric();
                    //objRefObjectDictionaryDBCentric = RemotingServices.Marshal(objectDictionaryDBCentric, "Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric");

                    BinaryServerFormatterSinkProvider bcfs = new BinaryServerFormatterSinkProvider();
                    bcfs.TypeFilterLevel = TypeFilterLevel.Full;
                    IDictionary id = new Hashtable();
                    id = new Hashtable();
                    id.Add("port", 1500);
                    MessageBox.Show("new TcpChannel(id, null, bcfs);");
                    mi_Channel = new TcpChannel(id, null, bcfs);

                    MessageBox.Show("ChannelServices.RegisterChannel(mi_Channel, false);");
                    ChannelServices.RegisterChannel(mi_Channel, false);

                    objectDictionaryDBCentric = new AriaObjectDictionaryDBCentric();
                    objRefObjectDictionaryDBCentric = RemotingServices.Marshal(objectDictionaryDBCentric, "Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric" + _instanceName);

                    requestAgent = new AriaRequestAgent();
                    objRefRequestAgent = RemotingServices.Marshal(requestAgent, "Aria.EnterpriseServices.RequestHandler.AriaRequestAgent" + _instanceName);

                    enviromentVariables = new AriaEnviromentVariables();
                    objRefEnviromentVariables = RemotingServices.Marshal(enviromentVariables, "Aria.Environment.AriaEnviromentVariables" + _instanceName);

                    objectDataPathsExplorer = new AriaObjectDataPathsExplorer();
                    objRefObjectDataPathsExplorer = RemotingServices.Marshal(objectDataPathsExplorer, "Aria.EnterpriseServices.ObjectDictionary.AriaObjectDataPathsExplorer" + _instanceName);

                    messagingDBCentric = new AriaMessagingDBCentric();
                    objRefMessagingDBCentric = RemotingServices.Marshal(messagingDBCentric, "Aria.EnterpriseServices.Messaging.AriaMessagingDBCentric" + _instanceName);

                    messagingManager = new AriaMessagingManager();
                    objRefMessagingManager = RemotingServices.Marshal(messagingManager, "Aria.EnterpriseServices.Messaging.AriaMessagingManager" + _instanceName);

                    requestProxy = new AriaRequestProxy();
                    objRefRequestProxy = RemotingServices.Marshal(requestProxy, "Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy" + _instanceName);

                    parameterSubstituter = new AriaParameterSubstituter();
                    objRefParameterSubstituter = RemotingServices.Marshal(parameterSubstituter, "Aria.Utilities.ParameterSubstitution" + _instanceName);
                    // MAH


                }
                try { } // test code only *x*tmi*x*
                catch (Exception Ex)
                {
                    // Mah Log
                    //AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, Ex.Message, EventLogEntryType.Error);
                    AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, Ex.Message, EventLogEntryType.Error, Ex);
                    // Mah Log

                    //EventLog.WriteEntry("Aria.Services.RequestHandler", "Channel not Open", EventLogEntryType.Information);
                    //EventLog.WriteEntry("Aria.Services.RequestHandler", Ex.Message.ToString(), EventLogEntryType.Information);
                }
                #endregion

                # region Loop On Clients File On System Master SQL DB
                //try
                {
                    string clientId = "";

                    Aria.Environment.AriaEnviromentVariables Env = new Aria.Environment.AriaEnviromentVariables();

                    System.Data.SqlClient.SqlConnection Conn = new System.Data.SqlClient.SqlConnection(Env.Aria50SystemFilesConnectionString);
                    Conn.Open();

                    // T20110803.0001 MAH JULY 7 2011
                    //System.Data.SqlClient.SqlCommand Cmd = new System.Data.SqlClient.SqlCommand("Select CCLIENTID from CLIENTS ", Conn);

                    string serverComputerName = SystemInformation.ComputerName.ToUpper();
                    System.Data.SqlClient.SqlCommand Cmd = new System.Data.SqlClient.SqlCommand("Select CCLIENTID from CLIENTS where ReqServer = @ReqServer", Conn);
                    Cmd.Parameters.Add(new System.Data.SqlClient.SqlParameter("ReqServer", serverComputerName));

                    //
                    this.server = serverComputerName;


                    // T20110803.0001 MAH JULY 7 2011 End


                    System.Data.DataTable Dt = new System.Data.DataTable();
                    Dt.Load(Cmd.ExecuteReader());
                    Conn.Close();
                    // T20100512.0026 Hassan.I 20-05-2010 [Begin]

                    // Mah Log
                    AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, "Loop on Clients", EventLogEntryType.Information);
                    // Mah Log

                    for (int i = 0; i < Dt.Rows.Count; i++)
                    {
                        clientId = Dt.Rows[i]["CCLIENTID"].ToString();

                        // Mah Log
                        AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, "Execute Requests for Client:" + clientId, EventLogEntryType.Information);
                        // Mah Log

                        AriaRequestAgent requestAgent = new AriaRequestAgent();
                        #region Genete on computer start up requests
                        //try
                        {
                            requestAgent.GenerateOnComputerStartupRequests(clientId);
                        }
                        try { }
                        catch (Exception ex)
                        {
                            // Mah Log
                            //AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, ex.Message, EventLogEntryType.Error);
                            AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, ex.Message, EventLogEntryType.Error, ex);
                            // Mah Log
                            //EventLog.WriteEntry("Aria.Services.RequestHandler", ex.Message, EventLogEntryType.Error);
                        }

                        #endregion
                        #region Execure one time only requests
                        //try
                        {
                            requestAgent.ExecuteOneTimeOnlyRequests(clientId);
                        }
                        try { }
                        catch (Exception ex)
                        {
                            // Mah Log
                            //AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, ex.Message, EventLogEntryType.Error);
                            AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, ex.Message, EventLogEntryType.Error, ex);
                            // Mah Log
                            //EventLog.WriteEntry("Aria.Services.RequestHandler", ex.Message, EventLogEntryType.Error);
                        }
                        #endregion


                        // T20110803.0001 MAH JULY 7 2011
                        //#region Execute immediate requests
                        //try
                        //{
                        //   requestAgent.ExecuteImmediateRequests(clientId);
                        //}
                        //catch (Exception ex)
                        //{
                        //    EventLog.WriteEntry("Aria.Services.RequestHandler", ex.Message, EventLogEntryType.Error);
                        //}
                        // #endregion 
                        // T20110803.0001 MAH JULY 7 2011 End
                    }
                }
                try { }
                catch (Exception Ex)
                {
                    // Mah Log
                    //AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, Ex.Message, EventLogEntryType.Error);
                    AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, Ex.Message, EventLogEntryType.Error, Ex);
                    // Mah Log
                    //EventLog.WriteEntry("Aria.Services.RequestHandler", Ex.Message.ToString(), EventLogEntryType.Information);
                }

                # endregion
                //this.RequestAgentTimer.Start();

            }
            try { }
            catch (Exception Ex)
            {
                // Mah Log
                //AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, Ex.Message, EventLogEntryType.Error);
                AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, Ex.Message, EventLogEntryType.Error, Ex);
                // Mah Log
            }
        }

        
        public void loopOverRequests(TextBox txt)
        {
            myTxt = txt;

            //SAB 12-25-2013 Fix RB performance issue [Start]
            string Aria_Env = System.Environment.GetEnvironmentVariable("ARIA_ENVIRONMENT", EnvironmentVariableTarget.Machine);
            if (Aria_Env != "AriaInHouse" && DateTime.Now.Minute / 15 == lstExcQrtr)
            {
                return;
            }
            else
            {
                lstExcQrtr = DateTime.Now.Minute / 15;
            }
            //SAB 12-25-2013 Fix RB performance issue [End]

            if (_flag == true) { return; };
            #region Close AriaRequestAgent Channel
            ////AriaRequestAgent
            //if (mi_Service != null)
            //    RemotingServices.Unmarshal(mi_Service);

            //if (ImmediateService != null)
            //    RemotingServices.Disconnect(ImmediateService);

            //if (mi_Channel != null)
            //    ChannelServices.UnregisterChannel(mi_Channel);

            //mi_Service = null;
            //ImmediateService = null;
            //mi_Channel = null;
            #endregion

            #region Open AriaRequestAgent Channel

            //try
            //{

            //    int s32_Port = int.Parse("1500");
            //    RemotingConfiguration.CustomErrorsMode = CustomErrorsModes.Off;
            //    mi_Channel = new TcpChannel(s32_Port);
            //    ChannelServices.RegisterChannel(mi_Channel);

            //    ImmediateService = new AriaRequestAgent();

            //    mi_Service = RemotingServices.Marshal(ImmediateService, "AriaRequestAgent");

            //}
            //catch (Exception Ex)
            //{
            //    EventLog.WriteEntry("Aria.Services.RequestHandler", "Channel not Open", EventLogEntryType.Information);
            //    EventLog.WriteEntry("Aria.Services.RequestHandler", Ex.Message.ToString(), EventLogEntryType.Information);
            //}
            #endregion

            # region Loop On Clients File On System Master SQL DB
            string clientId = "";
            Aria.Environment.AriaEnviromentVariables Env = new Aria.Environment.AriaEnviromentVariables();

            System.Data.SqlClient.SqlConnection Conn = new System.Data.SqlClient.SqlConnection(Env.Aria50SystemFilesConnectionString);
            Conn.Open();

            // T20110803.0001 MAH JULY 7 2011
            //System.Data.SqlClient.SqlCommand Cmd = new System.Data.SqlClient.SqlCommand("Select CCLIENTID from CLIENTS ", Conn);

            string serverComputerName = SystemInformation.ComputerName.ToUpper();
            System.Data.SqlClient.SqlCommand Cmd = new System.Data.SqlClient.SqlCommand("Select CCLIENTID from CLIENTS where ReqServer = @ReqServer", Conn);
            Cmd.Parameters.Add(new System.Data.SqlClient.SqlParameter("ReqServer", serverComputerName));


            // T20110803.0001 MAH JULY 7 2011 End

            System.Data.DataTable Dt = new System.Data.DataTable();
            Dt.Load(Cmd.ExecuteReader());
            Conn.Close();


            // Mah Log
            AriaWindowsLogManager.AriaAddToLog("Test@RB"+this.info, "Loop on Clients", EventLogEntryType.Information);
            // Mah Log

            for (int i = 0; i < Dt.Rows.Count; i++)
            {
                clientId = Dt.Rows[i]["CCLIENTID"].ToString();

                // Mah Log
                AriaWindowsLogManager.AriaAddToLog("Test@RB"+this.info, "Execute Requests for Client:" + clientId, EventLogEntryType.Information);
                // Mah Log

                AriaRequestAgent requestAgent = new AriaRequestAgent();
                # region Generate Schedule Requests
                _flag = true;
                //try  //test only
                {
                    //requestAgent.GenerateScheduleRequests(clientId);
                    GenScheduleRequests(clientId);
                }
                try { }  // test only
                catch (Exception ex)
                {
                    // Mah Log
                    //AriaWindowsLogManager.AriaAddToLog("Test@RB" + this.info, ex.Message, EventLogEntryType.Error);
                    AriaWindowsLogManager.AriaAddToLog("Test@RB" + this.info, ex.Message, EventLogEntryType.Error,ex);
                    // Mah Log
                    //EventLog.WriteEntry("Aria.Services.RequestHandler", ex.Message, EventLogEntryType.Error);
                }

                _flag = false;
                #endregion

                # region Execute One Time Only Requests
                //try
                {
                    requestAgent.ExecuteOneTimeOnlyRequests(clientId);
                }
                try { }
                catch (Exception ex)
                {
                    // Mah Log
                    //AriaWindowsLogManager.AriaAddToLog("Test@RB" + this.info, ex.Message, EventLogEntryType.Error);
                    AriaWindowsLogManager.AriaAddToLog("Test@RB" + this.info, ex.Message, EventLogEntryType.Error, ex);
                    // Mah Log
                    //EventLog.WriteEntry("Aria.Services.RequestHandler", ex.Message, EventLogEntryType.Error);
                }
                #endregion

                # region Execute Immediate Requests
                //try
                {
                    requestAgent.ExecuteImmediateRequests(clientId);
                }
                try { }
                catch (Exception ex)
                {
                    // Mah Log
                    //AriaWindowsLogManager.AriaAddToLog("Test@RB" + this.info, ex.Message, EventLogEntryType.Error);
                    AriaWindowsLogManager.AriaAddToLog("Test@RB" + this.info, ex.Message, EventLogEntryType.Error, ex);
                    // Mah Log
                    //EventLog.WriteEntry("Aria.Services.RequestHandler", ex.Message, EventLogEntryType.Error);
                }
                #endregion

            };

            #endregion
        }

        public void GenScheduleRequests(string clientId)
        {
            string AriaDbCommandText = "SELECT * FROM AriaRequest " +
                                      "WHERE NextChildRequestDateTime <= @Now AND ( Status = @OnHoldStatus OR Status = @StartedStatus ) " +
                                      "AND (RecurrenceType = @Daily OR RecurrenceType = @Weekly OR RecurrenceType = @Monthly)";
            
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("Now", DateTime.Now));
            command.Parameters.Add(new AriaDbParameter("OnHoldStatus", AriaRequestStatusTypes.OnHold.ToString()));
            command.Parameters.Add(new AriaDbParameter("StartedStatus", AriaRequestStatusTypes.Started.ToString()));
            command.Parameters.Add(new AriaDbParameter("Daily", AriaRequestRecurrenceTypes.Daily.ToString()));
            command.Parameters.Add(new AriaDbParameter("Weekly", AriaRequestRecurrenceTypes.Weekly.ToString()));
            command.Parameters.Add(new AriaDbParameter("Monthly", AriaRequestRecurrenceTypes.Monthly.ToString()));

            AriaDataProvider dataProvider = new AriaDataProvider();

            ArrayList requests = dataProvider.GetObjectList(command, typeof(AriaRequest));


            for (int requestIndex = 0; requestIndex < requests.Count; requestIndex++)
            {
                AriaRequest request = null;

                request = (AriaRequest)requests[requestIndex];

                if (GetRequestStartAfterDateTime(request) <= DateTime.Now)
                {
                    if (request.Status == AriaRequestStatusTypes.OnHold)
                    {
                        request.Status = AriaRequestStatusTypes.Started;
                    }

                    AriaRequestScheduler scheduler = new AriaRequestScheduler();

                    Aria.Xml.AriaXmlSerializer ser = new Aria.Xml.AriaXmlSerializer();
                    AriaRequest newRequest = (AriaRequest)ser.ConvertFromXml(ser.ConvertToXml(request));
                    //scheduler.Schedule(newRequest);


                    //if (newRequest.NextChildRequestDateTime > DateTime.Now)
                    //{
                    //    CloneOneTimeOnlyRequest(request, clientId);
                    //}

                    //
                    //try
                    {
                        CloneOneTimeOnlyRequest(request, clientId);
                    }
                    try { }
                    catch (Exception Ex)
                    { AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, Ex.Message, EventLogEntryType.Error, Ex); }

                    do
                    {
                        scheduler.Schedule(newRequest);
                    } while (newRequest.NextChildRequestDateTime < DateTime.Now && newRequest.Status == AriaRequestStatusTypes.Started);

                    //CloneOneTimeOnlyRequest(newRequest, clientId);


                    //dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                    //                            new string[] { "Status", "NextChildRequestDateTime", "Occurence" }, @"RequestId = @RequestId", clientId);
                    dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", newRequest,
                                                new string[] { "Status", "NextChildRequestDateTime", "Occurence" }, @"RequestId = @RequestId", clientId);
                }
            }
        }

        private DateTime GetRequestStartAfterDateTime(AriaRequest request)
        {
            return request.StartAfterDate.AddHours(request.RequestStartTime.Hour).
                                                    AddMinutes(request.RequestStartTime.Minute).
                                                        AddSeconds(request.RequestStartTime.Second);
        }
        private void CloneOneTimeOnlyRequest(AriaRequest request, string clientId)
        {
            AriaRequest onceRequest = new AriaRequest();

            onceRequest.SetRequestDateRangeAfterOccurence(request.NextChildRequestDateTime, 1);

            onceRequest.Description = request.Description;

            //SAB 02-16-2014 Generate different XML files for Scheduler Request [Start]
            string reqNewXMLFile = "", reqOldXMLFile = "", reqNewOutPutFile = "", reqOldOutPutFile = "";
            AriaOptionGridXmlDataSet reqOptionGrid = new AriaOptionGridXmlDataSet();
            System.Xml.XmlDocument reqOptionGridXML = new System.Xml.XmlDocument();

            if (request.MethodObjectName != null && request.MethodArgumentList != null)
            {
                foreach (object obj in request.MethodArgumentList)
                {
                    AriaArgument arg1 = (AriaArgument)obj;
                    if (arg1.Value is Aria.DataTypes.AriaOptionGridXmlDataSet)
                    {
                        reqNewXMLFile = Path.GetRandomFileName().Replace(".", "") + ".xml";
                        reqOptionGrid = arg1.Value as Aria.DataTypes.AriaOptionGridXmlDataSet;
                        //try
                        {
                            reqOptionGridXML.Load(reqOptionGrid.FileName);
                        }
                        try { }
                        catch (Exception ex)
                        {
                            AriaWindowsLogManager.AriaAddToLog("Test@RB OnStart" + this.info, ex.Message, EventLogEntryType.Error, ex);
                        }
                        reqOldOutPutFile = reqOptionGridXML.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText;

                        reqNewOutPutFile = Path.GetDirectoryName(reqOldOutPutFile) + "\\" + reqNewXMLFile.Replace(".xml", Path.GetExtension(reqOldOutPutFile));
                        reqOptionGridXML.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText = reqNewOutPutFile;

                        reqNewXMLFile = Path.GetDirectoryName(((Aria.DataTypes.AriaOptionGridXmlDataSet)reqOptionGrid).FileName) + "\\" + reqNewXMLFile;
                        reqOptionGridXML.Save(reqNewXMLFile);
                        reqOldXMLFile = reqOptionGrid.FileName;
                        reqOptionGrid.FileName = reqNewXMLFile;
                    }
                }
            }
            ((AriaArgument)request.MethodArgumentList[0]).Value = reqOptionGrid;
            //SAB 02-16-2014 Generate different XML files for Scheduler Request [End]

            onceRequest.SetRequestMethodSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList);
            onceRequest.SetRequestLoginSettings(request.LoginType, request.UserName, request.Password);
            onceRequest.SetOneTimeOnlyRequestSettings(request.RequestStartTime);
            onceRequest.SetRequestContextSettings(request.Context.CompanyName, request.Context.CompanyName);

            onceRequest.RepeatTask = request.RepeatTask;
            onceRequest.RepeatTaskEvery = request.RepeatTaskEvery;
            onceRequest.RepeatTaskUntilDuration = request.RepeatTaskUntilDuration;
            onceRequest.RepeatTaskUntillTime = request.RepeatTaskUntillTime;
            onceRequest.RepeatTaskUntilType = request.RepeatTaskUntilType;
            onceRequest.HoldTaskIfStillRunning = request.HoldTaskIfStillRunning;

            onceRequest.EventObjectName = request.EventObjectName;
            onceRequest.EventConditionList = request.EventConditionList;

            onceRequest.CompleteNotification = request.CompleteNotification;
            //SAB 02-16-2014 Generate different XML files for Scheduler Request [Start]
            if (onceRequest.CompleteNotification != null)
            {
                foreach (DictionaryEntry entry in onceRequest.CompleteNotification.Attachment)
                {
                    if (entry.Value.ToString().Trim() != "" && entry.Value != null && entry.Key.ToString().Trim() == reqOldOutPutFile)
                    {
                        onceRequest.CompleteNotification.Attachment.Remove(entry.Key);
                        onceRequest.CompleteNotification.Attachment.Add(reqNewOutPutFile, entry.Value);
                        break;
                    }
                }
            }
            //SAB 02-16-2014 Generate different XML files for Scheduler Request [End]
            onceRequest.ErrorNotification = request.ErrorNotification;

            onceRequest.ParentRequestID = new Guid(request.RequestID).ToString();
            onceRequest.ParentRequestNumber = request.RequestNumber;

            AriaDataProvider dataProvider = new AriaDataProvider();

            AriaDbConnection connection = new AriaDbConnection("Aria", "");

            dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, onceRequest, new string[] { "RequestNumber" }, clientId);

            //MAH Log
            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Create Request Occurance", new AriaLogElement("Date", onceRequest.StartAfterDate.ToString()), new AriaLogElement("Time", onceRequest.RequestStartTime.ToString()));
            //MAH Log

            if (onceRequest.RepeatTask)
            {
                
                ArrayList dateTimeList = GetRepeatableNextChildDateTimes(onceRequest);

                for (int i = 0; i < dateTimeList.Count; i++)
                {
                    AriaRequest repeatRequest = new AriaRequest();

                    repeatRequest.SetRequestDateRangeAfterOccurence(request.NextChildRequestDateTime, 1);

                    repeatRequest.Description = request.Description;

                    //SAB 02-03-2014 Generate different XML files for Scheduler Request [Start]
                    string newXMLFile = "", oldXMLFile = "", newOutPutFile = "", oldOutPutFile = "";
                    AriaOptionGridXmlDataSet optionGrid = new AriaOptionGridXmlDataSet();
                    System.Xml.XmlDocument optionGridXML = new System.Xml.XmlDocument();

                    if (request.MethodObjectName != null && request.MethodArgumentList != null)
                    {
                        foreach (object obj in request.MethodArgumentList)
                        {
                            AriaArgument arg1 = (AriaArgument)obj;
                            if (arg1.Value is Aria.DataTypes.AriaOptionGridXmlDataSet)
                            {
                                newXMLFile = Path.GetRandomFileName().Replace(".", "") + ".xml";
                                optionGrid = arg1.Value as Aria.DataTypes.AriaOptionGridXmlDataSet;
                                optionGridXML.Load(optionGrid.FileName);
                                oldOutPutFile = optionGridXML.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText;

                                newOutPutFile = Path.GetDirectoryName(oldOutPutFile) + "\\" + newXMLFile.Replace(".xml", Path.GetExtension(oldOutPutFile));
                                optionGridXML.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText = newOutPutFile;

                                newXMLFile = Path.GetDirectoryName(((Aria.DataTypes.AriaOptionGridXmlDataSet)optionGrid).FileName) + "\\" + newXMLFile;
                                optionGridXML.Save(newXMLFile);
                                oldXMLFile = optionGrid.FileName;
                                optionGrid.FileName = newXMLFile;
                            }
                        }
                    }
                    ((AriaArgument)request.MethodArgumentList[0]).Value = optionGrid;
                    //SAB 02-03-2014 Generate different XML files for Scheduler Request [End]

                    repeatRequest.SetRequestMethodSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList);
                    repeatRequest.SetRequestLoginSettings(request.LoginType, request.UserName, request.Password);
                    repeatRequest.SetOneTimeOnlyRequestSettings(request.RequestStartTime);
                    repeatRequest.SetRequestContextSettings(request.Context.CompanyName, request.Context.CompanyName);

                    repeatRequest.EventObjectName = request.EventObjectName;
                    repeatRequest.EventConditionList = request.EventConditionList;

                    repeatRequest.CompleteNotification = request.CompleteNotification;
                    //SAB 02-16-2014 Generate different XML files for Scheduler Request [Start]
                    if (repeatRequest.CompleteNotification != null)
                    {
                        foreach (DictionaryEntry entry in repeatRequest.CompleteNotification.Attachment)
                        {
                            if (entry.Value.ToString().Trim() != "" && entry.Value != null && entry.Key.ToString().Trim() == oldOutPutFile)
                            {
                                repeatRequest.CompleteNotification.Attachment.Remove(entry.Key);
                                repeatRequest.CompleteNotification.Attachment.Add(newOutPutFile, entry.Value);
                                break;
                            }
                        }
                    }
                    //SAB 02-16-2014 Generate different XML files for Scheduler Request [End]                    
                    repeatRequest.ErrorNotification = request.ErrorNotification;

                    repeatRequest.ParentRequestID = new Guid(request.RequestID).ToString();
                    repeatRequest.ParentRequestNumber = request.RequestNumber;

                    repeatRequest.RequestStartTime = (DateTime)dateTimeList[i];

                    repeatRequest.StartAfterDate = repeatRequest.StartAfterDate.AddDays(repeatRequest.RequestStartTime.Subtract(new DateTime(1900, 1, 1)).Days);

                    dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, repeatRequest, new string[] { "RequestNumber" }, clientId);

                    //MAH Log
                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Create Request Occurance", new AriaLogElement("Date", repeatRequest.StartAfterDate.ToString()), new AriaLogElement("Time", repeatRequest.RequestStartTime.ToString()));
                    //MAH Log
                }
            }
        }
        private ArrayList GetRepeatableNextChildDateTimes(AriaRequest request)
        {
            ArrayList dateTimeArrayList = new ArrayList();
            DateTime nextRepeatDateTime = DateTime.MinValue;

            DateTime endDateTime;
            if (request.RepeatTaskUntilType == AriaRequestRepeatUntilTypes.Duration)
            {
                endDateTime = request.RequestStartTime.AddMinutes(request.RepeatTaskUntilDuration);
            }
            else
            {
                endDateTime = new DateTime(request.RequestStartTime.Year,
                                                        request.RequestStartTime.Month,
                                                            request.RequestStartTime.Day,
                                                                request.RepeatTaskUntillTime.Hour,
                                                                    request.RepeatTaskUntillTime.Minute,
                                                                        request.RepeatTaskUntillTime.Second);
            }

            for (int minute = request.RepeatTaskEvery;
                 request.RequestStartTime.AddMinutes(minute) <= endDateTime;
                 minute += request.RepeatTaskEvery)
            {
                dateTimeArrayList.Add(request.RequestStartTime.AddMinutes(minute));
            }

            return dateTimeArrayList;
        }
        
        
    }


    public class AriaWindowsLogManager
    {
        public AriaWindowsLogManager()
        {
        }

        public static void AriaAddToLog(string source, string message, EventLogEntryType type)
        {

            //SAB 01/21/2013 [Start]
            //AriaEnviromentVariables env = new AriaEnviromentVariables();
            //if (env.DebugMode == DebugMode.Enabled)
            //{
            //    //SAB 01/21/2013 [End]
            //    EventLog.WriteEntry(source, message, type);
            //}
            AriaAddToLog(source, ">>"+message, type, null);

        }
        public static void AriaAddToLog(string source, string message, EventLogEntryType type,Exception ex)
        {

            //SAB 01/21/2013 [Start]
            AriaEnviromentVariables env = new AriaEnviromentVariables();
            if (env.DebugMode == DebugMode.Enabled)
            {
                string msg = message;
                if (ex != null)
                {
                    Exception InnExc = ex.GetBaseException();
                    msg = InnExc.Message + ":" +
                        InnExc.Source.ToString() + ":" +
                        InnExc.StackTrace.ToString() + ":" +
                        InnExc.TargetSite.ToString() + ":";
                }
                EventLog.WriteEntry(source, message, type);
                //AriaRequestHandler.myTxt.Text = msg;
                
            }
        }
    }
    

}