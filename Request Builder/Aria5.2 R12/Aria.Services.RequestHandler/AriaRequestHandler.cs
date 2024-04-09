using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.ServiceProcess;
using System.Text;
using Aria.EnterpriseServices.RequestHandler;
using System.Timers;
using System.Xml;
using System.Drawing;
using System.Collections;
using System.Data;
using System.Threading;
using System.Runtime.Remoting;
using System.Runtime.Remoting.Services;
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Tcp;
using System.Windows.Forms;
using Aria.EnterpriseServices.ObjectDictionary;
using System.Runtime.Serialization.Formatters;
using Aria.EnterpriseServices.RequestHandler.Proxy;
using Aria.Environment;
using Aria.EnterpriseServices.Messaging;
using Aria.Utilities.ParameterSubstitution;
using Aria.Utilities.Log;

namespace Aria.Services.RequestHandler
{

    public partial class AriaRequestHandler : ServiceBase
    {
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

        public AriaRequestHandler()
        {
            Init();

            InitializeComponent();
        }

        // mah 5/20/2014
        private void _requestHangTimer_Elapsed(object sender, ElapsedEventArgs e)
        {
            EventLog.WriteEntry("Request Hang1", "", EventLogEntryType.Warning);
            foreach (var x in Process.GetProcesses())
            {
                if (x.ProcessName.ToUpper().Contains(@"RequestHandler".ToUpper()) && DateTime.Now.Subtract(x.StartTime).TotalMinutes > 2) 
                {
                    EventLog.WriteEntry("Request Hang2","", EventLogEntryType.Error);

                    x.Kill();
                    EventLog.WriteEntry("Request Kill", "", EventLogEntryType.Error);
                    return;
                }
            }
        }
        // mah 5/20/2014

        private void Init()
        {
            //

            // T20110803.0001 MAH JULY 7 2011
            _serviceName = "Aria Request Handler";
            return;
            // T20110803.0001 MAH JULY 7 2011 End

            try
            {
                // Mah Log
                AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", "Initialize", EventLogEntryType.Information);
                // Mah Log

                XmlDocument xmlDocument = new XmlDocument();

                xmlDocument.Load(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));

                XmlElement documentElement = xmlDocument.DocumentElement;

                for (int index = 0; index < documentElement.ChildNodes.Count; index++)
                {
                    if (documentElement.ChildNodes[index].Name == "RequestHandlerService")
                    {
                        XmlNode xmlNode = null;

                        for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                        {
                            xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                            if (xmlNode.Name == "ServiceName")
                            {
                                _serviceName = xmlNode.InnerText;

                                break;
                            }
                        }
                    }
                }

                xmlDocument.Save(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));
            }
            catch (Exception ex)
            {
                // Mah Log
                AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", ex.Message, EventLogEntryType.Error);
                // Mah Log
                //EventLog.WriteEntry("Aria.Services.RequestHandler", ex.Message, EventLogEntryType.Error);
            }
        }

        protected override void OnStart(string[] args)
        {
            try
            {
                // Mah Log
                AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", "Start", EventLogEntryType.Information);
                // Mah Log


                #region Open AriaRequestAgent Channel

                try
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
                    mi_Channel = new TcpChannel(id, null, bcfs);

                    ChannelServices.RegisterChannel(mi_Channel, false);

                    objectDictionaryDBCentric = new AriaObjectDictionaryDBCentric();
                    objRefObjectDictionaryDBCentric = RemotingServices.Marshal(objectDictionaryDBCentric, "Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric");

                    requestAgent = new AriaRequestAgent();
                    objRefRequestAgent = RemotingServices.Marshal(requestAgent, "Aria.EnterpriseServices.RequestHandler.AriaRequestAgent");

                    enviromentVariables = new AriaEnviromentVariables();
                    objRefEnviromentVariables = RemotingServices.Marshal(enviromentVariables, "Aria.Environment.AriaEnviromentVariables");

                    objectDataPathsExplorer = new AriaObjectDataPathsExplorer();
                    objRefObjectDataPathsExplorer = RemotingServices.Marshal(objectDataPathsExplorer, "Aria.EnterpriseServices.ObjectDictionary.AriaObjectDataPathsExplorer");

                    messagingDBCentric = new AriaMessagingDBCentric();
                    objRefMessagingDBCentric = RemotingServices.Marshal(messagingDBCentric, "Aria.EnterpriseServices.Messaging.AriaMessagingDBCentric");

                    messagingManager = new AriaMessagingManager();
                    objRefMessagingManager = RemotingServices.Marshal(messagingManager, "Aria.EnterpriseServices.Messaging.AriaMessagingManager");

                    requestProxy = new AriaRequestProxy();
                    objRefRequestProxy = RemotingServices.Marshal(requestProxy, "Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy");

                    parameterSubstituter = new AriaParameterSubstituter();
                    objRefParameterSubstituter = RemotingServices.Marshal(parameterSubstituter, "Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy");
                    // MAH


                }
                catch (Exception Ex)
                {
                    // Mah Log
                    AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", Ex.Message, EventLogEntryType.Error);
                    // Mah Log

                    //EventLog.WriteEntry("Aria.Services.RequestHandler", "Channel not Open", EventLogEntryType.Information);
                    //EventLog.WriteEntry("Aria.Services.RequestHandler", Ex.Message.ToString(), EventLogEntryType.Information);
                }
                #endregion

                # region Loop On Clients File On System Master SQL DB

                try
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



                    // T20110803.0001 MAH JULY 7 2011 End


                    System.Data.DataTable Dt = new System.Data.DataTable();
                    Dt.Load(Cmd.ExecuteReader());
                    Conn.Close();
                    // T20100512.0026 Hassan.I 20-05-2010 [Begin]

                    // Mah Log
                    //AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", "Loop on Clients", EventLogEntryType.Information);
                    // Mah Log

                    for (int i = 0; i < Dt.Rows.Count; i++)
                    {
                        clientId = Dt.Rows[i]["CCLIENTID"].ToString();

                        // Mah Log
                        //AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", "Execute Requests for Client:" + clientId, EventLogEntryType.Information);
                        // Mah Log

                        AriaRequestAgent requestAgent = new AriaRequestAgent();
                        #region Genete on computer start up requests
                        try
                        {
                            requestAgent.GenerateOnComputerStartupRequests(clientId);
                        }
                        catch (Exception ex)
                        {
                            // Mah Log
                            AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", ex.Message, EventLogEntryType.Error);
                            // Mah Log
                            //EventLog.WriteEntry("Aria.Services.RequestHandler", ex.Message, EventLogEntryType.Error);
                        }

                        #endregion
                        #region Execure one time only requests
                        try
                        {
                            requestAgent.ExecuteOneTimeOnlyRequests(clientId);
                        }
                        catch (Exception ex)
                        {
                            // Mah Log
                            AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", ex.Message, EventLogEntryType.Error);
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
                catch (Exception Ex)
                {
                    // Mah Log
                    AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", Ex.Message, EventLogEntryType.Error);
                    // Mah Log
                    //EventLog.WriteEntry("Aria.Services.RequestHandler", Ex.Message.ToString(), EventLogEntryType.Information);
                }

                # endregion
                this.RequestAgentTimer.Start();

            }
            catch (Exception Ex)
            {
                // Mah Log
                AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", Ex.Message, EventLogEntryType.Error);
                // Mah Log
            }
        }

        protected override void OnStop()
        {
            this.RequestAgentTimer.Stop();
            #region Close AriaRequestAgent Channel
            //AriaRequestAgent
            if (objRefRequestAgent != null)
                RemotingServices.Unmarshal(objRefRequestAgent);

            if (requestAgent != null)
                RemotingServices.Disconnect(requestAgent);

            // MAH
            if (objRefObjectDictionaryDBCentric != null)
                RemotingServices.Unmarshal(objRefObjectDictionaryDBCentric);

            if (objectDictionaryDBCentric != null)
                RemotingServices.Disconnect(objectDictionaryDBCentric);

            if (objRefEnviromentVariables != null)
                RemotingServices.Unmarshal(objRefEnviromentVariables);

            if (enviromentVariables != null)
                RemotingServices.Disconnect(enviromentVariables);

            if (objRefObjectDataPathsExplorer != null)
                RemotingServices.Unmarshal(objRefObjectDataPathsExplorer);

            if (objectDataPathsExplorer != null)
                RemotingServices.Disconnect(objectDataPathsExplorer);

            if (objRefMessagingDBCentric != null)
                RemotingServices.Unmarshal(objRefMessagingDBCentric);

            if (messagingDBCentric != null)
                RemotingServices.Disconnect(messagingDBCentric);

            if (objRefMessagingManager != null)
                RemotingServices.Unmarshal(objRefMessagingManager);

            if (messagingManager != null)
                RemotingServices.Disconnect(messagingManager);

            if (objRefRequestProxy != null)
                RemotingServices.Unmarshal(objRefRequestProxy);

            if (requestProxy != null)
                RemotingServices.Disconnect(requestProxy);


            if (objRefParameterSubstituter != null)
                RemotingServices.Unmarshal(objRefParameterSubstituter);

            if (parameterSubstituter != null)
                RemotingServices.Disconnect(parameterSubstituter);
            // MAH

            if (mi_Channel != null)
                ChannelServices.UnregisterChannel(mi_Channel);

            objRefRequestAgent = null;
            requestAgent = null;

            objectDictionaryDBCentric = null;
            objRefObjectDictionaryDBCentric = null;

            enviromentVariables = null;
            objRefEnviromentVariables = null;

            objectDataPathsExplorer = null;
            objRefObjectDataPathsExplorer = null;

            messagingDBCentric = null;
            objRefMessagingDBCentric = null;

            messagingManager = null;
            objRefMessagingManager = null;

            requestProxy = null;
            objRefRequestProxy = null;

            parameterSubstituter = null;
            objRefParameterSubstituter = null;

            mi_Channel = null;
            #endregion

            // Mah Log
            AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", "Stop", EventLogEntryType.Information);
            // Mah Log

            //EventLog.WriteEntry("Aria.Services.RequestHandler", "Service Stoped", EventLogEntryType.Information);
        }



        private void _requestAgentTimer_Elapsed(object sender, ElapsedEventArgs e)
        {
            //SAB 12-25-2013 Fix RB performance issue [Start]
            string Aria_Env = System.Environment.GetEnvironmentVariable("ARIA_ENVIRONMENT", EnvironmentVariableTarget.Machine);
            if (Aria_Env != "AriaInHouse" && DateTime.Now.Minute / 15 == lstExcQrtr)
            {
                if (DateTime.Now.Minute / 15 == lstExcQrtr)
                {
                    return;
                }
                else
                {
                    lstExcQrtr = DateTime.Now.Minute / 15;
                }
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
            AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", "Loop on Clients", EventLogEntryType.Information);
            // Mah Log

            for (int i = 0; i < Dt.Rows.Count; i++)
            {
                clientId = Dt.Rows[i]["CCLIENTID"].ToString();

                // Mah Log
                AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", "Execute Requests for Client:" + clientId, EventLogEntryType.Information);
                // Mah Log

                AriaRequestAgent requestAgent = new AriaRequestAgent();
                # region Generate Schedule Requests
                _flag = true;
                try
                {
                    requestAgent.GenerateScheduleRequests(clientId);
                }
                catch (Exception ex)
                {
                    // Mah Log
                    AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", ex.Message, EventLogEntryType.Error);
                    // Mah Log
                    //EventLog.WriteEntry("Aria.Services.RequestHandler", ex.Message, EventLogEntryType.Error);
                }

                _flag = false;
                #endregion

                # region Execute One Time Only Requests
                try
                {
                    requestAgent.ExecuteOneTimeOnlyRequests(clientId);
                }
                catch (Exception ex)
                {
                    // Mah Log
                    AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", ex.Message, EventLogEntryType.Error);
                    // Mah Log
                    //EventLog.WriteEntry("Aria.Services.RequestHandler", ex.Message, EventLogEntryType.Error);
                }
                #endregion

                # region Execute Immediate Requests
                try
                {
                    requestAgent.ExecuteImmediateRequests(clientId);
                }
                catch (Exception ex)
                {
                    // Mah Log
                    AriaWindowsLogManager.AriaAddToLog("Aria Request Handler Service", ex.Message, EventLogEntryType.Error);
                    // Mah Log
                    //EventLog.WriteEntry("Aria.Services.RequestHandler", ex.Message, EventLogEntryType.Error);
                }
                #endregion

            };

            #endregion
        }
    }
}