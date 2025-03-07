using System;
using System.Collections;
using System.Threading;
using Aria.DataTypes.ObjectDictionary;
using Aria.DataTypes.RequestHandler;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Data;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.EnterpriseServices.Messaging;
using Aria.Environment;
using Aria.Utilities.ParameterSubstitution;

using Aria.Xml;
using Aria.Reflection;
using System.Diagnostics;
using Aria.DataTypes;
using Aria.DataTypes.Settings;
using System.Data;
using System.IO;
using System.Drawing.Printing;
using System.Collections.Generic;
using Aria.DataTypes.Messaging;


namespace Aria.EnterpriseServices.RequestHandler
{
    /// <summary>
    /// This class responsible on run request depends on condition (Execute Immediate Request, Execute One Time Only Requests... etc)
    /// </summary>
    public class AriaRequestAgent : MarshalByRefObject
    {
        public override object InitializeLifetimeService()
        {
            return null;

        }
        public void ExecuteImmediateRequests(string clientId)
        {

            string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RecurrenceType = @RecurrenceType and Status = @OnHoldStatus ";

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);

            command.Parameters.Add(new AriaDbParameter("RecurrenceType", AriaRequestRecurrenceTypes.Immediate.ToString()));
            command.Parameters.Add(new AriaDbParameter("OnHoldStatus", AriaRequestStatusTypes.OnHold.ToString()));

            AriaDataProvider dataProvider = new AriaDataProvider();

            ArrayList requests = dataProvider.GetObjectList(command, typeof(AriaRequest));

            for (int index = 0; index < requests.Count; index++)
            {
                AriaRequest immediateRequest = (AriaRequest)requests[index];

                immediateRequest.Status = AriaRequestStatusTypes.Running;

                immediateRequest.Occurence++;

                dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", immediateRequest,
                                            new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);


                //InvokeRequestMethod(immediateRequest, clientId);

                AriaThreadManager thread = new AriaThreadManager();
                immediateRequest.clientID = clientId;
                thread.StartThread(immediateRequest, clientId);


            };

        }
        public void ExecuteImmediateRequest(string requestId, string clientId)
        {
            string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);

            command.Parameters.Add(new AriaDbParameter("RequestID", requestId));

            AriaDataProvider dataProvider = new AriaDataProvider();

            ArrayList requests = dataProvider.GetObjectList(command, typeof(AriaRequest));

            AriaRequest immediateRequest = (AriaRequest)requests[0];

            immediateRequest.Status = AriaRequestStatusTypes.Running;

            immediateRequest.Occurence++;

            dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", immediateRequest,
                                        new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);


            //InvokeRequestMethod(immediateRequest, clientId);
            AriaThreadManager thread = new AriaThreadManager();
            immediateRequest.clientID = clientId;
            thread.StartThread(immediateRequest, clientId);


        }

        public void ExecuteOneTimeOnlyRequests(string clientId)
        {
            string AriaDbCommandText = "SELECT * FROM AriaRequest " +
                                            "WHERE StartAfterDate <= @Now " +
                                                "AND Status = @OnHoldStatus " +
                                                    "AND RecurrenceType = @Once ";

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);

            command.Parameters.Add(new AriaDbParameter("Now", DateTime.Now.Date));
            command.Parameters.Add(new AriaDbParameter("OnHoldStatus", AriaRequestStatusTypes.OnHold.ToString()));
            command.Parameters.Add(new AriaDbParameter("Once", AriaRequestRecurrenceTypes.Once.ToString()));

            AriaDataProvider dataProvider = new AriaDataProvider();

            ArrayList requests = dataProvider.GetObjectList(command, typeof(AriaRequest));

            for (int index = 0; index < requests.Count; index++)
            {
                AriaRequest onceRequest = (AriaRequest)requests[index];
                
                AriaDbCommand similarCommand = new AriaDbCommand("SELECT * FROM AriaRequest Where RequestID > @RequestID AND ParentRequestID = @ParentRequestID", connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);

                similarCommand.Parameters.Add(new AriaDbParameter("RequestID", onceRequest.RequestID));
                similarCommand.Parameters.Add(new AriaDbParameter("ParentRequestID", onceRequest.ParentRequestID));

                ArrayList similarRequests = dataProvider.GetObjectList(similarCommand, typeof(AriaRequest));

                if (similarRequests.Count > 0 && (onceRequest.EventObjectName == null || onceRequest.EventObjectName.Trim().Length == 0))
                {
                    onceRequest.Status = AriaRequestStatusTypes.Canceled;

                    dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", onceRequest,
                                                new string[] { "Status", "NextChildRequestDateTime", "Occurence" }, @"RequestId = @RequestId", clientId);
                }
                else
                {
                    if (GetRequestStartAfterDateTime(onceRequest) <= DateTime.Now)
                    {
                        onceRequest.Status = AriaRequestStatusTypes.Running;

                        onceRequest.Occurence++;

                        dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", onceRequest,
                                                    new string[] { "Status", "NextChildRequestDateTime", "Occurence" }, @"RequestId = @RequestId", clientId);

                        AriaThreadManager thread = new AriaThreadManager();
                        onceRequest.clientID = clientId;
                        thread.StartThread(onceRequest, clientId);
                    }
                }
            }
        }

        public void GenerateScheduleRequests(string clientId)
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

            //EventLog.WriteEntry("Aria.Services.RequestHandler.Command", command.CommandText, EventLogEntryType.Information);
            //EventLog.WriteEntry("Aria.Services.RequestHandler.Requests Count", requests.Count.ToString(), EventLogEntryType.Information);

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

                    Xml.AriaXmlSerializer ser = new Aria.Xml.AriaXmlSerializer();
                    AriaRequest newRequest = (AriaRequest)ser.ConvertFromXml(ser.ConvertToXml(request));
                    //scheduler.Schedule(newRequest);

                    
                    //if (newRequest.NextChildRequestDateTime > DateTime.Now)
                    //{
                    //    CloneOneTimeOnlyRequest(request, clientId);
                    //}

                    CloneOneTimeOnlyRequest(request, clientId);

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

        public void GenerateOnComputerStartupRequests(string clientId)
        {

            string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RecurrenceType = @RecurrenceType and Status <> @RemovedStatus ";

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);

            command.Parameters.Add(new AriaDbParameter("RecurrenceType", AriaRequestRecurrenceTypes.ComputerStart.ToString()));
            command.Parameters.Add(new AriaDbParameter("RemovedStatus", AriaRequestRecurrenceTypes.ComputerStart.ToString()));


            AriaDataProvider dataProvider = new AriaDataProvider();

            ArrayList requests = dataProvider.GetObjectList(command, typeof(AriaRequest));

            AriaRequest request = null;
            ArrayList dateTimeList = null;
            AriaRequestScheduler scheduler = new AriaRequestScheduler();

            for (int requestIndex = 0; requestIndex < requests.Count; requestIndex++)
            {
                request = (AriaRequest)requests[requestIndex];

                if (request.Status == AriaRequestStatusTypes.OnHold)
                {
                    request.Status = AriaRequestStatusTypes.Started;
                }

                CloneOneTimeOnlyRequest(request, clientId);

                request.Occurence++;

                dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                            new string[] { "Status", "NextChildRequestDateTime", "Occurence" }, @"RequestId = @RequestId", clientId);
            }
        }

        public void GenerateEventRequests(AriaDbConnection callerConnection, string eventObjectName, string eventName, AriaArgumentList eventArguments, string clientId)
        {
            GetRequestEventArgumentsSettings(eventObjectName, eventName, eventArguments, clientId);

            string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE " +
                                            "RecurrenceType = @RecurrenceType AND " +
                                                "EventObjectName = @EventObjectName AND " +
                                                    "EventName = @EventName And Status <> @RemovedStatus ";

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RecurrenceType", AriaRequestRecurrenceTypes.Event.ToString()));
            command.Parameters.Add(new AriaDbParameter("EventObjectName", eventObjectName));
            command.Parameters.Add(new AriaDbParameter("EventName", eventName));
            command.Parameters.Add(new AriaDbParameter("RemovedStatus", AriaRequestStatusTypes.Removed.ToString()));

            AriaDataProvider dataProvider = new AriaDataProvider();

            ArrayList requests = dataProvider.GetObjectList(command, typeof(AriaRequest));

            for (int requestIndex = 0; requestIndex < requests.Count; requestIndex++)
            {
                AriaRequest request = null;
                request = (AriaRequest)requests[requestIndex];

                if (callerConnection.CompanyName == request.Context.CompanyName)
                {
                    if (request.Status == AriaRequestStatusTypes.OnHold)
                    {
                        request.Status = AriaRequestStatusTypes.Started;
                    }

                    request.Occurence += 1;
                    request.Status = AriaRequestStatusTypes.Started;

                    dataProvider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                                new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);

                    AriaDbConnection connection1 = new AriaDbConnection("Aria", request.Context.CompanyName);
                    connection1.Context = request.Context;
                    connection1.CompanyName = request.Context.CompanyName;
                    connection1.Context.UserName = request.UserName;
                    connection1.Context.MethodName = request.MethodName;

                    eventArguments.BusinessObjectParameterName = "OrderPointer";
                    AriaParameterSubstituter sub = new AriaParameterSubstituter(connection1, eventArguments, clientId);
                    sub.DeepSubstitute(request.EventConditionList, clientId);

                    if (request.EventConditionList.AreConditionsValid())
                    {
                        request.EventArgumentList = eventArguments;


                        AriaRequest onceRequest = CloneOneTimeOnlyRequest(request, DateTime.Now.Date.AddHours(DateTime.Now.Hour)
                                                                                 .AddMinutes(DateTime.Now.Minute)
                                                                                    .AddSeconds(DateTime.Now.Second), clientId);
                    }
                }
            }
        }

        private void CloneOneTimeOnlyRequest(AriaRequest request, string clientId)
        {
            AriaRequest onceRequest = new AriaRequest();

            onceRequest.SetRequestDateRangeAfterOccurence(request.NextChildRequestDateTime, 1);

            onceRequest.Description = request.Description;

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
            onceRequest.ErrorNotification = request.ErrorNotification;

            onceRequest.ParentRequestID = new Guid(request.RequestID).ToString();
            onceRequest.ParentRequestNumber = request.RequestNumber;

            AriaDataProvider dataProvider = new AriaDataProvider();

            AriaDbConnection connection = new AriaDbConnection("Aria", "");

            dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, onceRequest, new string[] { "RequestNumber" }, clientId);

            if (onceRequest.RepeatTask)
            {
                ArrayList dateTimeList = GetRepeatableNextChildDateTimes(onceRequest);

                for (int i = 0; i < dateTimeList.Count; i++)
                {
                    AriaRequest repeatRequest = new AriaRequest();

                    repeatRequest.SetRequestDateRangeAfterOccurence(request.NextChildRequestDateTime, 1);

                    repeatRequest.Description = request.Description;

                    repeatRequest.SetRequestMethodSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList);
                    repeatRequest.SetRequestLoginSettings(request.LoginType, request.UserName, request.Password);
                    repeatRequest.SetOneTimeOnlyRequestSettings(request.RequestStartTime);
                    repeatRequest.SetRequestContextSettings(request.Context.CompanyName, request.Context.CompanyName);

                    repeatRequest.EventObjectName = request.EventObjectName;
                    repeatRequest.EventConditionList = request.EventConditionList;

                    repeatRequest.CompleteNotification = request.CompleteNotification;
                    repeatRequest.ErrorNotification = request.ErrorNotification;

                    repeatRequest.ParentRequestID = new Guid(request.RequestID).ToString();
                    repeatRequest.ParentRequestNumber = request.RequestNumber;

                    repeatRequest.RequestStartTime = (DateTime)dateTimeList[i];

                    repeatRequest.StartAfterDate = repeatRequest.StartAfterDate.AddDays(repeatRequest.RequestStartTime.Subtract(new DateTime(1900, 1, 1)).Days);

                    dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, repeatRequest, new string[] { "RequestNumber" }, clientId);
                }
            }
        }

        private AriaRequest CloneOneTimeOnlyRequest(AriaRequest request, DateTime startAfterDateTime, string clientId)
        {
            AriaRequest onceRequest = new AriaRequest();

            onceRequest.SetRequestDateRangeAfterOccurence(startAfterDateTime, 1);

            onceRequest.Description = request.Description;

            onceRequest.SetRequestLoginSettings(request.LoginType, request.UserName, request.Password);
            onceRequest.SetOnEventRequestSettings(request.EventObjectName, request.EventName, request.EventArgumentList, request.EventConditionList);
            onceRequest.SetRequestMethodSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList);
            onceRequest.SetOneTimeOnlyRequestSettings(new DateTime(1900, 1, 1, startAfterDateTime.Hour, startAfterDateTime.Minute, startAfterDateTime.Second));
            onceRequest.ParentRequestID = new Guid(request.RequestID).ToString();
            onceRequest.ParentRequestNumber = request.RequestNumber;
            onceRequest.SetRequestContextSettings(request.Context.CompanyName, request.Context.CompanyName);

            onceRequest.RepeatTask = request.RepeatTask;
            onceRequest.RepeatTaskEvery = request.RepeatTaskEvery;
            onceRequest.RepeatTaskUntilDuration = request.RepeatTaskUntilDuration;
            onceRequest.RepeatTaskUntillTime = request.RepeatTaskUntillTime;
            onceRequest.RepeatTaskUntilType = request.RepeatTaskUntilType;
            onceRequest.HoldTaskIfStillRunning = request.HoldTaskIfStillRunning;

            onceRequest.CompleteNotification = request.CompleteNotification;
            onceRequest.ErrorNotification = request.ErrorNotification;

            AriaDataProvider dataProvider = new AriaDataProvider();

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            dataProvider.InsertObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, onceRequest, new string[] { "RequestNumber" }, clientId);

            return onceRequest;
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

        public void InvokeRequestMethod(object requestObject, string clientId)
        {
            // MAH Get Request File Name If Exist
            string outputFileName = null;
            bool outputFileNameExist = false;

            AriaRequest request1 = (AriaRequest)requestObject;

            if (request1.MethodArgumentList != null)
            {
                foreach (object obj in request1.MethodArgumentList)
                {
                    AriaArgument arg1 = (AriaArgument)obj;

                    if (arg1.Value is Aria.DataTypes.AriaOptionGridXmlDataSet)
                    {
                        AriaOptionGridXmlDataSet optionGrid1 = arg1.Value as Aria.DataTypes.AriaOptionGridXmlDataSet;
                        System.Xml.XmlDocument optionGridXML1 = new System.Xml.XmlDocument();
                        optionGridXML1.Load(optionGrid1.FileName);
                        outputFileName = optionGridXML1.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText;

                        if (outputFileName == null)
                        {
                            outputFileName = "";
                        }

                        if (outputFileName != null && outputFileName != "" && request1.CompleteNotification != null)
                        {
                            foreach (DictionaryEntry entry in request1.CompleteNotification.Attachment)
                            {
                                if (entry.Value != null && outputFileName.ToString().Trim().ToUpper() == entry.Value.ToString().Trim().ToUpper())
                                {
                                    outputFileNameExist = true;
                                }
                            }
                        }

                        EventLog.WriteEntry("Aria.Services.RequestHandler.Requests.FileName", outputFileName, EventLogEntryType.Information);

                        break;
                    }
                }
            }

            if (!outputFileNameExist)
            {
                outputFileName = null;
            }
            // MAH Get Request File Name If Exist
            
            
            AriaRequest request = (AriaRequest)requestObject;
            //if (string.IsNullOrEmpty(clientId) == true)
            //{ clientId = request.clientID; };

            AriaDbConnection connection = new AriaDbConnection();

            connection.Context = request.Context;
            connection.CompanyName = request.Context.CompanyName;
            connection.Context.UserName = request.UserName;
            connection.Context.MethodName = request.MethodName;

            AriaDataProvider dataProvider = new AriaDataProvider();
            AriaMessagingManager messaging = new AriaMessagingManager();

            try
            {
                # region Old code which handel the request cases
                //if (request.MethodObjectName.Trim().Length == 0)
                //{   // Responce To Event Request Handeling 
                //    if (request.EventName.Trim().Length > 0)
                //    {
                //        request.Status = AriaRequestStatusTypes.Completed;

                //        GetRequestEventArgumentsSettings(request.EventObjectName, request.EventName, request.EventArgumentList, clientId);

                //        if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
                //        {
                //            messaging.SendEmail(connection, request.EventArgumentList, request.CompleteNotification, clientId);
                //        }

                //        request.Occurence += 1;

                //        dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                //                                    new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);
                //    }
                //    else
                //    {
                //        // Agent Request Handeling
                //        request.Status = AriaRequestStatusTypes.Completed;

                //        // Get the data set as pointers to each record, each pointer contins the Primiary key Only
                //        ArrayList list = GetAgentRequests(new AriaDbConnection(request.Context.CustomerName,
                //                                            request.Context.CompanyName), request.EventObjectName, request.EventConditionList, clientId);

                //        AriaObjectDictionaryDBCentric objecDictionary = new AriaObjectDictionaryDBCentric();

                //        AriaDataObjectPointerSettings settings = new AriaDataObjectPointerSettings();
                //        settings.DataObjectName = request.EventObjectName;
                //        settings.DataObjectRevision = objecDictionary.LoadActiveRevision(connection, request.EventObjectName, clientId).ObjectRevision;

                //        for (int index = 0; index < list.Count; index++)
                //        {
                //            if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
                //            {
                //                request.EventArgumentList = new AriaArgumentList();
                //                request.EventArgumentList.AddArgument("Pointer", (AriaDataType)list[index]);
                //                request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                //                ((AriaArgument)request.EventArgumentList[0]).Settings = settings;

                //                messaging.SendEmail(connection, request.EventArgumentList, request.CompleteNotification, clientId);
                //            }
                //        }

                //        request.Occurence += 1;

                //        dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                //                                    new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);
                //    }
                //}
                //else
                //{ // Schedule Request Handeling
                //    AriaObjectDictionaryDBCentric objectDictionaryDBCentric = new AriaObjectDictionaryDBCentric();

                //    AriaObjectRevision objectRevision = objectDictionaryDBCentric.LoadActiveRevision(connection, request.MethodObjectName, clientId);

                //    AriaObjectRevisionSettings serverObjectSettings = objectRevision.ObjectRevisionSettings;

                //    // T20100512.0026 Hassan.I 20-05-2010 [Begin]
                //    //object[] argumentsArray = new object[request.MethodArgumentList.Count + 1];
                //    object[] argumentsArray = new object[request.MethodArgumentList.Count + 2];
                //    // T20100512.0026 Hassan.I 20-05-2010 [End]

                //    argumentsArray[0] = request.RequestID.ToString();

                //    for (int index = 1; index < argumentsArray.Length - 1; index++)
                //    {
                //        argumentsArray[index] = (object)((AriaArgument)request.MethodArgumentList[index - 1]).Value;
                //    }
                //    // T20100512.0026 Hassan.I 20-05-2010 [Begin]
                //    try
                //    { argumentsArray[argumentsArray.Length - 1] = clientId.ToString(); }
                //    catch (Exception ex)
                //    { argumentsArray[argumentsArray.Length - 1] = ""; }



                //    // T20100512.0026 Hassan.I 20-05-2010 [End]

                //    GetRequestMethodArgumentsSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList, clientId);
                //    connection.Context = request.Context;
                //    connection.CompanyName = request.Context.CompanyName;
                //    connection.Context.UserName = request.UserName;
                //    connection.Context.MethodName = request.MethodName;

                //    AriaReflector reflector = new AriaReflector();

                //    argumentsArray[1] = ((Aria.DataTypes.AriaOptionGridXmlDataSet)argumentsArray[1]).FileName;

                //    if (serverObjectSettings is AriaServerObjectSettings)
                //        reflector.ExecuteMethod(((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                //    else if (serverObjectSettings is AriaReportObjectSettings)
                //        reflector.ExecuteMethod(((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                //    string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";

                //    AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                //    command.Parameters.Add(new AriaDbParameter("RequestID", request.RequestID));

                //    AriaDataProvider oldRequestDataProvider = new AriaDataProvider();

                //    AriaRequest oldRequest = (AriaRequest)oldRequestDataProvider.GetObjectList(command, typeof(AriaRequest))[0];
                //    if (oldRequest.Status == AriaRequestStatusTypes.Canceled)
                //    {
                //        return;
                //    }


                //    request.Status = AriaRequestStatusTypes.Completed;

                //    if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
                //    {
                //        messaging.SendEmail(connection, request.MethodArgumentList, request.CompleteNotification, clientId);
                //    }
                //}
                # endregion Old code which handel the request cases

                Boolean RequestCaseHandled = false;

                # region Immediate Request Handeling
                // Responce To Event Request Handeling 
                if (request.RecurrenceType == AriaRequestRecurrenceTypes.Immediate && RequestCaseHandled == false)
                {
                    RequestCaseHandled = true;
                    request.Status = AriaRequestStatusTypes.Running;

                    # region added code to handel the method firing

                    AriaObjectDictionaryDBCentric objectDictionaryDBCentric = new AriaObjectDictionaryDBCentric();
                    AriaObjectRevision objectRevision = objectDictionaryDBCentric.LoadActiveRevision(connection, request.MethodObjectName, clientId);
                    AriaObjectRevisionSettings serverObjectSettings = objectRevision.ObjectRevisionSettings;

                    object[] argumentsArray = new object[request.MethodArgumentList.Count + 2];
                    argumentsArray[0] = request.RequestID.ToString();

                    for (int index = 1; index < argumentsArray.Length - 1; index++)
                    { argumentsArray[index] = (object)((AriaArgument)request.MethodArgumentList[index - 1]).Value; }

                    try
                    { argumentsArray[argumentsArray.Length - 1] = clientId.ToString(); }
                    catch (Exception ex)
                    { argumentsArray[argumentsArray.Length - 1] = ""; }


                    GetRequestMethodArgumentsSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList, clientId);
                    connection.Context = request.Context;
                    connection.CompanyName = request.Context.CompanyName;
                    connection.Context.UserName = request.UserName;
                    connection.Context.MethodName = request.MethodName;

                    AriaReflector reflector = new AriaReflector();

                    argumentsArray[1] = ((Aria.DataTypes.AriaOptionGridXmlDataSet)argumentsArray[1]).FileName;

                    // Handle SAAS Issues
                    try
                    {
                        if (serverObjectSettings is AriaServerObjectSettings)
                            reflector.ExecuteMethod(clientId + "_" + ((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                        else if (serverObjectSettings is AriaReportObjectSettings)
                            reflector.ExecuteMethod(clientId + "_" + ((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                    }
                    catch (Exception)
                    {
                        if (serverObjectSettings is AriaServerObjectSettings)
                            reflector.ExecuteMethod(((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                        else if (serverObjectSettings is AriaReportObjectSettings)
                            reflector.ExecuteMethod(((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                    }


                    string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";

                    AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                    command.Parameters.Add(new AriaDbParameter("RequestID", request.RequestID));

                    AriaDataProvider oldRequestDataProvider = new AriaDataProvider();

                    AriaRequest oldRequest = (AriaRequest)oldRequestDataProvider.GetObjectList(command, typeof(AriaRequest))[0];
                    if (oldRequest.Status == AriaRequestStatusTypes.Canceled)
                    {
                        return;
                    }

                    if (oldRequest.Status == AriaRequestStatusTypes.Failed)
                    {
                        if (oldRequest.ErrorNotification != null && oldRequest.ErrorNotification.IsValid())
                        {
                            messaging.SendEmail(connection, oldRequest.MethodArgumentList, oldRequest.ErrorNotification, clientId);
                        }

                        return;
                    }
                    # endregion

                    request.Status = AriaRequestStatusTypes.Completed;

                    if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
                    {
                        if(outputFileName == null || System.IO.File.Exists(outputFileName))
                        {
                            messaging.SendEmail(connection, request.MethodArgumentList, GetRequest(request.RequestID, clientId).CompleteNotification, clientId);
                        }
                    }

                    request.Occurence++;
                    dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                                new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);

                }
                # endregion Immediate Request Handeling

                # region Responce To Event Request Handeling
                // Responce To Event Request Handeling 
                if (request.EventName.Trim().Length > 0 && RequestCaseHandled == false)
                {
                    RequestCaseHandled = true;

                    # region added code to handel the method firing
                    if (string.IsNullOrEmpty(request.MethodObjectName) == false)
                    {
                        AriaObjectDictionaryDBCentric objectDictionaryDBCentric = new AriaObjectDictionaryDBCentric();
                        AriaObjectRevision objectRevision = objectDictionaryDBCentric.LoadActiveRevision(connection, request.MethodObjectName, clientId);
                        AriaObjectRevisionSettings serverObjectSettings = objectRevision.ObjectRevisionSettings;

                        object[] argumentsArray = new object[request.MethodArgumentList.Count + 2];
                        argumentsArray[0] = request.RequestID.ToString();

                        for (int index = 1; index < argumentsArray.Length - 1; index++)
                        { argumentsArray[index] = (object)((AriaArgument)request.MethodArgumentList[index - 1]).Value; }

                        try
                        { argumentsArray[argumentsArray.Length - 1] = clientId.ToString(); }
                        catch (Exception ex)
                        { argumentsArray[argumentsArray.Length - 1] = ""; }


                        GetRequestMethodArgumentsSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList, clientId);
                        connection.Context = request.Context;
                        connection.CompanyName = request.Context.CompanyName;
                        connection.Context.UserName = request.UserName;
                        connection.Context.MethodName = request.MethodName;

                        AriaReflector reflector = new AriaReflector();

                        argumentsArray[1] = ((Aria.DataTypes.AriaOptionGridXmlDataSet)argumentsArray[1]).FileName;

                        try
                        {
                            if (serverObjectSettings is AriaServerObjectSettings)
                                reflector.ExecuteMethod(clientId + "_" + ((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                            else if (serverObjectSettings is AriaReportObjectSettings)
                                reflector.ExecuteMethod(clientId + "_" + ((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                        }
                        catch (Exception)
                        {
                            if (serverObjectSettings is AriaServerObjectSettings)
                                reflector.ExecuteMethod(((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                            else if (serverObjectSettings is AriaReportObjectSettings)
                                reflector.ExecuteMethod(((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                        }

                        string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";

                        AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                        command.Parameters.Add(new AriaDbParameter("RequestID", request.RequestID));

                        AriaDataProvider oldRequestDataProvider = new AriaDataProvider();

                        AriaRequest oldRequest = (AriaRequest)oldRequestDataProvider.GetObjectList(command, typeof(AriaRequest))[0];
                        if (oldRequest.Status == AriaRequestStatusTypes.Canceled)
                        {
                            return;
                        }

                        if (oldRequest.Status == AriaRequestStatusTypes.Failed)
                        {
                            if (oldRequest.ErrorNotification != null && oldRequest.ErrorNotification.IsValid())
                            {
                                messaging.SendEmail(connection, oldRequest.MethodArgumentList, oldRequest.ErrorNotification, clientId);
                            }

                            return;
                        }
                    }

                    # endregion

                    request.Status = AriaRequestStatusTypes.Completed;

                    GetRequestEventArgumentsSettings(request.EventObjectName, request.EventName, request.EventArgumentList, clientId);

                    if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
                    {
                        if (outputFileName == null || System.IO.File.Exists(outputFileName))
                        {
                            messaging.SendEmail(connection, request.EventArgumentList, GetRequest(request.RequestID, clientId).CompleteNotification, clientId);
                        }
                    }

                    request.Occurence += 1;

                    dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                                new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);
                }
                # endregion Responce To Event Request Handeling

                # region Agent Request Handeling
                // Agent Request Handeling 
                // if (request.EventName.Trim().Length == 0 && request.EventConditionList != null && request.EventConditionList.Items.Count > 0 && RequestCaseHandled == false)

                if (request.EventName.Trim().Length == 0 && request.EventObjectName.Length !=0 &&  RequestCaseHandled == false)
                {
                    RequestCaseHandled = true;
                    request.Status = AriaRequestStatusTypes.Completed;

                    // Get the data set as pointers to each record, each pointer contins the Primiary key Only
                    ArrayList list = GetAgentRequests(new AriaDbConnection(request.Context.CustomerName,
                                                        request.Context.CompanyName), request.EventObjectName, request.EventConditionList, clientId);

                    
                    // MAHMAHMAH
                    AriaEnviromentVariables env = new AriaEnviromentVariables();
                    if (env.AriaMaxRecordsPerAgent != 0 && list.Count > env.AriaMaxRecordsPerAgent)
                    {
                        throw new Exception("Agent request has exceeded the maximum number of allowed records '" + env.AriaMaxRecordsPerAgent.ToString() + "'.");
                    }
                    // MAHMAHMAH

                    AriaObjectDictionaryDBCentric objecDictionary = new AriaObjectDictionaryDBCentric();

                    AriaDataObjectPointerSettings settings = new AriaDataObjectPointerSettings();
                    settings.DataObjectName = request.EventObjectName;
                    settings.DataObjectRevision = objecDictionary.LoadActiveRevision(connection, request.EventObjectName, clientId).ObjectRevision;

                    for (int index = 0; index < list.Count; index++)
                    {

                        # region added code to handel the method firing
                        //MOH  t20100929.0001 11-10-2010 Start
                        Aria.DataTypes.AriaOptionGridXmlDataSet optionGrid = null;
                        string OldFile = null, NewOutPutFile = null, OldOutPutFile = null;
                        bool ReplaceAgentValues = false;
                        if (request.MethodObjectName != null && request.MethodObjectName.Trim() != "")
                        {
                            //MOH  t20100929.0001 11-10-2010 End
                            AriaObjectDictionaryDBCentric objectDictionaryDBCentric = new AriaObjectDictionaryDBCentric();
                            AriaObjectRevision objectRevision = objectDictionaryDBCentric.LoadActiveRevision(connection, request.MethodObjectName, clientId);
                            AriaObjectRevisionSettings serverObjectSettings = objectRevision.ObjectRevisionSettings;

                            object[] argumentsArray = new object[request.MethodArgumentList.Count + 2];
                            argumentsArray[0] = request.RequestID.ToString();

                            for (int intindex = 1; intindex < argumentsArray.Length - 1; intindex++)
                            { argumentsArray[intindex] = (object)((AriaArgument)request.MethodArgumentList[intindex - 1]).Value; }

                            try
                            { argumentsArray[argumentsArray.Length - 1] = clientId.ToString(); }
                            catch (Exception ex)
                            { argumentsArray[argumentsArray.Length - 1] = ""; }

                            //MOH T20100226.0004 Start
                            foreach (object obj in argumentsArray)
                            {
                                if (obj is Aria.DataTypes.AriaOptionGridXmlDataSet)
                                {
                                    optionGrid = obj as Aria.DataTypes.AriaOptionGridXmlDataSet;
                                    System.Xml.XmlDocument optionGridXML = new System.Xml.XmlDocument();
                                    optionGridXML.Load(optionGrid.FileName);
                                    System.Xml.XmlNode lcRpExp = optionGridXML.SelectSingleNode(@"//*/row[Name='lcrpExp']")["Value"];
                                    if (lcRpExp != null && lcRpExp.InnerText != null && lcRpExp.InnerText.Contains("{") && lcRpExp.InnerText.Contains("}"))
                                    {
                                        ReplaceAgentValues = true;
                                        string oldvalue = lcRpExp.InnerText.Substring(lcRpExp.InnerText.IndexOf("{") + 1, lcRpExp.InnerText.IndexOf("}") - lcRpExp.InnerText.IndexOf("{") - 1);
                                        string NewValue = "", PointerValue = "", PointerName = "", PointerType = "";
                                        int PointerLength = 0;
                                        if (list[index] is Aria.DataTypes.AriaDataObjectPointer)
                                        {
                                            Aria.DataTypes.AriaDataObjectPointer keyvalue = list[index] as Aria.DataTypes.AriaDataObjectPointer;
                                            foreach (object objpointer in keyvalue.KeyFields)
                                            {
                                                Aria.DataTypes.AriaDataObjectPointerKeyField pointer = objpointer as Aria.DataTypes.AriaDataObjectPointerKeyField;
                                                if (pointer.FieldName.ToUpper() == oldvalue.ToUpper())
                                                {
                                                    PointerValue = pointer.Value.ToString();
                                                    PointerName = pointer.FieldName;
                                                    if (pointer.Value is string)
                                                        PointerType = "C";
                                                    else if (pointer.Value is int || pointer.Value is double || pointer.Value is decimal || pointer.Value is float)
                                                    {
                                                        PointerType = "N";
                                                    }
                                                    else if (pointer.Value is bool || pointer.Value.ToString() == ".T." || pointer.Value.ToString() == ".F.")
                                                        PointerType = "L";
                                                    PointerLength = PointerValue.Length;
                                                    break;
                                                }
                                            }
                                            NewValue = PointerValue;
                                        }
                                        lcRpExp.InnerText = lcRpExp.InnerText.Replace("{" + oldvalue + "}", "'" + NewValue + "'");
                                        AriaObjectRevision rootObjectRevision = (AriaObjectRevision)objecDictionary.LoadActiveRevision(connection, request.EventObjectName, clientId);
                                        string table = ((AriaDataObjectSettings)rootObjectRevision.ObjectRevisionSettings).TableName + "." + PointerName;
                                        System.Xml.XmlNode tablenode = optionGridXML.SelectSingleNode(@"//*/row[starts-with(translate(Value,'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ'),'" + table.ToUpper() + "')]");
                                        string laOGFxFltname = tablenode["Name"].InnerText.Replace(",1]", ",6]");
                                        System.Xml.XmlNode laOGFxFlt = optionGridXML.SelectSingleNode(@"//*/row[Name='" + laOGFxFltname + "']")["Value"];
                                        if (laOGFxFlt != null)
                                        {
                                            NewValue = "";
                                            while (NewValue.Length < 8)
                                                NewValue += Path.GetRandomFileName().Replace(".", "");
                                            NewValue = RandomString(8);
                                            //NewValue = NewValue.Length > 8 ? NewValue.Remove(8) : NewValue;
                                            laOGFxFlt.InnerText = NewValue;
                                            System.Xml.XmlNode NewNode = optionGridXML.CreateNode(System.Xml.XmlNodeType.Element, "row", "");
                                            string xmlvalue = "";
                                            xmlvalue = @"<DataType>System.Table</DataType>";
                                            xmlvalue += @"<Name>" + NewValue + "</Name>";
                                            xmlvalue += @"<Value></Value>";
                                            xmlvalue += @"<CursorStrucutre>";
                                            xmlvalue += @"<Field>";
                                            xmlvalue += @"<Name>KEYEXP</Name>";
                                            xmlvalue += @"<Type>" + PointerType + "</Type>";
                                            xmlvalue += @"<Width>" + PointerLength + "</Width>";
                                            xmlvalue += @"<Decimals>0</Decimals>";
                                            xmlvalue += @"</Field>";
                                            xmlvalue += @"<Field>";
                                            xmlvalue += @"<Name>" + PointerName + "</Name>";
                                            xmlvalue += @"<Type>" + PointerType + "</Type>";
                                            xmlvalue += @"<Width>" + PointerLength + "</Width>";
                                            xmlvalue += @"<Decimals>0</Decimals>";
                                            xmlvalue += @"</Field>";
                                            xmlvalue += @"</CursorStrucutre>";
                                            NewNode.InnerXml = xmlvalue;
                                            NewNode["Value"].InnerText = "<?xml version=\"1.0\"?> <xdoc> <" + NewValue + "> <row> <keyexp>" + PointerValue + "</keyexp> <" + PointerName + ">" + PointerValue + "</" + PointerName + "> </row> </" + NewValue + "> </xdoc>";
                                            optionGridXML.SelectSingleNode("//Parameters").AppendChild(NewNode);
                                        }
                                        string NewName = Path.GetRandomFileName().Replace(".", "") + ".xml";
                                        OldOutPutFile = optionGridXML.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText;
                                        NewOutPutFile = Path.GetDirectoryName(OldOutPutFile) + "\\" + NewName.Replace(".xml", Path.GetExtension(OldOutPutFile));
                                        optionGridXML.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText = NewOutPutFile;

                                        NewName = Path.GetDirectoryName(((Aria.DataTypes.AriaOptionGridXmlDataSet)obj).FileName) + "\\" + NewName;
                                        optionGridXML.Save(NewName);
                                        OldFile = optionGrid.FileName;
                                        optionGrid.FileName = NewName;
                                        break;
                                    }
                                }
                            }

                            // MOH T20100226.0004 End

                            GetRequestMethodArgumentsSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList, clientId);
                            connection.Context = request.Context;
                            connection.CompanyName = request.Context.CompanyName;
                            connection.Context.UserName = request.UserName;
                            connection.Context.MethodName = request.MethodName;

                            AriaReflector reflector = new AriaReflector();

                            argumentsArray[1] = ((Aria.DataTypes.AriaOptionGridXmlDataSet)argumentsArray[1]).FileName;

                            try
                            {
                                if (serverObjectSettings is AriaServerObjectSettings)
                                    reflector.ExecuteMethod(clientId + "_" + ((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                                else if (serverObjectSettings is AriaReportObjectSettings)
                                    reflector.ExecuteMethod(clientId + "_" + ((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                            }
                            catch (Exception)
                            {
                                if (serverObjectSettings is AriaServerObjectSettings)
                                    reflector.ExecuteMethod(((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                                else if (serverObjectSettings is AriaReportObjectSettings)
                                    reflector.ExecuteMethod(((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                            }

                            string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";

                            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                            command.Parameters.Add(new AriaDbParameter("RequestID", request.RequestID));

                            AriaDataProvider oldRequestDataProvider = new AriaDataProvider();

                            AriaRequest oldRequest = (AriaRequest)oldRequestDataProvider.GetObjectList(command, typeof(AriaRequest))[0];
                            if (oldRequest.Status == AriaRequestStatusTypes.Canceled)
                            {
                                return;
                            }

                            if (oldRequest.Status == AriaRequestStatusTypes.Failed)
                            {
                                if (oldRequest.ErrorNotification != null && oldRequest.ErrorNotification.IsValid())
                                {
                                    messaging.SendEmail(connection, oldRequest.MethodArgumentList, oldRequest.ErrorNotification, clientId);
                                }

                                return;
                            }
                            //MOH  t20100929.0001 11-10-2010 Start
                        }
                        
                        ////MOH  t20100929.0001 11-10-2010 End
                        # endregion

                        //// MOH T20100226.0004 Start
                        //if (ReplaceAgentValues && request.CompleteNotification != null && NewOutPutFile != null && NewOutPutFile.Trim() != "")
                        //{
                        //    if (request.CompleteNotification.Attachment.ContainsKey(OldOutPutFile))
                        //    {
                        //        request.CompleteNotification.Attachment.Add(NewOutPutFile, request.CompleteNotification.Attachment[OldOutPutFile]);
                        //        request.CompleteNotification.Attachment.Remove(OldOutPutFile);
                        //    }
                        //}
                        //// MOH T20100226.0004 End

                        //if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
                        //{

                        //    request.EventArgumentList = new AriaArgumentList();
                        //    request.EventArgumentList.AddArgument("Pointer", (AriaDataType)list[index]);
                        //    request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                        //    ((AriaArgument)request.EventArgumentList[0]).Settings = settings;

                        //    messaging.SendEmail(connection, request.EventArgumentList, GetRequest(request.RequestID, clientId).CompleteNotification, clientId);
                        //}

                        //// MOH T20100226.0004 Start
                        //if (ReplaceAgentValues && !string.IsNullOrEmpty(OldFile) && OldFile.Trim() != "" && optionGrid != null)
                        //{
                        //    optionGrid.FileName = OldFile;
                        //}
                        //if (ReplaceAgentValues && request.CompleteNotification != null && NewOutPutFile != null && NewOutPutFile.Trim() != "")
                        //{
                        //    if (request.CompleteNotification.Attachment.ContainsKey(NewOutPutFile))
                        //    {
                        //        request.CompleteNotification.Attachment.Add(OldOutPutFile, request.CompleteNotification.Attachment[NewOutPutFile]);
                        //        request.CompleteNotification.Attachment.Remove(NewOutPutFile);
                        //    }
                        //}
                        //// MOH T20100226.0004 End



                        AriaEmail agentEmail = GetRequest(request.RequestID, clientId).CompleteNotification;

                        EventLog.WriteEntry("Aria.Services.RequestHandler.Agent", "Before1", EventLogEntryType.Information);
                        EventLog.WriteEntry("Aria.Services.RequestHandler.Agent", ReplaceAgentValues.ToString(), EventLogEntryType.Information);

                        if (ReplaceAgentValues)
                        {
                            EventLog.WriteEntry("Aria.Services.RequestHandler.Agent", "NewOutPutFile:" + NewOutPutFile, EventLogEntryType.Information);
                            EventLog.WriteEntry("Aria.Services.RequestHandler.Agent", "OldOutPutFile:" + OldOutPutFile, EventLogEntryType.Information);
                            

                            if (NewOutPutFile != null && NewOutPutFile.Trim() != "")
                            {
                                bool containOld = false;
                                if (agentEmail.Attachment.ContainsKey(OldOutPutFile))
                                {
                                    agentEmail.Attachment.Remove(OldOutPutFile);
                                    containOld = true;
                                    EventLog.WriteEntry("Aria.Services.RequestHandler.Agent", "Delete", EventLogEntryType.Information);
                                    EventLog.WriteEntry("Aria.Services.RequestHandler.Agent", "Count" + agentEmail.Attachment.Count.ToString(), EventLogEntryType.Information);
                                }

                                agentEmail.Attachment[NewOutPutFile] = NewOutPutFile;

                                EventLog.WriteEntry("Aria.Services.RequestHandler.Agent", "Add" + NewOutPutFile, EventLogEntryType.Information);
                                EventLog.WriteEntry("Aria.Services.RequestHandler.Agent", "Count" + agentEmail.Attachment.Count.ToString(), EventLogEntryType.Information);

                                try
                                {
                                    if (agentEmail != null && agentEmail.IsValid())
                                    {
                                        request.EventArgumentList = new AriaArgumentList();
                                        request.EventArgumentList.AddArgument("Pointer", (AriaDataType)list[index]);
                                        request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                                        ((AriaArgument)request.EventArgumentList[0]).Settings = settings;
                                    }

                                    if (NewOutPutFile == null || System.IO.File.Exists(NewOutPutFile))
                                    {
                                        messaging.SendEmail(connection, request.EventArgumentList, agentEmail, clientId);
                                    }
                                }
                                catch (Exception ex)
                                {
                                    EventLog.WriteEntry("Aria.Services.RequestHandler.Agent", ex.GetBaseException().Message, EventLogEntryType.Information);
                                    throw ex;
                                }

                                EventLog.WriteEntry("Aria.Services.RequestHandler.Agent", "Finish", EventLogEntryType.Information);

                                agentEmail.Attachment.Remove(NewOutPutFile);

                                if (containOld)
                                {
                                    agentEmail.Attachment.Add(OldOutPutFile, OldOutPutFile);
                                }
                            }
                            else
                            {
                                if (agentEmail != null && agentEmail.IsValid())
                                {
                                    request.EventArgumentList = new AriaArgumentList();
                                    request.EventArgumentList.AddArgument("Pointer", (AriaDataType)list[index]);
                                    request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                                    ((AriaArgument)request.EventArgumentList[0]).Settings = settings;
                                }

                                if (outputFileName == null || System.IO.File.Exists(outputFileName))
                                {
                                    messaging.SendEmail(connection, request.EventArgumentList, agentEmail, clientId);
                                }
                            }
                        }
                        else
                        {
                            if (agentEmail != null && agentEmail.IsValid())
                            {
                                request.EventArgumentList = new AriaArgumentList();
                                request.EventArgumentList.AddArgument("Pointer", (AriaDataType)list[index]);
                                request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                                ((AriaArgument)request.EventArgumentList[0]).Settings = settings;
                            }

                            if (outputFileName == null || System.IO.File.Exists(outputFileName))
                            {
                                messaging.SendEmail(connection, request.EventArgumentList, agentEmail, clientId);
                            }
                        }

                        if (ReplaceAgentValues && !string.IsNullOrEmpty(OldFile) && OldFile.Trim() != "" && optionGrid != null)
                        {
                            optionGrid.FileName = OldFile;
                        }
                    }

                    request.Occurence += 1;

                    dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                                new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);

                };
                # endregion Agent Request Handeling

                # region Schedule Request Handeling
                // Schedule Request Handeling 
                
                
                //Aria.DataTypes.RequestHandler.AriaRequest parentRequest = new Aria.DataTypes.RequestHandler.AriaRequest ();
                //parentRequest = proxy.GetRequest(request.ParentRequestID, clientId);


                //if (request.EventName.Trim().Length == 0 && ( (request.EventConditionList == null ) || (request.EventConditionList.Items.Count < 0) ) && request.DailyType != AriaRequestDailyTypes.NotSet && RequestCaseHandled == false)
                if (request.EventName.Trim().Length == 0 && ((request.EventConditionList == null) || (request.EventConditionList.Items.Count < 0)) && RequestCaseHandled == false)
                {
                    RequestCaseHandled = true;
                    AriaObjectDictionaryDBCentric objectDictionaryDBCentric = new AriaObjectDictionaryDBCentric();
                    AriaObjectRevision objectRevision = objectDictionaryDBCentric.LoadActiveRevision(connection, request.MethodObjectName, clientId);
                    AriaObjectRevisionSettings serverObjectSettings = objectRevision.ObjectRevisionSettings;



                    // T20100512.0026 Hassan.I 20-05-2010 [Begin]
                    //object[] argumentsArray = new object[request.MethodArgumentList.Count + 1];
                    object[] argumentsArray = new object[request.MethodArgumentList.Count + 2];
                    // T20100512.0026 Hassan.I 20-05-2010 [End]

                    argumentsArray[0] = request.RequestID.ToString();

                    for (int index = 1; index < argumentsArray.Length - 1; index++)
                    {
                        argumentsArray[index] = (object)((AriaArgument)request.MethodArgumentList[index - 1]).Value;
                    }
                    // T20100512.0026 Hassan.I 20-05-2010 [Begin]
                    try
                    { argumentsArray[argumentsArray.Length - 1] = clientId.ToString(); }
                    catch (Exception ex)
                    { argumentsArray[argumentsArray.Length - 1] = ""; }



                    // T20100512.0026 Hassan.I 20-05-2010 [End]

                    GetRequestMethodArgumentsSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList, clientId);
                    connection.Context = request.Context;
                    connection.CompanyName = request.Context.CompanyName;
                    connection.Context.UserName = request.UserName;
                    connection.Context.MethodName = request.MethodName;

                    AriaReflector reflector = new AriaReflector();

                    argumentsArray[1] = ((Aria.DataTypes.AriaOptionGridXmlDataSet)argumentsArray[1]).FileName;

                    try
                    {
                        if (serverObjectSettings is AriaServerObjectSettings)
                            reflector.ExecuteMethod(clientId + "_" + ((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                        else if (serverObjectSettings is AriaReportObjectSettings)
                            reflector.ExecuteMethod(clientId + "_" + ((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                    }
                    catch (Exception)
                    {
                        if (serverObjectSettings is AriaServerObjectSettings)
                            reflector.ExecuteMethod(((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                        else if (serverObjectSettings is AriaReportObjectSettings)
                            reflector.ExecuteMethod(((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                    }

                    string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";

                    AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                    command.Parameters.Add(new AriaDbParameter("RequestID", request.RequestID));

                    AriaDataProvider oldRequestDataProvider = new AriaDataProvider();

                    AriaRequest oldRequest = (AriaRequest)oldRequestDataProvider.GetObjectList(command, typeof(AriaRequest))[0];
                    if (oldRequest.Status == AriaRequestStatusTypes.Canceled)
                    {
                        return;
                    }

                    if (oldRequest.Status == AriaRequestStatusTypes.Failed)
                    {
                        if (oldRequest.ErrorNotification != null && oldRequest.ErrorNotification.IsValid())
                        {
                            messaging.SendEmail(connection, oldRequest.MethodArgumentList, oldRequest.ErrorNotification, clientId);
                        }

                        return;
                    }

                    request.Status = AriaRequestStatusTypes.Completed;

                    if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
                    {
                        if (outputFileName == null || System.IO.File.Exists(outputFileName))
                        {
                            messaging.SendEmail(connection, request.MethodArgumentList, GetRequest(request.RequestID, clientId).CompleteNotification, clientId);
                        }
                    }

                }
                # endregion Schedule Request Handeling







            }
            catch (Exception ex)
            {
                request.Error = ex;

                request.Status = AriaRequestStatusTypes.Failed;

                if (request.ErrorNotification != null && request.ErrorNotification.IsValid())
                {
                    if (request.EventObjectName != null && request.EventObjectName.Trim().Length != 0)
                    {
                        messaging.SendEmail(connection, request.EventArgumentList, GetRequest(request.RequestID, clientId).ErrorNotification, clientId);
                    }
                    else
                    {
                        messaging.SendEmail(connection, request.MethodArgumentList, GetRequest(request.RequestID, clientId).ErrorNotification, clientId);
                    }
                }
            }

            if (request.Error == null)
            {
                dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                            new string[] { "Status", "NextChildRequestDateTime", "Occurence" }, @"RequestId = @RequestId", clientId);
            }
            else
            {
                dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                            new string[] { "Status", "NextChildRequestDateTime", "Occurence", "Error" }, @"RequestId = @RequestId AND Status = 'Running'", clientId);
            }
        }

        public void UpdateObjectProgress(string requestId, AriaRequestProgress progress, string clientId)
        {
            string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RequestID", requestId));

            AriaDataProvider dataProvider = new AriaDataProvider();
            AriaRequest request = (AriaRequest)dataProvider.GetObjectList(command, typeof(AriaRequest))[0];

            if (request.Status == AriaRequestStatusTypes.Running)
            {
                request.Progress = progress;

                connection = new AriaDbConnection("Aria", "");
                dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                            new string[] { "Progress" }, @"RequestId = @RequestId", clientId);
            }
        }

        public void UpdateRequestStatus(string requestId, AriaRequestStatusTypes status, string error, string clientId)
        {
            string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RequestID", requestId));

            AriaDataProvider dataProvider = new AriaDataProvider();
            AriaRequest request = (AriaRequest)dataProvider.GetObjectList(command, typeof(AriaRequest))[0];

            if (error != null && error.Trim() != "")
            {
                request.Error = new Exception(error);
            }

            request.Status = status;

            connection = new AriaDbConnection("Aria", "");
            dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                new string[] { "Status", "Error" }, @"RequestId = @RequestId", clientId);
        }

        public void GetRequestMethodArgumentsSettings(string objectName, string methodName, AriaArgumentList arguments, string clientId)
        {
            AriaObjectDictionaryDBCentric dbCentric = new AriaObjectDictionaryDBCentric();

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            string objectRevision = dbCentric.LoadActiveRevision(connection, objectName, clientId).ObjectRevision;

            for (int index = 0; index < arguments.Count; index++)
            {
                AriaArgument argument = (AriaArgument)arguments[index];

                argument.Settings = ((AriaObjectMethodParameter)dbCentric.LoadAriaObjectMethodParameters(connection, objectName, objectRevision, methodName, clientId)[0]).ParameterSettings;
            }

            arguments.BusinessObjectParameterName = dbCentric.LoadAriaObjectMethod(connection, objectName, objectRevision, methodName, clientId).BusinessObjectParameterName;
        }

        public void GetRequestEventArgumentsSettings(string objectName, string eventName, AriaArgumentList arguments, string clientId)
        {
            AriaObjectDictionaryDBCentric dbCentric = new AriaObjectDictionaryDBCentric();

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            string objectRevision = dbCentric.LoadActiveRevision(connection, objectName, clientId).ObjectRevision;

            for (int index = 0; index < arguments.Count; index++)
            {
                AriaArgument argument = (AriaArgument)arguments[index];

                argument.Settings = ((AriaObjectEventParameter)dbCentric.LoadAriaObjectEventParameters(connection, objectName, objectRevision, eventName, clientId)[0]).ParameterSettings;
            }

            arguments.BusinessObjectParameterName = dbCentric.LoadAriaObjectEvent(connection, objectName, objectRevision, eventName, clientId).BusinessObjectParameterName;

        }

        private DateTime GetRequestStartAfterDateTime(AriaRequest request)
        {
            return request.StartAfterDate.AddHours(request.RequestStartTime.Hour).
                                                    AddMinutes(request.RequestStartTime.Minute).
                                                        AddSeconds(request.RequestStartTime.Second);
        }
        
        private string GetDataPathFieldName(string DataPath, string clientId)
        {
            string parentDataPath = DataPath.Substring(0, DataPath.LastIndexOf('.'));
            string propertName = DataPath.Substring(DataPath.LastIndexOf('.') + 1);

            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaObjectRevision objectRevision = (AriaObjectRevision)objectDictionary.LoadActiveRevision(new AriaDbConnection("", ""), parentDataPath, clientId);
            AriaObjectProperty property = objectDictionary.LoadAriaObjectProperty(new AriaDbConnection("", ""), parentDataPath, objectRevision.ObjectRevision, propertName, clientId);

            // T20110803.0001 MAH 8/2/2011
            //if (property.PropertyType == AriaDataTypes.AriaField)
            //{
            //    if (((AriaFieldSettings)property.PropertySettings).FieldName.ToUpper().TrimEnd() == "ORDER")
            //    {
            //        return "[ORDER]";
            //    }
            //    else
            //    {
            //        return ((AriaFieldSettings)property.PropertySettings).FieldName;
            //    }
            //}
            //else
            //{
            //    if (((AriaRelatedFieldSettings)property.PropertySettings).FieldName.ToUpper().TrimEnd() == "ORDER")
            //    {
            //        return "[ORDER]";
            //    }
            //    else
            //    {
            //        return ((AriaRelatedFieldSettings)property.PropertySettings).FieldName;
            //    }
            //}
            string fieldName = "";

            if (property.PropertyType == AriaDataTypes.AriaField)
            {
                fieldName = ((AriaFieldSettings)property.PropertySettings).FieldName;
            }
            else
            {
                fieldName =((AriaRelatedFieldSettings)property.PropertySettings).FieldName;
            }

            if (fieldName.IndexOf("(") > 0
                || fieldName.IndexOf("+") > 0
                || fieldName.IndexOf("-") > 0
                || fieldName.IndexOf("/") > 0
                || fieldName.IndexOf("*") > 0)
            {
                if (fieldName.Contains("|")) fieldName = fieldName.Split('|')[1];
            }
            else
            {
                fieldName = "[" + parentDataPath + "].[" + fieldName + "]";
            }

            return fieldName;
            // T20110803.0001 MAH 8/2/2011 End
        }

        private string GetCondition(AriaDbCommand command, int conditionNo, string objectName, AriaCondition condition, string clientId)
        {

            string result = "(" + (condition.IsOperator ? "" : " NOT ") + "(";
            string cTable;
            // T20110803.0001 MAH 8/2/2011
            // cTable = condition.LeftHandSide.PropertyDataPathDictionary["Value"].ToString();
            // cTable = cTable.Contains(".") ? cTable.Remove(cTable.IndexOf(".")) : "";
            //cTable = "[" + objectName.Trim().Substring(0, objectName.Trim().LastIndexOf('.')) + "]";
            cTable = objectName.Trim();
            // T20110803.0001 MAH 8/2/2011 End

            //[Modify]Ahmed Maher -Date: 09/03/2010 -tkt#T20100301.0004
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            AriaObjectRevision rootObjectRevision = (AriaObjectRevision)objectDictionary.LoadActiveRevision(new AriaDbConnection("", ""), objectName, clientId);
            AriaDataObjectSettings rootObjectRevisionSettings = (AriaDataObjectSettings)rootObjectRevision.ObjectRevisionSettings;

            // T20110803.0001 MAH 8/2/2011
            ////result += GetDataPathFieldName(objectName + "." + condition.LeftHandSide.PropertyDataPathDictionary["Value"].ToString());
            //string FieldNamex = GetDataPathFieldName(objectName + "." + condition.LeftHandSide.PropertyDataPathDictionary["Value"].ToString(), clientId);
            //if (FieldNamex.IndexOf("(") > 0
            //    || FieldNamex.IndexOf("+") > 0
            //    || FieldNamex.IndexOf("-") > 0
            //    || FieldNamex.IndexOf("/") > 0
            //    || FieldNamex.IndexOf("*") > 0
            //    || FieldNamex.Trim().IndexOf(" ") > 0)
            //// T20110803.0001 MAH 8/2/2011
            //// result += GetDataPathFieldName(objectName + "." + condition.LeftHandSide.PropertyDataPathDictionary["Value"].ToString(), clientId);
            //{
            //    string fieldName = GetDataPathFieldName(objectName + "." + condition.LeftHandSide.PropertyDataPathDictionary["Value"].ToString(), clientId);
            //    if (fieldName.Contains("|")) fieldName = fieldName.Split('|')[1];
            //        result += fieldName;
            //}
            //// T20110803.0001 MAH 8/2/2011 End
            //else
            //    if (string.IsNullOrEmpty(cTable))
            //    { result += rootObjectRevisionSettings.TableName + "." + GetDataPathFieldName(objectName + "." + condition.LeftHandSide.PropertyDataPathDictionary["Value"].ToString(), clientId); }
            //    else
            //    {
            //        // T20110803.0001 MAH 8/2/2011
            //        // result += cTable.ToString() + "." + GetDataPathFieldName(objectName + "." + condition.LeftHandSide.PropertyDataPathDictionary["Value"].ToString(), clientId);
            //        string path1 = objectName + "." + condition.LeftHandSide.PropertyDataPathDictionary["Value"].ToString();

            //        result += "[" + path1.Trim().Substring(0, path1.Trim().LastIndexOf('.')) + "].[" + GetDataPathFieldName(objectName + "." + condition.LeftHandSide.PropertyDataPathDictionary["Value"].ToString(), clientId) + "]";
                    
            //        // T20110803.0001 MAH 8/2/2011 End
            //    };
            ////[END]
            
            result += GetDataPathFieldName(objectName + "." + condition.LeftHandSide.PropertyDataPathDictionary["Value"].ToString(), clientId);
            // T20110803.0001 MAH 8/2/2011 End

            switch (condition.Operator)
            {
                case AriaConditionOperators.Like:
                    result += " = ";
                    break;

                case AriaConditionOperators.Contains:
                    result += " LIKE ";
                    break;

                case AriaConditionOperators.GreaterOrEqual:
                    result += " >= ";
                    break;

                case AriaConditionOperators.GreaterThan:
                    result += " > ";
                    break;

                case AriaConditionOperators.LessOrEqual:
                    result += " <= ";
                    break;

                case AriaConditionOperators.LessThan:
                    result += " < ";
                    break;

                case AriaConditionOperators.Between:
                    result += " BETWEEN ";
                    break;
            }

            if (condition.Operator == AriaConditionOperators.Between)
            {
                AriaRange range = (AriaRange)condition.RightHandSide;
                AriaStandardDataType from = (AriaStandardDataType)range.From;
                AriaStandardDataType to = (AriaStandardDataType)range.To;

                if (to.PropertyDataPathDictionary.Count == 0)
                {
                    if(command != null) command.Parameters.Add(new AriaDbParameter("Param" + conditionNo.ToString().TrimEnd() + "_1", from.Value));
                    if (command != null) command.Parameters.Add(new AriaDbParameter("Param" + conditionNo.ToString().TrimEnd() + "_2", to.Value));

                    result += "@Param" + conditionNo.ToString().TrimEnd() + "_1 AND ";
                    result += "@Param" + conditionNo.ToString().TrimEnd() + "_2";
                }
                else
                {
                    result += GetDataPathFieldName(objectName + "." + from.PropertyDataPathDictionary["Value"].ToString(), clientId) + " AND ";
                    result += GetDataPathFieldName(objectName + "." + to.PropertyDataPathDictionary["Value"].ToString(), clientId);
                }
            }
            else
            {
                if (condition.RightHandSide.PropertyDataPathDictionary.Count == 0)
                {
                    if (command != null) command.Parameters.Add(new AriaDbParameter("Param" + conditionNo.ToString().TrimEnd(),
                                                                ((AriaStandardDataType)condition.RightHandSide).Value));
                    if (condition.Operator == AriaConditionOperators.Contains)
                    {
                        result += "'%' + " + "@Param" + conditionNo.ToString().TrimEnd() + " + '%'";
                    }
                    else
                    {
                        result += "@Param" + conditionNo.ToString().TrimEnd();
                    }
                }
                else
                {
                    if (condition.Operator == AriaConditionOperators.Contains)
                    {
                        result += "'%' + " +
                                    GetDataPathFieldName(objectName + "." + condition.RightHandSide.PropertyDataPathDictionary["Value"].ToString(), clientId) +
                                        " + '%'";
                    }
                    else
                    {
                        result += GetDataPathFieldName(objectName + "." + condition.RightHandSide.PropertyDataPathDictionary["Value"].ToString(), clientId);
                    }
                }
            }

            return result + "))";
        }

        public string GetKeyAndFilterFields(AriaDbConnection connection, string objctName, string clientId, bool withPracets, string conditionsString)
        {
            string sqlKeys = "";

            AriaEnviromentVariables env = new AriaEnviromentVariables();

            env.ClientID = clientId;
            env.ConnectionsRefresh();

            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaObjectRevision rootObjectRevision = (AriaObjectRevision)objectDictionary.LoadActiveRevision(new AriaDbConnection("", ""), objctName, clientId);

            ArrayList rootObjectProperties = objectDictionary.LoadAriaObjectProperties(connection, objctName, rootObjectRevision.ObjectRevision, false, clientId);

            ArrayList physicalKeys = new ArrayList();

            string left = withPracets ? "[" : "";
            string right = withPracets ? "]" : "";

            bool firstIndexColumnFound = false;
            for (int index = 0; index < rootObjectProperties.Count; index++)
            {
                AriaObjectProperty rootObjectProperty = (AriaObjectProperty)rootObjectProperties[index];

                if (rootObjectProperty.PropertySettings.GetType().ToString() == typeof(AriaFieldSettings).ToString() &&
                    (((AriaFieldSettings)rootObjectProperty.PropertySettings).IsPrimaryKey ||
                     conditionsString.ToUpper().Replace(" ", "").Contains(((AriaFieldSettings)rootObjectProperty.PropertySettings).FieldName.Trim().ToUpper())))
                {
                    if (firstIndexColumnFound)
                    {
                        sqlKeys += ", ";
                    }

                    sqlKeys += left + ((AriaFieldSettings)rootObjectProperty.PropertySettings).FieldName.Trim() + right;

                    firstIndexColumnFound = true;

                }

                if (rootObjectProperty.PropertySettings.GetType().ToString() == typeof(AriaRelatedFieldSettings).ToString() &&
                    ((AriaRelatedFieldSettings)rootObjectProperty.PropertySettings).IsPrimaryKey ||
                     conditionsString.ToUpper().Replace(" ", "").Contains(((AriaRelatedFieldSettings)rootObjectProperty.PropertySettings).FieldName.Trim().ToUpper()))
                {
                    if (firstIndexColumnFound)
                    {
                        sqlKeys += ", ";
                    }

                    sqlKeys += left + ((AriaRelatedFieldSettings)rootObjectProperty.PropertySettings).FieldName.Trim() + right;

                    firstIndexColumnFound = true;

                }

            }

            return sqlKeys;
        }

        public ArrayList GetAgentRequests(AriaDbConnection connection, string objctName, AriaConditionList conditions, string clientId)
        {



            string sqlCommandText = "";
            sqlCommandText += "SELECT DISTINCT ";

            AriaEnviromentVariables env = new AriaEnviromentVariables();

            //T20100512.0026 Hassan 2010 05 23 [Begin]
            env.ClientID = clientId;
            env.ConnectionsRefresh();
            //T20100512.0026 Hassan 2010 05 23 [END]


            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaObjectRevision rootObjectRevision = (AriaObjectRevision)objectDictionary.LoadActiveRevision(new AriaDbConnection("", ""), objctName, clientId);
            AriaDataObjectSettings rootObjectRevisionSettings = (AriaDataObjectSettings)rootObjectRevision.ObjectRevisionSettings;

            ArrayList rootObjectProperties = objectDictionary.LoadAriaObjectProperties(connection, objctName, rootObjectRevision.ObjectRevision, false, clientId);

            ArrayList physicalKeys = new ArrayList();

            string left;
            string right;
            if (rootObjectRevisionSettings.DatabaseType == AriaDatabaseTypes.Aria40Data)
            {
                left = "[";
                right = "]";
            }
            else
            {
                left = "";
                right = "";
            }

            string RelatedFieldsString = "";
            List<string> RelatedFields = new List<string>();

            List<string> mainFields = new List<string>();

            string conditionsString = "";
            for (int index = 0; index < conditions.Items.Count; index++)
            {
                conditionsString += GetCondition(null, index, objctName, (AriaCondition)conditions.Items[index], clientId);
            }
            
            bool firstIndexColumnFound = false;
            bool firstFilterColumnFound = false;
            for (int index = 0; index < rootObjectProperties.Count; index++)
            {
                AriaObjectProperty rootObjectProperty = (AriaObjectProperty)rootObjectProperties[index];

                if (((AriaFieldSettings)rootObjectProperty.PropertySettings).IsPrimaryKey)
                {
                    if (firstIndexColumnFound)
                    {
                        sqlCommandText += ", ";
                    }

                    // T20110803.0001 MAH 8/2/2011
                    //if (((AriaFieldSettings)rootObjectProperty.PropertySettings).FieldName.TrimEnd().ToUpper() == "ORDER")
                    //{
                    //    sqlCommandText += "[ORDER]";
                    //}
                    //else
                    //{
                    //    sqlCommandText += ((AriaFieldSettings)rootObjectProperty.PropertySettings).FieldName;
                    //}

                    sqlCommandText += "[" + objctName + "]." + "[" + ((AriaFieldSettings)rootObjectProperty.PropertySettings).FieldName.Trim() + "] AS [" + ((AriaFieldSettings)rootObjectProperty.PropertySettings).FieldName.Trim() + "]";

                    // T20110803.0001 MAH 8/2/2011 End

                    firstIndexColumnFound = true;

                    physicalKeys.Add(((AriaFieldSettings)rootObjectProperty.PropertySettings).FieldName.TrimEnd());
                }

                if (((AriaFieldSettings)rootObjectProperty.PropertySettings).IsPrimaryKey ||
                    conditionsString.ToUpper().Replace(" ", "").Contains(((AriaFieldSettings)rootObjectProperty.PropertySettings).FieldName.Trim().ToUpper()))
                {
                    if (firstFilterColumnFound)
                    {
                        RelatedFieldsString += ", ";
                    }

                    RelatedFieldsString += left + ((AriaFieldSettings)rootObjectProperty.PropertySettings).FieldName.Trim() + right;
                    RelatedFields.Add(((AriaFieldSettings)rootObjectProperty.PropertySettings).FieldName.ToUpper().Trim());

                    firstFilterColumnFound = true;
                }

                mainFields.Add(((AriaFieldSettings)rootObjectProperty.PropertySettings).FieldName.Trim());
            }

            

            if (rootObjectRevisionSettings.DatabaseType == AriaDatabaseTypes.Aria40Data)
            {
                // T20110803.0001 MAH 8/2/2011
                // sqlCommandText += " FROM " + rootObjectRevisionSettings.TableName + " AS " + rootObjectRevisionSettings.TableName;
                //sqlCommandText += " FROM " + rootObjectRevisionSettings.TableName + " AS [" + objctName + "]";

                sqlCommandText += " FROM OPENROWSET('MSDASQL','" + env.GetAria04CompanyDataConnectionStringODBC(connection.CompanyName) +
                                                                "','SELECT {RelaredFields} " + " FROM " + rootObjectRevisionSettings.TableName
                                                                + (rootObjectRevisionSettings.FixedFilter == null || rootObjectRevisionSettings.FixedFilter.Trim().Length == 0 ? "" : " WHERE " + rootObjectRevisionSettings.FixedFilter) + " ')" + " AS [" + objctName + "]";
                // T20110803.0001 MAH 8/2/2011 End
            }
            else
            {
                // T20110803.0001 MAH 8/2/2011
                // string rootTableOpenRowSet = " OPENROWSET('MSDASQL','" + env.GetConnectionString(connection.CompanyName,
                //                                                             rootObjectRevisionSettings.DatabaseType) +
                //                                                                 "','SELECT * FROM " + rootObjectRevisionSettings.TableName
                //                                                                     + "')" + " AS " + rootObjectRevisionSettings.TableName;

                string rootTableOpenRowSet = " OPENROWSET('MSDASQL','" + env.GetConnectionString(connection.CompanyName,
                                                            rootObjectRevisionSettings.DatabaseType) +
                                                                "','SELECT {RelaredFields} " + " FROM " + rootObjectRevisionSettings.TableName
                                                                    + "')" + " AS [" + objctName + "]";
                // T20110803.0001 MAH 8/2/2011 End

                sqlCommandText += " FROM " + rootTableOpenRowSet;
                
            }

            ArrayList childObjects = objectDictionary.LoadAriaObjectChildObjectsOfType(new AriaDbConnection("", ""), objctName, AriaObjectTypes.RelatedData, clientId);

            for (int index = 0; index < childObjects.Count; index++)
            {
                AriaObject childObject = (AriaObject)childObjects[index];

                AriaObjectRevision childRelatedObjectRevision = (AriaObjectRevision)objectDictionary.LoadActiveRevision(new AriaDbConnection("", ""), childObject.ObjectName, clientId);
                AriaRelatedDataObjectSettings childRelatedObjectRevisionSettings = (AriaRelatedDataObjectSettings)childRelatedObjectRevision.ObjectRevisionSettings;

                AriaObjectRevision childObjectRevision = (AriaObjectRevision)objectDictionary.LoadActiveRevision(new AriaDbConnection("", ""), childRelatedObjectRevisionSettings.DataObjectName, clientId);
                AriaDataObjectSettings childObjectRevisionSettings = (AriaDataObjectSettings)childObjectRevision.ObjectRevisionSettings;

                string[] childObjectParents = childObject.ObjectName.Split(new char[] { '.' });

                if (childObjectRevisionSettings.DatabaseType == AriaDatabaseTypes.Aria40Data)
                {
                    // T20110803.0001 MAH 8/2/2011
                    // sqlCommandText += " LEFT JOIN " + childObjectRevisionSettings.TableName + " AS " + childObjectParents[childObjectParents.Length - 1];
                    //sqlCommandText += " LEFT JOIN " + childObjectRevisionSettings.TableName + " AS [" + childObject.ObjectName.Trim() + "]";
                    sqlCommandText += " LEFT JOIN OPENROWSET('MSDASQL','" + env.GetAria04CompanyDataConnectionStringODBC(connection.CompanyName) +
                                                                    "','SELECT " + GetKeyAndFilterFields(connection, childObject.ObjectName, clientId, true, conditionsString) + " FROM " + childObjectRevisionSettings.TableName
                                                                        + "')" + " AS [" + childObject.ObjectName.Trim() + "]";

                    // T20110803.0001 MAH 8/2/2011 End

                    sqlCommandText += " ON " + childRelatedObjectRevisionSettings.Filter.Split(new char[] { '|' })[1];
                }
                else
                {
                    string childTableOpenRowSet = " OPENROWSET('MSDASQL','" + env.GetConnectionString(connection.CompanyName,
                                                                                childObjectRevisionSettings.DatabaseType) +
                                                                                    "','SELECT " + GetKeyAndFilterFields(connection, childObject.ObjectName, clientId, false, conditionsString) + " FROM " + childObjectRevisionSettings.TableName
                                                                                        + "')";

                    // T20110803.0001 MAH 8/2/2011
                    // sqlCommandText += " LEFT JOIN " + childTableOpenRowSet + " AS " + childObjectParents[childObjectParents.Length - 1];
                    sqlCommandText += " LEFT JOIN " + childTableOpenRowSet + " AS [" + childObject.ObjectName.Trim() + "]";
                    // T20110803.0001 MAH 8/2/2011 End
                    sqlCommandText += " ON " + childRelatedObjectRevisionSettings.Filter.Split(new char[] { '|' })[1];
                }

                for(int j = 0; j < mainFields.Count; j++)
                {
                    if (childRelatedObjectRevisionSettings.Filter.Split(new char[] { '|' })[1].ToUpper().Replace(" ", "").Contains(mainFields[j].ToUpper().Trim()))
                    {
                        if (!RelatedFields.Contains(mainFields[j].ToUpper().Trim()))
                        {
                            RelatedFieldsString += "," + left + mainFields[j] + right;
                            RelatedFields.Add(mainFields[j].ToUpper().Trim());
                        }
                    }
                }
            }

            sqlCommandText = sqlCommandText.Replace("{RelaredFields}", RelatedFieldsString);

            AriaDbCommand dbCommand = new AriaDbCommand("", connection, AriaDatabaseTypes.AriaOpenRowSet, clientId);

            for (int index = 0; index < conditions.Items.Count; index++)
            {
                if (index == 0) sqlCommandText += " WHERE ";
                sqlCommandText += GetCondition(dbCommand, index, objctName, (AriaCondition)conditions.Items[index], clientId);
                if (index < conditions.Items.Count - 1) sqlCommandText += " AND ";
            }

            //[Add]ahmed Maher -Date:16/04/2010 -Support System Date as value or sql server Functions
            for (int i = 0; i < dbCommand.Parameters.Count; i++)
            {
                if (dbCommand.Parameters[i].ParameterValue.ToString().IndexOf("{") == 0 &&
                    dbCommand.Parameters[i].ParameterValue.ToString().IndexOf("}") ==
                        dbCommand.Parameters[i].ParameterValue.ToString().Trim().Length - 1)
                {
                    string v = dbCommand.Parameters[i].ParameterValue.ToString().Replace("{", "").Replace("}", "");
                    sqlCommandText = sqlCommandText.Replace("@" + dbCommand.Parameters[i].ParameterName, v);
                }
            }
            //[END]
            dbCommand.CommandText = sqlCommandText;

            EventLog.WriteEntry("Aria.Services.RequestHandler.sql", sqlCommandText, EventLogEntryType.Information);


            DataTable table = dbCommand.GetDataTable();

            ArrayList dataPointers = new ArrayList();
            for (int recordIndex = 0; recordIndex < table.Rows.Count; recordIndex++)
            {
                AriaDataObjectPointer pointer = new AriaDataObjectPointer();
                for (int index = 0; index < physicalKeys.Count; index++)
                {
                    // T20110803.0001 MAH 8/2/2011
                    // pointer.KeyFields.Add(new AriaDataObjectPointerKeyField((string)physicalKeys[index], (string)table.Rows[recordIndex][(string)physicalKeys[index]]));
                    pointer.KeyFields.Add(new AriaDataObjectPointerKeyField((string)physicalKeys[index], table.Rows[recordIndex][(string)physicalKeys[index]]));
                    // T20110803.0001 MAH 8/2/2011 End
                }
                dataPointers.Add(pointer);
            }

            return dataPointers;
        }

        private ArrayList GetAriaObjects(AriaObject ariaObject, string clientId)
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            ArrayList ariaObjects = objectDictionary.LoadAriaObjectChildObjects(new AriaDbConnection("", ""), ariaObject.ObjectName, clientId);

            for (int index = 0; index < ariaObjects.Count; index++)
            {
                ariaObjects.AddRange(GetAriaObjects((AriaObject)ariaObjects[index], clientId));
            }

            return ariaObjects;
        }

        public string GetRequestCompany(string requestId, string clientId)
        {
            string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RequestID", requestId));

            AriaDataProvider dataProvider = new AriaDataProvider();
            AriaRequest request = (AriaRequest)dataProvider.GetObjectList(command, typeof(AriaRequest))[0];

            return request.Context.CompanyName;
        }

        public AriaRequest GetRequest(string requestId, string clientId)
        {
            string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RequestID", requestId));

            AriaDataProvider dataProvider = new AriaDataProvider();
            return (AriaRequest)dataProvider.GetObjectList(command, typeof(AriaRequest))[0];

        }

        // T20110803.0001 MAH 9/13/2011
        public string[] GetPrinters()
        {
            List<string> result = new List<string>();

            for (int i = 0; i < System.Drawing.Printing.PrinterSettings.InstalledPrinters.Count; i++)
            {
                result.Add(System.Drawing.Printing.PrinterSettings.InstalledPrinters[i]);
            }

            return result.ToArray();
        }
        // T20110803.0001 MAH 9/13/2011

        private readonly Random _rng = new Random();
        private const string _chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

        private string RandomString(int size)
        {
            char[] buffer = new char[size];

            for (int i = 0; i < size; i++)
            {
                buffer[i] = _chars[_rng.Next(_chars.Length)];
            }
            return new string(buffer);
        }
    }
}
