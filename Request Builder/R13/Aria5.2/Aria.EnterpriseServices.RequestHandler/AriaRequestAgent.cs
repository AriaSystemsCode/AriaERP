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
using Aria.Utilities.Log;
using System.Xml;
using System.Windows.Forms;

namespace Aria.EnterpriseServices.RequestHandler
{
    /// <summary>
    /// This class responsible on run request depends on condition (Execute Immediate Request, Execute One Time Only Requests... etc)
    /// </summary>
    public class AriaRequestAgent : MarshalByRefObject
    {
        //HIA  T20140226.0012 - How to edit or adjust the current requests on RB (Nina McLemore, Inc)  2015-01-10 [Begin]
        private static Dictionary<int, int> _failedRequest = new Dictionary<int, int>();
        //HIA  T20140226.0012 - How to edit or adjust the current requests on RB (Nina McLemore, Inc) [End]

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
            //SAB 01/28/2013 [Start]
            //string AriaDbCommandText = "SELECT * FROM AriaRequest " +
            //                                "WHERE StartAfterDate <= @Now " +
            //                                    "AND Status = @OnHoldStatus " +
            //                                        "AND RecurrenceType = @Once ";
            string AriaDbCommandText = "SELECT * FROM AriaRequest " +
                                            "WHERE StartAfterDate <= @Today " +
                                                "AND RequestStartTime <= @Now " +
                                                    "AND Status = @OnHoldStatus " +
                                                        "AND RecurrenceType = @Once ";
            //SAB 01/28/2013 [End]

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);

            //SAB 01/28/2013 [Start]
            //command.Parameters.Add(new AriaDbParameter("Now", DateTime.Now.Date));
            command.Parameters.Add(new AriaDbParameter("Today", DateTime.Now.Date));
            command.Parameters.Add(new AriaDbParameter("Now", new DateTime(1900, 1, 1, DateTime.Now.Hour, DateTime.Now.Minute, DateTime.Now.Second)));
            //SAB 01/28/2013 [Start]

            command.Parameters.Add(new AriaDbParameter("OnHoldStatus", AriaRequestStatusTypes.OnHold.ToString()));
            command.Parameters.Add(new AriaDbParameter("Once", AriaRequestRecurrenceTypes.Once.ToString()));

            AriaDataProvider dataProvider = new AriaDataProvider();

            ArrayList requests = dataProvider.GetObjectList(command, typeof(AriaRequest));

            for (int index = 0; index < requests.Count; index++)
            {
                AriaRequest onceRequest = (AriaRequest)requests[index];

                //SAB 01/28/2013 [Start]
                //AriaDbCommand similarCommand = new AriaDbCommand("SELECT * FROM AriaRequest Where RequestID > @RequestID AND ParentRequestID = @ParentRequestID", connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                AriaDbCommand similarCommand = new AriaDbCommand("SELECT * FROM AriaRequest Where RequestID > @RequestID AND ParentRequestID = @ParentRequestID AND StartAfterDate <= @Today AND RequestStartTime <= @Now AND Status = @OnHoldStatus AND RecurrenceType = @Once", connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                //SAB 01/28/2013 [End]

                similarCommand.Parameters.Add(new AriaDbParameter("RequestID", onceRequest.RequestID));
                similarCommand.Parameters.Add(new AriaDbParameter("ParentRequestID", onceRequest.ParentRequestID));

                //SAB 01/28/2013 [Start]                
                similarCommand.Parameters.Add(new AriaDbParameter("Today", DateTime.Now.Date));
                similarCommand.Parameters.Add(new AriaDbParameter("Now", new DateTime(1900, 1, 1, DateTime.Now.Hour, DateTime.Now.Minute, DateTime.Now.Second)));
                similarCommand.Parameters.Add(new AriaDbParameter("OnHoldStatus", AriaRequestStatusTypes.OnHold.ToString()));
                similarCommand.Parameters.Add(new AriaDbParameter("Once", AriaRequestRecurrenceTypes.Once.ToString()));
                //SAB 01/28/2013 [Start]

                ArrayList similarRequests = dataProvider.GetObjectList(similarCommand, typeof(AriaRequest));

                //SAB 01/28/2013 [Start]
                //if (similarRequests.Count > 0 && (onceRequest.EventObjectName == null || onceRequest.EventObjectName.Trim().Length == 0))
                if (similarRequests.Count > 0 && (onceRequest.EventName == null || onceRequest.EventName.Trim().Length == 0))
                //SAB 01/28/2013 [End]
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

                        try
                        {
                            AriaThreadManager thread = new AriaThreadManager();
                            onceRequest.clientID = clientId;
                            thread.StartThread(onceRequest, clientId);
                        }
                        catch (Exception ex)
                        {
                            EventLog.WriteEntry("Get One Time Only Requests", ex.Message, EventLogEntryType.Information);
                        }
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
                        reqOptionGridXML.Load(reqOptionGrid.FileName);
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

            //MAH Log
            List<AriaLogElement> logs = new List<AriaLogElement>();
            if (onceRequest.EventArgumentList != null)
            {
                logs.Add(new AriaLogElement("Date", onceRequest.StartAfterDate.ToString()));
                logs.Add(new AriaLogElement("Time", onceRequest.RequestStartTime.ToString()));

                for (int i = 0; i < onceRequest.EventArgumentList.Count; i++)
                {
                    AriaArgument arg = onceRequest.EventArgumentList[0] as AriaArgument;

                    if (arg.Value is AriaDataObjectPointer)
                    {
                        AriaDataObjectPointer pointer = arg.Value as AriaDataObjectPointer;

                        for (int j = 0; j < pointer.KeyFields.Count; j++)
                        {
                            AriaDataObjectPointerKeyField field = pointer.KeyFields[j] as AriaDataObjectPointerKeyField;
                            logs.Add(new AriaLogElement(field.FieldName, field.Value.ToString()));
                        }
                    }
                }
            }

            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Create Response to Event Request", logs.ToArray());
            //MAH Log

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
            XmlDocument xmlInstanceDocument = new XmlDocument();

            string instanceName = "";

            xmlInstanceDocument.Load(Path.Combine(Application.StartupPath, "instance settings.xml"));
            for (int index = 0; index < xmlInstanceDocument.ChildNodes.Count; index++)
            {
                if (xmlInstanceDocument.ChildNodes[index].Name.Trim().ToUpper() == "InstanceName".ToUpper())
                {
                    instanceName = xmlInstanceDocument.ChildNodes[index].InnerText;
                }
            }
            EventLog.WriteEntry("After Instance Read", "Derby", EventLogEntryType.Information);

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


                        break;
                    }
                }
            }
            EventLog.WriteEntry("After XML READ", "Derby", EventLogEntryType.Information);
            if (!outputFileNameExist)
            {
                outputFileName = null;
            }
            // MAH Get Request File Name If Exist


            AriaRequest request = (AriaRequest)requestObject;
            //if (string.IsNullOrEmpty(clientId) == true)
            //{ clientId = request.clientID; };

            //SAB 01-10-2013 [Start]
            request.Error = null;
            //SAB 01-10-2013 [Start]

            AriaDbConnection connection = new AriaDbConnection();

            connection.Context = request.Context;
            connection.CompanyName = request.Context.CompanyName;
            connection.Context.UserName = request.UserName;
            connection.Context.MethodName = request.MethodName;

            // Mah Log
            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Execute");
            // Mah Log

            AriaDataProvider dataProvider = new AriaDataProvider();
            AriaMessagingManager messaging = new AriaMessagingManager();


            //SAB 01-30-2014 Fix event request [Start]
            #region Commented Code
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

                    //SAB 02/10/2013 [Start]
                    //object[] argumentsArray = new object[request.MethodArgumentList.Count + 2];
                    //argumentsArray[0] = request.RequestID.ToString();
                    //for (int index = 1; index < argumentsArray.Length - 1; index++)
                    //{ argumentsArray[index] = (object)((AriaArgument)request.MethodArgumentList[index - 1]).Value; }

                    //try
                    //{ argumentsArray[argumentsArray.Length - 1] = clientId.ToString(); }
                    //catch (Exception ex)
                    //{ argumentsArray[argumentsArray.Length - 1] = ""; }

                    object[] argumentsArray = new object[request.MethodArgumentList.Count + 6];
                    argumentsArray[0] = request.RequestID.ToString();
                    argumentsArray[1] = (object)((AriaArgument)request.MethodArgumentList[0]).Value;
                    try
                    {
                        argumentsArray[2] = clientId.ToString();
                    }
                    catch (Exception)
                    {
                        argumentsArray[2] = "";
                    }
                    //SAB 02/10/2013 [End]


                    GetRequestMethodArgumentsSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList, clientId);
                    connection.Context = request.Context;
                    connection.CompanyName = request.Context.CompanyName;
                    connection.Context.UserName = request.UserName;
                    connection.Context.MethodName = request.MethodName;

                    AriaReflector reflector = new AriaReflector();

                    argumentsArray[1] = ((Aria.DataTypes.AriaOptionGridXmlDataSet)argumentsArray[1]).FileName;

                    // Handle SAAS Issues
                    //SAB 02/10/2013 [Start]
                    //try
                    //{
                    //    if (serverObjectSettings is AriaServerObjectSettings)
                    //        reflector.ExecuteMethod(clientId + "_" + ((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                    //    else if (serverObjectSettings is AriaReportObjectSettings)
                    //        reflector.ExecuteMethod(clientId + "_" + ((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                    //}
                    //catch (Exception)
                    //{
                    //    if (serverObjectSettings is AriaServerObjectSettings)
                    //        reflector.ExecuteMethod(((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                    //    else if (serverObjectSettings is AriaReportObjectSettings)
                    //        reflector.ExecuteMethod(((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                    //}
                    if (serverObjectSettings is AriaServerObjectSettings)
                    {
                        argumentsArray[3] = ((AriaServerObjectSettings)serverObjectSettings).ClassName.Split('.')[0];
                        AriaEnviromentVariables ariaEnv = new AriaEnviromentVariables();
                        argumentsArray[4] = SystemInformation.ComputerName.ToUpper();
                        argumentsArray[5] = instanceName;
                        argumentsArray[6] = ariaEnv.RpcPort;
                        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "ExecuteMethod Parameters <RequestHandler.RequestHandler>, <" + request.MethodName.ToString() + ">, <" + argumentsArray[3] + ">"); //SAB                        
                        //SAB 04-03-2014 Add validation on the Execution Time [Start]
                        DateTime startTime = DateTime.Now;
                        //SAB 04-03-2014 Add validation on the Execution Time [End]
                        reflector.ExecuteMethod("RequestHandler.RequestHandler", request.MethodName, argumentsArray);
                        //SAB 04-03-2014 Add validation on the Execution Time [Start]
                        ValidateExectionTime(startTime, request, messaging, connection, clientId);
                        //SAB 04-03-2014 Add validation on the Execution Time [End]
                    }
                    else if (serverObjectSettings is AriaReportObjectSettings)
                    {
                        argumentsArray[3] = ((AriaReportObjectSettings)serverObjectSettings).ClassName.Split('.')[0];
                        AriaEnviromentVariables ariaEnv = new AriaEnviromentVariables();
                        argumentsArray[4] = SystemInformation.ComputerName.ToUpper();
                        argumentsArray[5] = instanceName;
                        argumentsArray[6] = ariaEnv.RpcPort;
                        //SAB 04-03-2014 Add validation on the Execution Time [Start]
                        DateTime startTime = DateTime.Now;
                        //SAB 04-03-2014 Add validation on the Execution Time [End]
                        reflector.ExecuteMethod("RequestHandler.RequestHandler", request.MethodName, argumentsArray);
                        //SAB 04-03-2014 Add validation on the Execution Time [Start]
                        ValidateExectionTime(startTime, request, messaging, connection, clientId);
                        //SAB 04-03-2014 Add validation on the Execution Time [End]
                    }
                    //SAB 02/10/2013 [End]


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

                            // Mah Log
                            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Failed Email", messaging.GetLog());
                            // Mah Log
                        }

                        return;
                    }
                    # endregion

                    request.Status = AriaRequestStatusTypes.Completed;
                    EventLog.WriteEntry("INVOKEREQuest", "Complete", EventLogEntryType.Error);
                    if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
                    {
                        if (outputFileName == null || System.IO.File.Exists(outputFileName))
                        {
                            messaging.SendEmail(connection, request.MethodArgumentList, GetRequest(request.RequestID, clientId).CompleteNotification, clientId);

                            // Mah Log
                            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
                            // Mah Log
                        }
                    }

                    request.Occurence++;
                    dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                                new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);

                    // Mah Log
                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Complete Request");
                    // Mah Log


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

                        //SAB 01-30-2014 Fix event request [Start]
                        AriaObjectDictionaryDBCentric objecDictionary = new AriaObjectDictionaryDBCentric();
                        AriaObjectRevision rootDataObjectRevision = (AriaObjectRevision)objecDictionary.LoadActiveRevision(connection, request.EventObjectName, clientId);
                        //SAB 01-30-2014 Fix event request [End]

                        //SAB 02/10/2013 [Start]
                        //object[] argumentsArray = new object[request.MethodArgumentList.Count + 2];
                        //argumentsArray[0] = request.RequestID.ToString();

                        //for (int index = 1; index < argumentsArray.Length - 1; index++)
                        //{ argumentsArray[index] = (object)((AriaArgument)request.MethodArgumentList[index - 1]).Value; }

                        //try
                        //{ argumentsArray[argumentsArray.Length - 1] = clientId.ToString(); }
                        //catch (Exception ex)
                        //{ argumentsArray[argumentsArray.Length - 1] = ""; }
                        object[] argumentsArray = new object[request.MethodArgumentList.Count + 6];
                        argumentsArray[0] = request.RequestID.ToString();
                        argumentsArray[1] = (object)((AriaArgument)request.MethodArgumentList[0]).Value;
                        try
                        {
                            argumentsArray[2] = clientId.ToString();
                        }
                        catch (Exception)
                        {
                            argumentsArray[2] = "";
                        }
                        //SAB 02/10/2013 [End]

                        //SAB 01-30-2014 Fix event request [Start]
                        AriaOptionGridXmlDataSet optionGrid;
                        string OldFile, NewOutPutFile, OldOutPutFile;
                        bool ReplaceAgentValues;
                        Aria.DataTypes.AriaDataObjectPointer eventPointer = new Aria.DataTypes.AriaDataObjectPointer();
                        eventPointer = (AriaDataObjectPointer)((AriaArgument)request.EventArgumentList[0]).Value;
                        GenerateOptionGridXmlFileForObjectKey(argumentsArray, eventPointer, rootDataObjectRevision, out optionGrid, out OldFile, out NewOutPutFile, out OldOutPutFile, out ReplaceAgentValues);
                        //SAB 01-30-2014 Fix event request [End]

                        GetRequestMethodArgumentsSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList, clientId);
                        connection.Context = request.Context;
                        connection.CompanyName = request.Context.CompanyName;
                        connection.Context.UserName = request.UserName;
                        connection.Context.MethodName = request.MethodName;

                        AriaReflector reflector = new AriaReflector();

                        argumentsArray[1] = ((Aria.DataTypes.AriaOptionGridXmlDataSet)argumentsArray[1]).FileName;

                        //SAB 02/10/2013 [Start]
                        //try
                        //{
                        //    if (serverObjectSettings is AriaServerObjectSettings)
                        //        reflector.ExecuteMethod(clientId + "_" + ((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                        //    else if (serverObjectSettings is AriaReportObjectSettings)
                        //        reflector.ExecuteMethod(clientId + "_" + ((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                        //}
                        //catch (Exception)
                        //{
                        //    if (serverObjectSettings is AriaServerObjectSettings)
                        //        reflector.ExecuteMethod(((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                        //    else if (serverObjectSettings is AriaReportObjectSettings)
                        //        reflector.ExecuteMethod(((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                        //}
                        if (serverObjectSettings is AriaServerObjectSettings)
                        {
                            argumentsArray[3] = ((AriaServerObjectSettings)serverObjectSettings).ClassName.Split('.')[0];
                            AriaEnviromentVariables ariaEnv = new AriaEnviromentVariables();
                            argumentsArray[4] = SystemInformation.ComputerName.ToUpper();
                            argumentsArray[5] = instanceName;
                            argumentsArray[6] = ariaEnv.RpcPort;
                            //SAB 04-03-2014 Add validation on the Execution Time [Start]
                            DateTime startTime = DateTime.Now;
                            //SAB 04-03-2014 Add validation on the Execution Time [End]
                            reflector.ExecuteMethod("RequestHandler.RequestHandler", request.MethodName, argumentsArray);
                            //SAB 04-03-2014 Add validation on the Execution Time [Start]
                            ValidateExectionTime(startTime, request, messaging, connection, clientId);
                            //SAB 04-03-2014 Add validation on the Execution Time [End]
                        }
                        else if (serverObjectSettings is AriaReportObjectSettings)
                        {
                            argumentsArray[3] = ((AriaReportObjectSettings)serverObjectSettings).ClassName.Split('.')[0];
                            AriaEnviromentVariables ariaEnv = new AriaEnviromentVariables();
                            argumentsArray[4] = SystemInformation.ComputerName.ToUpper();
                            argumentsArray[5] = instanceName;
                            argumentsArray[6] = ariaEnv.RpcPort;
                            //SAB 04-03-2014 Add validation on the Execution Time [Start]
                            DateTime startTime = DateTime.Now;
                            //SAB 04-03-2014 Add validation on the Execution Time [End]
                            reflector.ExecuteMethod("RequestHandler.RequestHandler", request.MethodName, argumentsArray);
                            //SAB 04-03-2014 Add validation on the Execution Time [Start]
                            ValidateExectionTime(startTime, request, messaging, connection, clientId);
                            //SAB 04-03-2014 Add validation on the Execution Time [End]
                        }
                        //SAB 02/10/2013 [End]

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

                                // Mah Log
                                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Failed Email", messaging.GetLog());
                                // Mah Log
                            }

                            return;
                        }

                        //SAB 01-30-2014 Fix event request [Start]
                        AriaDataObjectPointerSettings settings = new AriaDataObjectPointerSettings();
                        settings.DataObjectName = request.EventObjectName;
                        settings.DataObjectRevision = objecDictionary.LoadActiveRevision(connection, request.EventObjectName, clientId).ObjectRevision;
                        SendAgentMail(request, clientId, eventPointer, settings, connection, messaging, outputFileName, optionGrid, OldFile, NewOutPutFile, OldOutPutFile, ReplaceAgentValues);
                        //SAB 01-30-2014 Fix event request [End]
                    }

                    # endregion

                    request.Status = AriaRequestStatusTypes.Completed;

                    GetRequestEventArgumentsSettings(request.EventObjectName, request.EventName, request.EventArgumentList, clientId);

                    if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
                    {
                        if (outputFileName == null || System.IO.File.Exists(outputFileName))
                        {
                            messaging.SendEmail(connection, request.EventArgumentList, GetRequest(request.RequestID, clientId).CompleteNotification, clientId);

                            // Mah Log
                            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
                            // Mah Log
                        }
                    }

                    request.Occurence += 1;

                    dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                                new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);

                    // Mah Log
                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Complete Request");
                    // Mah Log
                }
                # endregion Responce To Event Request Handeling

                # region Agent Request Handeling
                // Agent Request Handeling 
                // if (request.EventName.Trim().Length == 0 && request.EventConditionList != null && request.EventConditionList.Items.Count > 0 && RequestCaseHandled == false)

                if (request.EventName.Trim().Length == 0 && request.EventObjectName.Length != 0 && RequestCaseHandled == false)
                {
                    RequestCaseHandled = true;
                    request.Status = AriaRequestStatusTypes.Completed;

                    // Get the data set as pointers to each record, each pointer contins the Primiary key Only
                    ArrayList list = GetAgentRequests(new AriaDbConnection(request.Context.CustomerName,
                                                        request.Context.CompanyName), request.EventObjectName, request.EventConditionList, clientId, request.RequestID);


                    // MAHMAHMAH
                    AriaEnviromentVariables env = new AriaEnviromentVariables();
                    if (env.AriaMaxRecordsPerAgent != 0 && list.Count > env.AriaMaxRecordsPerAgent)
                    {
                        //SAB 04-03-2014 Add validation on the Execution Time [Start]
                        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Max Number of Records per Agent exceeded", new AriaLogElement("Request Number", request.RequestID), new AriaLogElement("Agent Records Count", list.Count.ToString()));

                        if (!string.IsNullOrEmpty(env.ExceedLimitNotificationEmail) || !string.IsNullOrEmpty(env.ClientExceedLimitNotificationEmail))
                        {
                            AriaEmail notifyEmail = new AriaEmail();
                            notifyEmail.To = string.IsNullOrEmpty(env.ExceedLimitNotificationEmail) ? "" : env.ExceedLimitNotificationEmail;
                            notifyEmail.To = (string.IsNullOrEmpty(notifyEmail.To) || string.IsNullOrEmpty(env.ClientExceedLimitNotificationEmail)) ? "" : (notifyEmail.To + ";") + (string.IsNullOrEmpty(env.ClientExceedLimitNotificationEmail) ? "" : env.ClientExceedLimitNotificationEmail);
                            notifyEmail.Subject = "Max Number of Records per Agent exceeded <<" + request.RequestNumber + ">>";
                            notifyEmail.Body = "Client = " + clientId + "\nRequest Number = " + request.RequestNumber + "\nAgent Records Count = " + list.Count.ToString();

                            messaging.SendEmail(connection, request.EventArgumentList, notifyEmail, clientId);
                        }
                        //SAB 04-03-2014 Add validation on the Execution Time [End]
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

                            //SAB 01-30-2014 Fix event request [Start]
                            AriaObjectRevision rootObjectRevision = (AriaObjectRevision)objecDictionary.LoadActiveRevision(connection, request.EventObjectName, clientId);
                            //SAB 01-30-2014 Fix event request [End]

                            //SAB 02/10/2013 [Start]
                            //object[] argumentsArray = new object[request.MethodArgumentList.Count + 2];
                            //argumentsArray[0] = request.RequestID.ToString();
                            //for (int intindex = 1; intindex < argumentsArray.Length - 1; intindex++)
                            //{ argumentsArray[intindex] = (object)((AriaArgument)request.MethodArgumentList[intindex - 1]).Value; }

                            //try
                            //{ argumentsArray[argumentsArray.Length - 1] = clientId.ToString(); }
                            //catch (Exception ex)
                            //{ argumentsArray[argumentsArray.Length - 1] = ""; }

                            object[] argumentsArray = new object[request.MethodArgumentList.Count + 6];
                            argumentsArray[0] = request.RequestID.ToString();
                            argumentsArray[1] = (object)((AriaArgument)request.MethodArgumentList[0]).Value;
                            try
                            {
                                argumentsArray[2] = clientId.ToString();
                            }
                            catch (Exception)
                            {
                                argumentsArray[2] = "";
                            }
                            //SAB 02/10/2013 [End]


                            // SAB 01-30-2014 Fix event request [Start]
                            ////MOH T20100226.0004 Start
                            //foreach (object obj in argumentsArray)
                            //{
                            //    if (obj is Aria.DataTypes.AriaOptionGridXmlDataSet)
                            //    {
                            //        optionGrid = obj as Aria.DataTypes.AriaOptionGridXmlDataSet;
                            //        System.Xml.XmlDocument optionGridXML = new System.Xml.XmlDocument();
                            //        optionGridXML.Load(optionGrid.FileName);
                            //        System.Xml.XmlNode lcRpExp = optionGridXML.SelectSingleNode(@"//*/row[Name='lcrpExp']")["Value"];
                            //        if (lcRpExp != null && lcRpExp.InnerText != null && lcRpExp.InnerText.Contains("{") && lcRpExp.InnerText.Contains("}"))
                            //        {
                            //            ReplaceAgentValues = true;
                            //            string oldvalue = lcRpExp.InnerText.Substring(lcRpExp.InnerText.IndexOf("{") + 1, lcRpExp.InnerText.IndexOf("}") - lcRpExp.InnerText.IndexOf("{") - 1);
                            //            string NewValue = "", PointerValue = "", PointerName = "", PointerType = "";
                            //            int PointerLength = 0;
                            //            if (list[index] is Aria.DataTypes.AriaDataObjectPointer)
                            //            {
                            //                Aria.DataTypes.AriaDataObjectPointer keyvalue = list[index] as Aria.DataTypes.AriaDataObjectPointer;
                            //                foreach (object objpointer in keyvalue.KeyFields)
                            //                {
                            //                    Aria.DataTypes.AriaDataObjectPointerKeyField pointer = objpointer as Aria.DataTypes.AriaDataObjectPointerKeyField;
                            //                    if (pointer.FieldName.ToUpper() == oldvalue.ToUpper())
                            //                    {
                            //                        PointerValue = pointer.Value.ToString();
                            //                        PointerName = pointer.FieldName;
                            //                        if (pointer.Value is string)
                            //                            PointerType = "C";
                            //                        else if (pointer.Value is int || pointer.Value is double || pointer.Value is decimal || pointer.Value is float)
                            //                        {
                            //                            PointerType = "N";
                            //                        }
                            //                        else if (pointer.Value is bool || pointer.Value.ToString() == ".T." || pointer.Value.ToString() == ".F.")
                            //                            PointerType = "L";
                            //                        PointerLength = PointerValue.Length;
                            //                        break;
                            //                    }
                            //                }
                            //                NewValue = PointerValue;
                            //            }
                            //            lcRpExp.InnerText = lcRpExp.InnerText.Replace("{" + oldvalue + "}", "'" + NewValue + "'");
                            //            AriaObjectRevision rootObjectRevision = (AriaObjectRevision)objecDictionary.LoadActiveRevision(connection, request.EventObjectName, clientId);
                            //            string table = ((AriaDataObjectSettings)rootObjectRevision.ObjectRevisionSettings).TableName + "." + PointerName;
                            //            System.Xml.XmlNode tablenode = optionGridXML.SelectSingleNode(@"//*/row[starts-with(translate(Value,'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ'),'" + table.ToUpper() + "')]");
                            //            string laOGFxFltname = tablenode["Name"].InnerText.Replace(",1]", ",6]");
                            //            System.Xml.XmlNode laOGFxFlt = optionGridXML.SelectSingleNode(@"//*/row[Name='" + laOGFxFltname + "']")["Value"];
                            //            if (laOGFxFlt != null)
                            //            {
                            //                NewValue = "";
                            //                while (NewValue.Length < 8)
                            //                    NewValue += Path.GetRandomFileName().Replace(".", "");
                            //                NewValue = RandomString(8);
                            //                //NewValue = NewValue.Length > 8 ? NewValue.Remove(8) : NewValue;
                            //                laOGFxFlt.InnerText = NewValue;
                            //                System.Xml.XmlNode NewNode = optionGridXML.CreateNode(System.Xml.XmlNodeType.Element, "row", "");
                            //                string xmlvalue = "";
                            //                xmlvalue = @"<DataType>System.Table</DataType>";
                            //                xmlvalue += @"<Name>" + NewValue + "</Name>";
                            //                xmlvalue += @"<Value></Value>";
                            //                xmlvalue += @"<CursorStrucutre>";
                            //                xmlvalue += @"<Field>";
                            //                xmlvalue += @"<Name>KEYEXP</Name>";
                            //                xmlvalue += @"<Type>" + PointerType + "</Type>";
                            //                xmlvalue += @"<Width>" + PointerLength + "</Width>";
                            //                xmlvalue += @"<Decimals>0</Decimals>";
                            //                xmlvalue += @"</Field>";
                            //                xmlvalue += @"<Field>";
                            //                xmlvalue += @"<Name>" + PointerName + "</Name>";
                            //                xmlvalue += @"<Type>" + PointerType + "</Type>";
                            //                xmlvalue += @"<Width>" + PointerLength + "</Width>";
                            //                xmlvalue += @"<Decimals>0</Decimals>";
                            //                xmlvalue += @"</Field>";
                            //                xmlvalue += @"</CursorStrucutre>";
                            //                NewNode.InnerXml = xmlvalue;
                            //                //SAB 01-31-2013 [Start]
                            //                //NewNode["Value"].InnerText = "<?xml version=\"1.0\"?> <xdoc> <" + NewValue + "> <row> <keyexp>" + PointerValue + "</keyexp> <" + PointerName + ">" + PointerValue + "</" + PointerName + "> </row> </" + NewValue + "> </xdoc>";
                            //                NewNode["Value"].InnerText = "<?xml version=\"1.0\"?> <xdoc> <" + NewValue + "> <row> <keyexp>" + PointerValue + "</keyexp> <" + PointerName.ToLower() + ">" + PointerValue + "</" + PointerName.ToLower() + "> </row> </" + NewValue + "> </xdoc>";
                            //                //SAB 01-31-2013 [End]
                            //                optionGridXML.SelectSingleNode("//Parameters").AppendChild(NewNode);
                            //            }
                            //            string NewName = Path.GetRandomFileName().Replace(".", "") + ".xml";
                            //            OldOutPutFile = optionGridXML.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText;
                            //            NewOutPutFile = Path.GetDirectoryName(OldOutPutFile) + "\\" + NewName.Replace(".xml", Path.GetExtension(OldOutPutFile));
                            //            optionGridXML.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText = NewOutPutFile;

                            //            NewName = Path.GetDirectoryName(((Aria.DataTypes.AriaOptionGridXmlDataSet)obj).FileName) + "\\" + NewName;
                            //            optionGridXML.Save(NewName);
                            //            OldFile = optionGrid.FileName;
                            //            optionGrid.FileName = NewName;
                            //            break;
                            //        }
                            //    }
                            //}

                            //// MOH T20100226.0004 End
                            GenerateOptionGridXmlFileForObjectKey(argumentsArray, list[index] as AriaDataObjectPointer, rootObjectRevision, out optionGrid, out OldFile, out NewOutPutFile, out OldOutPutFile, out ReplaceAgentValues);
                            //SAB 01-30-2014 Fix event request [End]

                            GetRequestMethodArgumentsSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList, clientId);
                            connection.Context = request.Context;
                            connection.CompanyName = request.Context.CompanyName;
                            connection.Context.UserName = request.UserName;
                            connection.Context.MethodName = request.MethodName;

                            AriaReflector reflector = new AriaReflector();

                            argumentsArray[1] = ((Aria.DataTypes.AriaOptionGridXmlDataSet)argumentsArray[1]).FileName;

                            //SAB 01-10-2013 [Start]
                            //// Mah Log
                            //AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Execute Request for Record", new AriaLogElement("FileName", ((Aria.DataTypes.AriaOptionGridXmlDataSet)argumentsArray[1]).FileName));
                            //// Mah Log
                            //SAB 01-10-2013 [End]

                            //SAB 02/10/2013 [Start]
                            //try
                            //{
                            //    if (serverObjectSettings is AriaServerObjectSettings)
                            //        reflector.ExecuteMethod(clientId + "_" + ((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                            //    else if (serverObjectSettings is AriaReportObjectSettings)
                            //        reflector.ExecuteMethod(clientId + "_" + ((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                            //}
                            //catch (Exception)
                            //{
                            //    if (serverObjectSettings is AriaServerObjectSettings)
                            //        reflector.ExecuteMethod(((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                            //    else if (serverObjectSettings is AriaReportObjectSettings)
                            //        reflector.ExecuteMethod(((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                            //}
                            if (serverObjectSettings is AriaServerObjectSettings)
                            {
                                argumentsArray[3] = ((AriaServerObjectSettings)serverObjectSettings).ClassName.Split('.')[0];
                                AriaEnviromentVariables ariaEnv = new AriaEnviromentVariables();
                                argumentsArray[4] = SystemInformation.ComputerName.ToUpper();
                                argumentsArray[5] = instanceName;
                                argumentsArray[6] = ariaEnv.RpcPort;
                                //SAB 04-03-2014 Add validation on the Execution Time [Start]
                                DateTime startTime = DateTime.Now;
                                //SAB 04-03-2014 Add validation on the Execution Time [End]
                                reflector.ExecuteMethod("RequestHandler.RequestHandler", request.MethodName, argumentsArray);
                                //SAB 04-03-2014 Add validation on the Execution Time [Start]
                                ValidateExectionTime(startTime, request, messaging, connection, clientId);
                                //SAB 04-03-2014 Add validation on the Execution Time [End]
                            }
                            else if (serverObjectSettings is AriaReportObjectSettings)
                            {
                                argumentsArray[3] = ((AriaReportObjectSettings)serverObjectSettings).ClassName.Split('.')[0];
                                AriaEnviromentVariables ariaEnv = new AriaEnviromentVariables();
                                argumentsArray[4] = SystemInformation.ComputerName.ToUpper();
                                argumentsArray[5] = instanceName;
                                argumentsArray[6] = ariaEnv.RpcPort;
                                //SAB 04-03-2014 Add validation on the Execution Time [Start]
                                DateTime startTime = DateTime.Now;
                                //SAB 04-03-2014 Add validation on the Execution Time [End]
                                reflector.ExecuteMethod("RequestHandler.RequestHandler", request.MethodName, argumentsArray);
                                //SAB 04-03-2014 Add validation on the Execution Time [Start]
                                ValidateExectionTime(startTime, request, messaging, connection, clientId);
                                //SAB 04-03-2014 Add validation on the Execution Time [End]
                            }
                            //SAB 02/10/2013 [End]

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

                                    // Mah Log
                                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Failed Email", messaging.GetLog());
                                    // Mah Log
                                }

                                return;
                            }
                            //MOH  t20100929.0001 11-10-2010 Start
                        }
                        //MOH  t20100929.0001 11-10-2010 End
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


                        //SAB 01-30-2014 Fix event request [Start]
                        //AriaEmail agentEmail = GetRequest(request.RequestID, clientId).CompleteNotification;


                        //if (ReplaceAgentValues)
                        //{

                        //    if (NewOutPutFile != null && NewOutPutFile.Trim() != "")
                        //    {
                        //        bool containOld = false;
                        //        if (agentEmail.Attachment.ContainsKey(OldOutPutFile))
                        //        {
                        //            agentEmail.Attachment.Remove(OldOutPutFile);
                        //            containOld = true;
                        //        }

                        //        agentEmail.Attachment[NewOutPutFile] = NewOutPutFile;


                        //        try
                        //        {
                        //            if (agentEmail != null && agentEmail.IsValid())
                        //            {
                        //                request.EventArgumentList = new AriaArgumentList();
                        //                request.EventArgumentList.AddArgument("Pointer", (AriaDataType)list[index]);
                        //                request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                        //                ((AriaArgument)request.EventArgumentList[0]).Settings = settings;
                        //            }

                        //            if (NewOutPutFile == null || System.IO.File.Exists(NewOutPutFile))
                        //            {
                        //                messaging.SendEmail(connection, request.EventArgumentList, agentEmail, clientId);

                        //                // Mah Log
                        //                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
                        //                // Mah Log
                        //            }
                        //        }
                        //        catch (Exception ex)
                        //        {
                        //            throw ex;
                        //        }


                        //        agentEmail.Attachment.Remove(NewOutPutFile);

                        //        if (containOld)
                        //        {
                        //            agentEmail.Attachment.Add(OldOutPutFile, OldOutPutFile);
                        //        }
                        //    }
                        //    else
                        //    {
                        //        if (agentEmail != null && agentEmail.IsValid())
                        //        {
                        //            request.EventArgumentList = new AriaArgumentList();
                        //            request.EventArgumentList.AddArgument("Pointer", (AriaDataType)list[index]);
                        //            request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                        //            ((AriaArgument)request.EventArgumentList[0]).Settings = settings;
                        //        }

                        //        if (outputFileName == null || System.IO.File.Exists(outputFileName))
                        //        {
                        //            messaging.SendEmail(connection, request.EventArgumentList, agentEmail, clientId);

                        //            // Mah Log
                        //            // SABER
                        //            //AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
                        //            // Mah Log

                        //        }
                        //    }
                        //}
                        //else
                        //{
                        //    if (agentEmail != null && agentEmail.IsValid())
                        //    {
                        //        request.EventArgumentList = new AriaArgumentList();
                        //        request.EventArgumentList.AddArgument("Pointer", (AriaDataType)list[index]);
                        //        request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                        //        ((AriaArgument)request.EventArgumentList[0]).Settings = settings;
                        //    }

                        //    if (outputFileName == null || System.IO.File.Exists(outputFileName))
                        //    {
                        //        messaging.SendEmail(connection, request.EventArgumentList, agentEmail, clientId);

                        //        // Mah Log
                        //        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
                        //        // Mah Log
                        //    }
                        //}

                        //if (ReplaceAgentValues && !string.IsNullOrEmpty(OldFile) && OldFile.Trim() != "" && optionGrid != null)
                        //{
                        //    optionGrid.FileName = OldFile;
                        //}

                        SendAgentMail(request, clientId, list[index] as AriaDataObjectPointer, settings, connection, messaging, outputFileName, optionGrid, OldFile, NewOutPutFile, OldOutPutFile, ReplaceAgentValues);
                        //SAB 01-30-2014 Fix event request [End]
                    }

                    request.Occurence += 1;

                    dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                                new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);

                    // Mah Log
                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Update Request Status", new AriaLogElement("Status", request.Status.ToString()));
                    // Mah Log
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


                    //SAB 02/10/2013 [Start]
                    //// T20100512.0026 Hassan.I 20-05-2010 [Begin]
                    ////object[] argumentsArray = new object[request.MethodArgumentList.Count + 1];
                    //object[] argumentsArray = new object[request.MethodArgumentList.Count + 2];
                    //// T20100512.0026 Hassan.I 20-05-2010 [End]
                    //argumentsArray[0] = request.RequestID.ToString();
                    //for (int index = 1; index < argumentsArray.Length - 1; index++)
                    //{
                    //    argumentsArray[index] = (object)((AriaArgument)request.MethodArgumentList[index - 1]).Value;
                    //}
                    //// T20100512.0026 Hassan.I 20-05-2010 [Begin]
                    //try
                    //{ argumentsArray[argumentsArray.Length - 1] = clientId.ToString(); }
                    //catch (Exception ex)
                    //{ argumentsArray[argumentsArray.Length - 1] = ""; }
                    //// T20100512.0026 Hassan.I 20-05-2010 [End]
                    object[] argumentsArray = new object[request.MethodArgumentList.Count + 6];
                    argumentsArray[0] = request.RequestID.ToString();
                    argumentsArray[1] = (object)((AriaArgument)request.MethodArgumentList[0]).Value;
                    try
                    {
                        argumentsArray[2] = clientId.ToString();
                    }
                    catch (Exception)
                    {
                        argumentsArray[2] = "";
                    }
                    //SAB 02/10/2013 [End]

                    GetRequestMethodArgumentsSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList, clientId);
                    connection.Context = request.Context;
                    connection.CompanyName = request.Context.CompanyName;
                    connection.Context.UserName = request.UserName;
                    connection.Context.MethodName = request.MethodName;

                    AriaReflector reflector = new AriaReflector();

                    argumentsArray[1] = ((Aria.DataTypes.AriaOptionGridXmlDataSet)argumentsArray[1]).FileName;

                    // Mah Log  (SABER)
                    //AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Execute Request for Record", new AriaLogElement("FileName", ((Aria.DataTypes.AriaOptionGridXmlDataSet)argumentsArray[1]).FileName));
                    // Mah Log

                    //SAB 02/10/2013 [Start]
                    //try
                    //{
                    //    if (serverObjectSettings is AriaServerObjectSettings)
                    //        reflector.ExecuteMethod(clientId + "_" + ((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                    //    else if (serverObjectSettings is AriaReportObjectSettings)
                    //        reflector.ExecuteMethod(clientId + "_" + ((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                    //}
                    //catch (Exception)
                    //{
                    //    if (serverObjectSettings is AriaServerObjectSettings)
                    //        reflector.ExecuteMethod(((AriaServerObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);

                    //    else if (serverObjectSettings is AriaReportObjectSettings)
                    //        reflector.ExecuteMethod(((AriaReportObjectSettings)serverObjectSettings).ClassName, request.MethodName, argumentsArray);
                    //}
                    if (serverObjectSettings is AriaServerObjectSettings)
                    {
                        argumentsArray[3] = ((AriaServerObjectSettings)serverObjectSettings).ClassName.Split('.')[0];
                        AriaEnviromentVariables ariaEnv = new AriaEnviromentVariables();
                        argumentsArray[4] = SystemInformation.ComputerName.ToUpper();
                        argumentsArray[5] = instanceName;
                        argumentsArray[6] = ariaEnv.RpcPort;
                        //SAB 04-03-2014 Add validation on the Execution Time [Start]
                        DateTime startTime = DateTime.Now;
                        //SAB 04-03-2014 Add validation on the Execution Time [End]
                        reflector.ExecuteMethod("RequestHandler.RequestHandler", request.MethodName, argumentsArray);
                        //SAB 04-03-2014 Add validation on the Execution Time [Start]
                        ValidateExectionTime(startTime, request, messaging, connection, clientId);
                        //SAB 04-03-2014 Add validation on the Execution Time [End]
                    }
                    else if (serverObjectSettings is AriaReportObjectSettings)
                    {
                        argumentsArray[3] = ((AriaReportObjectSettings)serverObjectSettings).ClassName.Split('.')[0];
                        AriaEnviromentVariables ariaEnv = new AriaEnviromentVariables();
                        argumentsArray[4] = SystemInformation.ComputerName.ToUpper();
                        argumentsArray[5] = instanceName;
                        argumentsArray[6] = ariaEnv.RpcPort;
                        //SAB 04-03-2014 Add validation on the Execution Time [Start]
                        DateTime startTime = DateTime.Now;
                        //SAB 04-03-2014 Add validation on the Execution Time [End]
                        reflector.ExecuteMethod("RequestHandler.RequestHandler", request.MethodName, argumentsArray);
                        //SAB 04-03-2014 Add validation on the Execution Time [Start]
                        ValidateExectionTime(startTime, request, messaging, connection, clientId);
                        //SAB 04-03-2014 Add validation on the Execution Time [End]
                    }
                    //SAB 02/10/2013 [End]

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

                            // Mah Log
                            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Failed Email", messaging.GetLog());
                            // Mah Log
                        }

                        return;
                    }

                    request.Status = AriaRequestStatusTypes.Completed;

                    if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
                    {
                        if (outputFileName == null || System.IO.File.Exists(outputFileName))
                        {
                            messaging.SendEmail(connection, request.MethodArgumentList, GetRequest(request.RequestID, clientId).CompleteNotification, clientId);

                            // Mah Log
                            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
                            // Mah Log
                        }
                    }

                    // Mah Log
                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Complete Request");
                    // Mah Log
                }
                # endregion Schedule Request Handeling


            }
            catch (Exception ex)
            {
                EventLog.WriteEntry("Catch INVOKE", ex.Message.ToString(), EventLogEntryType.Error);
                request.Error = ex;
                request.Status = AriaRequestStatusTypes.Failed;

                //HIA  T20140226.0012 - How to edit or adjust the current requests on RB (Nina McLemore, Inc)  2015-01-10 [Begin]
                if (request.RecurrenceType != AriaRequestRecurrenceTypes.Immediate)
                {
                    request.Status = AriaRequestStatusTypes.OnHold;
                    if (_failedRequest.ContainsKey(request.RequestNumber))
                    {
                       
                        _failedRequest[request.RequestNumber] = _failedRequest[request.RequestNumber] + 1;
                        AriaEnviromentVariables ariaEnviromentVariables = new AriaEnviromentVariables();

                        if (_failedRequest[request.RequestNumber] >= ariaEnviromentVariables.FailedRequestRetryNumber)
                        {
                            _failedRequest.Remove(request.RequestNumber);
                            request.Status = AriaRequestStatusTypes.Failed; }

                    }
                    else _failedRequest.Add(request.RequestNumber, 1);
                }
                //HIA  T20140226.0012 - How to edit or adjust the current requests on RB (Nina McLemore, Inc) [End]


                if (request.ErrorNotification != null && request.ErrorNotification.IsValid())
                {
                    if (request.EventObjectName != null && request.EventObjectName.Trim().Length != 0)
                    {
                        messaging.SendEmail(connection, request.EventArgumentList, GetRequest(request.RequestID, clientId).ErrorNotification, clientId);

                        // Mah Log
                        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Failed Email", messaging.GetLog());
                        // Mah Log
                    }
                    else
                    {
                        messaging.SendEmail(connection, request.MethodArgumentList, GetRequest(request.RequestID, clientId).ErrorNotification, clientId);

                        // Mah Log
                        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Failed Email", messaging.GetLog());
                        // Mah Log
                    }
                }
            }
            #endregion
            #region New Code
            //try
            //{
            //    Boolean RequestCaseHandled = false;

            //    AriaObjectDictionaryDBCentric objectDictionaryDBCentric = new AriaObjectDictionaryDBCentric();
            //    AriaObjectRevision objectRevision = objectDictionaryDBCentric.LoadActiveRevision(connection, request.MethodObjectName, clientId);
            //    AriaObjectRevisionSettings serverObjectSettings = objectRevision.ObjectRevisionSettings;

            //    AriaEnviromentVariables ariaEnv = new AriaEnviromentVariables();
            //    string computerName = SystemInformation.ComputerName.ToUpper();
            //    string portNumber = ariaEnv.RpcPort.ToString();

            //    Aria.DataTypes.AriaOptionGridXmlDataSet optionGrid = null;
            //    string OldFile = null, NewOutPutFile = null, OldOutPutFile = null;
            //    bool ReplaceAgentValues = false;

            //    //Immediate Request Handeling ////////////////////////////////////////////////////////////////////////////////
            //    if (request.RecurrenceType == AriaRequestRecurrenceTypes.Immediate && RequestCaseHandled == false)
            //    {
            //        #region Immediate Request Handeling
            //        RequestCaseHandled = true;
            //        request.Status = AriaRequestStatusTypes.Running;

            //        // Call the ExecuteRequest method
            //        ExecuteRequest("objectName", serverObjectSettings, new Guid(request.RequestID), "xmlFileName", request.MethodName, computerName, instanceName, portNumber, request.MethodArgumentList, clientId, connection);

            //        request.Status = AriaRequestStatusTypes.Completed;

            //        if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
            //        {
            //            if (outputFileName == null || System.IO.File.Exists(outputFileName))
            //            {
            //                messaging.SendEmail(connection, request.MethodArgumentList, GetRequest(request.RequestID, clientId).CompleteNotification, clientId);

            //                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
            //            }
            //        }

            //        request.Occurence++;
            //        dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request, new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);

            //        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Complete Request");
            //        #endregion
            //    }
            //    //Event Request Handeling ////////////////////////////////////////////////////////////////////////////////
            //    else if (request.EventName.Trim().Length > 0 && RequestCaseHandled == false)
            //    {
            //        #region Event Request Handeling
            //        RequestCaseHandled = true;

            //        // Call the ExecuteRequest method
            //        if (string.IsNullOrEmpty(request.MethodObjectName) == false)
            //        {
            //            ExecuteRequest("objectName", serverObjectSettings, new Guid(request.RequestID), "xmlFileName", request.MethodName, computerName, instanceName, portNumber, request.MethodArgumentList, clientId, connection);
            //        }

            //        request.Status = AriaRequestStatusTypes.Completed;

            //        GetRequestEventArgumentsSettings(request.EventObjectName, request.EventName, request.EventArgumentList, clientId);

            //        if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
            //        {
            //            if (outputFileName == null || System.IO.File.Exists(outputFileName))
            //            {
            //                messaging.SendEmail(connection, request.EventArgumentList, GetRequest(request.RequestID, clientId).CompleteNotification, clientId);

            //                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
            //            }
            //        }

            //        request.Occurence += 1;

            //        dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request, new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);

            //        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Complete Request");
            //        #endregion
            //    }
            //    //Agent Request Handeling ////////////////////////////////////////////////////////////////////////////////
            //    else if (request.EventName.Trim().Length == 0 && request.EventObjectName.Length != 0 && RequestCaseHandled == false)
            //    {
            //        RequestCaseHandled = true;
            //        request.Status = AriaRequestStatusTypes.Completed;

            //        //// Get the data set as pointers to each record, each pointer contins the Primiary key Only
            //        ArrayList list = GetAgentRequests(new AriaDbConnection(request.Context.CustomerName, request.Context.CompanyName), request.EventObjectName, request.EventConditionList, clientId, request.RequestID);

            //        AriaEnviromentVariables env = new AriaEnviromentVariables();
            //        if (env.AriaMaxRecordsPerAgent != 0 && list.Count > env.AriaMaxRecordsPerAgent)
            //        {
            //            throw new Exception("Agent request has exceeded the maximum number of allowed records '" + env.AriaMaxRecordsPerAgent.ToString() + "'.");
            //        }

            //        AriaObjectDictionaryDBCentric objecDictionary = new AriaObjectDictionaryDBCentric();

            //        AriaDataObjectPointerSettings settings = new AriaDataObjectPointerSettings();
            //        settings.DataObjectName = request.EventObjectName;
            //        settings.DataObjectRevision = objecDictionary.LoadActiveRevision(connection, request.EventObjectName, clientId).ObjectRevision;

            //        for (int index = 0; index < list.Count; index++)
            //        {
            //            if (request.MethodObjectName != null && request.MethodObjectName.Trim() != "")
            //            {
            //                // Call the ExecuteRequest method
            //                ExecuteRequest("objectName", serverObjectSettings, new Guid(request.RequestID), "xmlFileName", request.MethodName, computerName, instanceName, portNumber, request.MethodArgumentList, clientId, connection);
            //            }
            //            AriaEmail agentEmail = GetRequest(request.RequestID, clientId).CompleteNotification;

            //            if (ReplaceAgentValues)
            //            {

            //                if (NewOutPutFile != null && NewOutPutFile.Trim() != "")
            //                {
            //                    bool containOld = false;
            //                    if (agentEmail.Attachment.ContainsKey(OldOutPutFile))
            //                    {
            //                        agentEmail.Attachment.Remove(OldOutPutFile);
            //                        containOld = true;
            //                    }

            //                    agentEmail.Attachment[NewOutPutFile] = NewOutPutFile;


            //                    try
            //                    {
            //                        if (agentEmail != null && agentEmail.IsValid())
            //                        {
            //                            request.EventArgumentList = new AriaArgumentList();
            //                            request.EventArgumentList.AddArgument("Pointer", (AriaDataType)list[index]);
            //                            request.EventArgumentList.BusinessObjectParameterName = "Pointer";
            //                            ((AriaArgument)request.EventArgumentList[0]).Settings = settings;
            //                        }

            //                        if (NewOutPutFile == null || System.IO.File.Exists(NewOutPutFile))
            //                        {
            //                            messaging.SendEmail(connection, request.EventArgumentList, agentEmail, clientId);

            //                            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
            //                        }
            //                    }
            //                    catch (Exception ex)
            //                    {
            //                        throw ex;
            //                    }


            //                    agentEmail.Attachment.Remove(NewOutPutFile);

            //                    if (containOld)
            //                    {
            //                        agentEmail.Attachment.Add(OldOutPutFile, OldOutPutFile);
            //                    }
            //                }
            //                else
            //                {
            //                    if (agentEmail != null && agentEmail.IsValid())
            //                    {
            //                        request.EventArgumentList = new AriaArgumentList();
            //                        request.EventArgumentList.AddArgument("Pointer", (AriaDataType)list[index]);
            //                        request.EventArgumentList.BusinessObjectParameterName = "Pointer";
            //                        ((AriaArgument)request.EventArgumentList[0]).Settings = settings;
            //                    }

            //                    if (outputFileName == null || System.IO.File.Exists(outputFileName))
            //                    {
            //                        messaging.SendEmail(connection, request.EventArgumentList, agentEmail, clientId);

            //                        //AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());

            //                    }
            //                }
            //            }
            //            else
            //            {
            //                if (agentEmail != null && agentEmail.IsValid())
            //                {
            //                    request.EventArgumentList = new AriaArgumentList();
            //                    request.EventArgumentList.AddArgument("Pointer", (AriaDataType)list[index]);
            //                    request.EventArgumentList.BusinessObjectParameterName = "Pointer";
            //                    ((AriaArgument)request.EventArgumentList[0]).Settings = settings;
            //                }

            //                if (outputFileName == null || System.IO.File.Exists(outputFileName))
            //                {
            //                    messaging.SendEmail(connection, request.EventArgumentList, agentEmail, clientId);

            //                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
            //                }
            //            }

            //            if (ReplaceAgentValues && !string.IsNullOrEmpty(OldFile) && OldFile.Trim() != "" && optionGrid != null)
            //            {
            //                optionGrid.FileName = OldFile;
            //            }
            //        }
            //        request.Occurence += 1;

            //        dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request, new string[] { "Status", "Occurence" }, @"RequestId = @RequestId", clientId);

            //        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Update Request Status", new AriaLogElement("Status", request.Status.ToString()));

            //    }
            //    //Schedule Request Handeling ////////////////////////////////////////////////////////////////////////////////
            //    else if (request.EventName.Trim().Length == 0 && ((request.EventConditionList == null) || (request.EventConditionList.Items.Count < 0)) && RequestCaseHandled == false)
            //    {
            //        #region Schedule Request Handeling
            //        RequestCaseHandled = true;

            //        // Call the ExecuteRequest method
            //        ExecuteRequest("objectName", serverObjectSettings, new Guid(request.RequestID), "xmlFileName", request.MethodName, computerName, instanceName, portNumber, request.MethodArgumentList, clientId, connection);

            //        request.Status = AriaRequestStatusTypes.Completed;

            //        if (request.CompleteNotification != null && request.CompleteNotification.IsValid())
            //        {
            //            if (outputFileName == null || System.IO.File.Exists(outputFileName))
            //            {
            //                messaging.SendEmail(connection, request.MethodArgumentList, GetRequest(request.RequestID, clientId).CompleteNotification, clientId);

            //                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
            //            }
            //        }

            //        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Complete Request");
            //        #endregion
            //    }

            //}
            //catch (Exception ex)
            //{
            //    request.Error = ex;

            //    request.Status = AriaRequestStatusTypes.Failed;

            //    if (request.ErrorNotification != null && request.ErrorNotification.IsValid())
            //    {
            //        if (request.EventObjectName != null && request.EventObjectName.Trim().Length != 0)
            //        {
            //            messaging.SendEmail(connection, request.EventArgumentList, GetRequest(request.RequestID, clientId).ErrorNotification, clientId);

            //            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Failed Email", messaging.GetLog());
            //        }
            //        else
            //        {
            //            messaging.SendEmail(connection, request.MethodArgumentList, GetRequest(request.RequestID, clientId).ErrorNotification, clientId);

            //            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Failed Email", messaging.GetLog());
            //        }
            //    }
            //}
            #endregion
            //SAB 01-30-2014 Fix event request [End]
            EventLog.WriteEntry("3", "Derby", EventLogEntryType.Information);

            if (request.Error == null)
            {
                dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                            new string[] { "Status", "NextChildRequestDateTime", "Occurence" }, @"RequestId = @RequestId", clientId);

                // Mah Log
                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Update Request Status", new AriaLogElement("Status", request.Status.ToString()));
                // Mah Log
            }
            else
            {
                dataProvider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request,
                                            new string[] { "Status", "NextChildRequestDateTime", "Occurence", "Error" }, @"RequestId = @RequestId AND Status = 'Running'", clientId);

                // Mah Log
                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Update Request Status", new AriaLogElement("Status", request.Status.ToString()), new AriaLogElement("Error", request.Error.Message));
                // Mah Log
            }
        }

        public void UpdateObjectProgress(string requestId, AriaRequestProgress progress, string clientId)
        {
            //MessageBox.Show("Hi");
            EventLog.WriteEntry("AriaAgent", "1", EventLogEntryType.Information);
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

            //MAH Log
            EventLog.WriteEntry("AriaAgent", "1", EventLogEntryType.Information);
            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Update Request Progress", new AriaLogElement("Status", request.Status.ToString()), new AriaLogElement("Description", progress.Description), new AriaLogElement("Percent", progress.Percent.ToString()));
            //MAH Log
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

            //MAH Log
            if (error != null && error.Trim() != "")
            {
                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Update Request Status", new AriaLogElement("Status", request.Status.ToString()), new AriaLogElement("Error", error));
            }
            else
            {
                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Update Request Status", new AriaLogElement("Status", request.Status.ToString()));
            }
            //MAH Log
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

            EventLog.WriteEntry("DataPath", DataPath, EventLogEntryType.Information); 
            string parentDataPath = DataPath.Substring(0, DataPath.LastIndexOf('.'));
            EventLog.WriteEntry("parentDataPath", parentDataPath, EventLogEntryType.Information); 
            string propertName = DataPath.Substring(DataPath.LastIndexOf('.') + 1);
            EventLog.WriteEntry("propertName", propertName, EventLogEntryType.Information); 
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaObjectRevision objectRevision = (AriaObjectRevision)objectDictionary.LoadActiveRevision(new AriaDbConnection("", ""), parentDataPath, clientId);
            AriaObjectProperty property = objectDictionary.LoadAriaObjectProperty(new AriaDbConnection("", ""), parentDataPath, objectRevision.ObjectRevision, propertName, clientId);
            EventLog.WriteEntry("property", property.ToString(), EventLogEntryType.Information); 
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
                fieldName = ((AriaRelatedFieldSettings)property.PropertySettings).FieldName;
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
            EventLog.WriteEntry("fieldName", fieldName, EventLogEntryType.Information); 
            return fieldName;
           
            // T20110803.0001 MAH 8/2/2011 End
        }

        private string GetCondition(AriaDbCommand command, int conditionNo, string objectName, AriaCondition condition, string clientId)
        {
            EventLog.WriteEntry("Start Command", "Sara", EventLogEntryType.Information);

            if (command == null)
            {
                EventLog.WriteEntry("Start is null value", "Command is null", EventLogEntryType.Information);
            }

            else  
            {
                if (command.CommandText == null)
                { 
                    EventLog.WriteEntry("command.CommandText", "null", EventLogEntryType.Information); 
                }
            }

            EventLog.WriteEntry("Start Obj Name", objectName, EventLogEntryType.Information);

            if (condition == null)
            { 
                
                EventLog.WriteEntry("Condition LeftHandSide", "Null", EventLogEntryType.Information);
            }

            else
            {
                if (condition.LeftHandSide.Value == null)
                {
                    EventLog.WriteEntry("Start Condition LeftHandSide", "Null", EventLogEntryType.Information);
                }
                else { EventLog.WriteEntry("condition.LeftHandSide.Value", condition.LeftHandSide.Value.ToString(), EventLogEntryType.Information); }

                if (condition.LeftHandSide.PropertyDataPathDictionary == null)
                {
                    EventLog.WriteEntry("condition.LeftHandSide.PropertyDataPathDictionary", "Null", EventLogEntryType.Information);
                }

                else { EventLog.WriteEntry("condition.LeftHandSide.PropertyDataPathDictionary", condition.LeftHandSide.PropertyDataPathDictionary.ToString(), EventLogEntryType.Information); }
            }
            
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
            EventLog.WriteEntry("Start result", result, EventLogEntryType.Information);
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
                    if (command != null) command.Parameters.Add(new AriaDbParameter("Param" + conditionNo.ToString().TrimEnd() + "_1", from.Value));
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
            EventLog.WriteEntry("Start result finally", result, EventLogEntryType.Information);
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
                EventLog.WriteEntry("AriaObject Pro ObjectID", ((AriaObjectProperty)rootObjectProperties[index]).ObjectID.ToString(), EventLogEntryType.Information);
                 EventLog.WriteEntry("AriaObject PropertyName", ((AriaObjectProperty)rootObjectProperties[index]).PropertyName.ToString(), EventLogEntryType.Information);
                 EventLog.WriteEntry("AriaObject PropertyName", ((AriaObjectProperty)rootObjectProperties[index]).PropertyType.ToString(), EventLogEntryType.Information);
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

        public ArrayList GetAgentRequests(AriaDbConnection connection, string objctName, AriaConditionList conditions, string clientId, string requestID)
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
                    //SAB 12/13/2012 [Start]
                    //// T20110803.0001 MAH 8/2/2011
                    //// sqlCommandText += " LEFT JOIN " + childObjectRevisionSettings.TableName + " AS " + childObjectParents[childObjectParents.Length - 1];
                    ////sqlCommandText += " LEFT JOIN " + childObjectRevisionSettings.TableName + " AS [" + childObject.ObjectName.Trim() + "]";
                    //sqlCommandText += " FROM OPENROWSET('MSDASQL','" + env.GetAria04CompanyDataConnectionStringODBC(connection.CompanyName) +
                    //                                                "','SELECT * FROM " + childObjectRevisionSettings.TableName
                    //                                                    + "')" + " AS [" + childObject.ObjectName.Trim() + "]";

                    //// T20110803.0001 MAH 8/2/2011 End
                    sqlCommandText += " LEFT JOIN OPENROWSET('MSDASQL','" + env.GetAria04CompanyDataConnectionStringODBC(connection.CompanyName) +
                                                                    "','SELECT " + GetKeyAndFilterFields(connection, childObject.ObjectName, clientId, true, conditionsString) + " FROM " + childObjectRevisionSettings.TableName
                                                                        + "')" + " AS [" + childObject.ObjectName.Trim() + "]";
                    //SAB 12/13/2012 [End]
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

                for (int j = 0; j < mainFields.Count; j++)
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


            List<AriaLogElement> paramsList = new List<AriaLogElement>();

            paramsList.Add(new AriaLogElement("SelectCommand", sqlCommandText));
            for (int i = 0; i < dbCommand.Parameters.Count; i++)
            {
                paramsList.Add(new AriaLogElement(dbCommand.Parameters[i].ParameterName, dbCommand.Parameters[i].ParameterValue.ToString()));
            }

            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(requestID), "Get Request Matached Records", paramsList.ToArray());

            DataTable table = dbCommand.GetDataTable();

            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(requestID), "Number of Request Matached Records", new AriaLogElement("RecordsCount", table.Rows.Count.ToString()));

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
            EventLog.WriteEntry("GetRequest", "1", EventLogEntryType.Information);
            string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";
            EventLog.WriteEntry("GetRequest", "2", EventLogEntryType.Information);
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            EventLog.WriteEntry("GetRequest", "3", EventLogEntryType.Information);
            AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            EventLog.WriteEntry("GetRequest", "4", EventLogEntryType.Information);
            command.Parameters.Add(new AriaDbParameter("RequestID", requestId));
            EventLog.WriteEntry("GetRequest", "5", EventLogEntryType.Information);
            AriaDataProvider dataProvider = new AriaDataProvider();
            EventLog.WriteEntry("GetRequest", "6", EventLogEntryType.Information);
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

        //SAB 01-30-2014 Fix event request [Start]
        #region Canceled Method
        //private void ExecuteRequest(string objectName, AriaObjectRevisionSettings objectSettings, Guid requestGuid, string xmlFileName, string methodName, string rpcComputerName, string rpcInstanceName, string rpcPort, AriaArgumentList argumentList, string clientId, AriaDbConnection connection)
        //{
        //    AriaMessagingManager messaging = new AriaMessagingManager();

        //    //SAB AriaObjectDictionaryDBCentric objectDictionaryDBCentric = new AriaObjectDictionaryDBCentric();
        //    //SAB AriaObjectRevision objectRevision = objectDictionaryDBCentric.LoadActiveRevision(connection, request.MethodObjectName, clientId);
        //    //SAB AriaObjectRevisionSettings serverObjectSettings = objectRevision.ObjectRevisionSettings;

        //    object[] argumentsArray = new object[argumentList.Count + 6]; //SAB request.MethodArgumentList.Count + 6];
        //    argumentsArray[0] = requestGuid.ToString(); //SAB request.RequestID.ToString();
        //    argumentsArray[1] = (object)((AriaArgument)argumentList[0]).Value; //SAB request.MethodArgumentList[0]).Value;
        //    try
        //    {
        //        argumentsArray[2] = clientId.ToString();
        //    }
        //    catch (Exception)
        //    {
        //        argumentsArray[2] = "";
        //    }

        //    //GenerateOptionGridXmlFileForObjectKey();

        //    //SAB GetRequestMethodArgumentsSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList, clientId);
        //    //SAB connection.Context = request.Context;
        //    //SAB connection.CompanyName = request.Context.CompanyName;
        //    //SAB connection.Context.UserName = request.UserName;
        //    //SAB connection.Context.MethodName = request.MethodName;

        //    AriaReflector reflector = new AriaReflector();

        //    argumentsArray[1] = ((Aria.DataTypes.AriaOptionGridXmlDataSet)argumentsArray[1]).FileName;

        //    if (objectSettings is AriaServerObjectSettings) //SAB (serverObjectSettings is AriaServerObjectSettings)
        //    {
        //        argumentsArray[3] = ((AriaServerObjectSettings)objectSettings).ClassName.Split('.')[0]; //SAB serverObjectSettings).ClassName.Split('.')[0];
        //        //AriaEnviromentVariables ariaEnv = new AriaEnviromentVariables();
        //        //argumentsArray[4] = rpcComputerName;   //SAB SystemInformation.ComputerName.ToUpper();
        //        //argumentsArray[5] = rpcInstanceName;   //SAB instanceName;
        //        //argumentsArray[6] = rpcPort;           //SAB ariaEnv.RpcPort;
        //        //AriaDatabaseLogManager.AriaAddToLog(clientId, connection, requestGuid, "ExecuteMethod Parameters <RequestHandler.RequestHandler>, <" + methodName + ">, <" + argumentsArray[3] + ">"); //SAB new Guid(request.RequestID), "ExecuteMethod Parameters <RequestHandler.RequestHandler>, <" + request.MethodName.ToString() + ">, <" + argumentsArray[3] + ">");
        //        //reflector.ExecuteMethod("RequestHandler.RequestHandler", methodName, argumentsArray); //SAB request.MethodName, argumentsArray);
        //    }
        //    else if (objectSettings is AriaReportObjectSettings) //SAB(serverObjectSettings is AriaReportObjectSettings)
        //    {
        //        argumentsArray[3] = ((AriaReportObjectSettings)objectSettings).ClassName.Split('.')[0]; //SAB serverObjectSettings).ClassName.Split('.')[0];
        //        //AriaEnviromentVariables ariaEnv = new AriaEnviromentVariables();
        //        //argumentsArray[4] = rpcComputerName;   //SAB SystemInformation.ComputerName.ToUpper();
        //        //argumentsArray[5] = rpcInstanceName;   //SAB instanceName;
        //        //argumentsArray[6] = rpcPort;           //SAB ariaEnv.RpcPort;
        //        //AriaDatabaseLogManager.AriaAddToLog(clientId, connection, requestGuid, "ExecuteMethod Parameters <RequestHandler.RequestHandler>, <" + methodName + ">, <" + argumentsArray[3] + ">"); //SAB new Guid(request.RequestID), "ExecuteMethod Parameters <RequestHandler.RequestHandler>, <" + request.MethodName.ToString() + ">, <" + argumentsArray[3] + ">");
        //        //reflector.ExecuteMethod("RequestHandler.RequestHandler", methodName, argumentsArray); //SAB request.MethodName, argumentsArray);
        //    }
        //    argumentsArray[4] = rpcComputerName;   //SAB SystemInformation.ComputerName.ToUpper();
        //    argumentsArray[5] = rpcInstanceName;   //SAB instanceName;
        //    argumentsArray[6] = rpcPort;           //SAB ariaEnv.RpcPort;
        //    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, requestGuid, "ExecuteMethod Parameters <RequestHandler.RequestHandler>, <" + methodName + ">, <" + argumentsArray[3] + ">"); //SAB new Guid(request.RequestID), "ExecuteMethod Parameters <RequestHandler.RequestHandler>, <" + request.MethodName.ToString() + ">, <" + argumentsArray[3] + ">");
        //    reflector.ExecuteMethod("RequestHandler.RequestHandler", methodName, argumentsArray); //SAB request.MethodName, argumentsArray);


        //    string AriaDbCommandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";

        //    AriaDbCommand command = new AriaDbCommand(AriaDbCommandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
        //    command.Parameters.Add(new AriaDbParameter("RequestID", requestGuid)); //SAB request.RequestID));

        //    AriaDataProvider oldRequestDataProvider = new AriaDataProvider();

        //    AriaRequest oldRequest = (AriaRequest)oldRequestDataProvider.GetObjectList(command, typeof(AriaRequest))[0];
        //    if (oldRequest.Status == AriaRequestStatusTypes.Canceled)
        //    {
        //        return;
        //    }

        //    if (oldRequest.Status == AriaRequestStatusTypes.Failed)
        //    {
        //        if (oldRequest.ErrorNotification != null && oldRequest.ErrorNotification.IsValid())
        //        {
        //            messaging.SendEmail(connection, oldRequest.MethodArgumentList, oldRequest.ErrorNotification, clientId);

        //            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, requestGuid, "Send Failed Email", messaging.GetLog()); //SAB new Guid(request.RequestID), "Send Failed Email", messaging.GetLog());
        //        }

        //        return;
        //    }
        //}                
        #endregion

        string GenerateOptionGridXmlFileForObjectKey(object[] argumentsArray, AriaDataObjectPointer dataObjectPointer, AriaObjectRevision rootObjectRevision, out AriaOptionGridXmlDataSet optionGrid, out string OldFile, out string NewOutPutFile, out string OldOutPutFile, out bool ReplaceAgentValues)
        {
            optionGrid = null;
            OldFile = null;
            NewOutPutFile = null;
            OldOutPutFile = null;
            ReplaceAgentValues = false;

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
                        //if (list[index] is Aria.DataTypes.AriaDataObjectPointer)
                        //{
                        //    Aria.DataTypes.AriaDataObjectPointer keyvalue = list[index] as Aria.DataTypes.AriaDataObjectPointer;
                        foreach (object objpointer in dataObjectPointer.KeyFields)
                        {
                            Aria.DataTypes.AriaDataObjectPointerKeyField pointer = objpointer as Aria.DataTypes.AriaDataObjectPointerKeyField;
                            if (pointer.FieldName.ToUpper().Trim() == oldvalue.ToUpper().Trim())
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
                        //}
                        lcRpExp.InnerText = lcRpExp.InnerText.Replace("{" + oldvalue + "}", "'" + NewValue + "'");
                        //AriaObjectRevision rootObjectRevision = (AriaObjectRevision)objecDictionary.LoadActiveRevision(connection, request.EventObjectName, clientId);
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
                            //SAB 01-31-2013 [Start]
                            //NewNode["Value"].InnerText = "<?xml version=\"1.0\"?> <xdoc> <" + NewValue + "> <row> <keyexp>" + PointerValue + "</keyexp> <" + PointerName + ">" + PointerValue + "</" + PointerName + "> </row> </" + NewValue + "> </xdoc>";
                            NewNode["Value"].InnerText = "<?xml version=\"1.0\"?> <xdoc> <" + NewValue + "> <row> <keyexp>" + PointerValue + "</keyexp> <" + PointerName.ToLower() + ">" + PointerValue + "</" + PointerName.ToLower() + "> </row> </" + NewValue + "> </xdoc>";
                            //SAB 01-31-2013 [End]
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
            return "";
        }

        void SendAgentMail(AriaRequest request, string clientId, AriaDataObjectPointer dataObjectPointer, AriaDataObjectPointerSettings pointerSetting, AriaDbConnection connection, AriaMessagingManager messaging, string outputFileName, AriaOptionGridXmlDataSet optionGrid, string OldFile, string NewOutPutFile, string OldOutPutFile, bool ReplaceAgentValues) //SAB, out AriaOptionGridXmlDataSet optionGrid
        {
            AriaEmail agentEmail = GetRequest(request.RequestID, clientId).CompleteNotification;


            if (ReplaceAgentValues)
            {

                if (NewOutPutFile != null && NewOutPutFile.Trim() != "")
                {
                    bool containOld = false;
                    if (agentEmail.Attachment.ContainsKey(OldOutPutFile))
                    {
                        agentEmail.Attachment.Remove(OldOutPutFile);
                        containOld = true;
                    }

                    agentEmail.Attachment[NewOutPutFile] = NewOutPutFile;


                    try
                    {
                        if (agentEmail != null && agentEmail.IsValid())
                        {
                            request.EventArgumentList = new AriaArgumentList();
                            request.EventArgumentList.AddArgument("Pointer", (AriaDataType)dataObjectPointer);//SAB list[index]);
                            request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                            ((AriaArgument)request.EventArgumentList[0]).Settings = pointerSetting; //SAB settings;
                        }

                        if (NewOutPutFile == null || System.IO.File.Exists(NewOutPutFile))
                        {
                            messaging.SendEmail(connection, request.EventArgumentList, agentEmail, clientId);

                            // Mah Log
                            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
                            // Mah Log
                        }
                    }
                    catch (Exception ex)
                    {
                        throw ex;
                    }


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
                        request.EventArgumentList.AddArgument("Pointer", (AriaDataType)dataObjectPointer); //SAB list[index]);
                        request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                        ((AriaArgument)request.EventArgumentList[0]).Settings = pointerSetting; //SAB settings;
                    }

                    if (outputFileName == null || System.IO.File.Exists(outputFileName))
                    {
                        messaging.SendEmail(connection, request.EventArgumentList, agentEmail, clientId);

                        // Mah Log
                        // SABER
                        //AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
                        // Mah Log

                    }
                }
            }
            else
            {
                if (agentEmail != null && agentEmail.IsValid())
                {
                    request.EventArgumentList = new AriaArgumentList();
                    request.EventArgumentList.AddArgument("Pointer", (AriaDataType)dataObjectPointer);//SAB list[index]);
                    request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                    ((AriaArgument)request.EventArgumentList[0]).Settings = pointerSetting; //SAB settings;
                }

                if (outputFileName == null || System.IO.File.Exists(outputFileName))
                {
                    messaging.SendEmail(connection, request.EventArgumentList, agentEmail, clientId);

                    // Mah Log
                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
                    // Mah Log
                }
            }

            if (ReplaceAgentValues && !string.IsNullOrEmpty(OldFile) && OldFile.Trim() != "" && optionGrid != null)
            {
                optionGrid.FileName = OldFile;
            }
        }
        //SAB 01-30-2014 Fix event request [Start]

        //SAB 04-03-2014 Add validation on the Execution Time [Start]
        private void ValidateExectionTime(DateTime startExecTime, AriaRequest request, AriaMessagingManager messaging, AriaDbConnection connection, string clientId)
        {
            double execSeconds = DateTime.Now.Subtract(startExecTime).TotalSeconds;
            AriaEnviromentVariables env = new AriaEnviromentVariables(clientId);
            double maxExecSeconds = (env.ClientMaxExecutionTimePerRequest != null && env.ClientMaxExecutionTimePerRequest != 0) ? env.ClientMaxExecutionTimePerRequest : (env.MaxExecutionTimePerRequest != null && env.MaxExecutionTimePerRequest != 0) ? env.MaxExecutionTimePerRequest : 600;
            if (execSeconds > maxExecSeconds)
            {
                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Max Execution Time per Request exceeded", new AriaLogElement("Request Number", request.RequestID), new AriaLogElement("Execution Time", execSeconds.ToString()));

                if (!string.IsNullOrEmpty(env.ExceedLimitNotificationEmail) || !string.IsNullOrEmpty(env.ClientExceedLimitNotificationEmail))
                {
                    AriaEmail notifyEmail = new AriaEmail();
                    notifyEmail.To = string.IsNullOrEmpty(env.ExceedLimitNotificationEmail) ? "" : env.ExceedLimitNotificationEmail;
                    notifyEmail.To += ((string.IsNullOrEmpty(notifyEmail.To) || string.IsNullOrEmpty(env.ClientExceedLimitNotificationEmail)) ? "" : notifyEmail.To + ";") + (string.IsNullOrEmpty(env.ClientExceedLimitNotificationEmail) ? "" : env.ClientExceedLimitNotificationEmail);
                    notifyEmail.Subject = "Max Execution Time per Request exceeded <<" + request.RequestNumber + ">>";
                    notifyEmail.Body = "Client = " + clientId + "\nRequest Number = " + request.RequestNumber + "\nExecution Time = " + execSeconds.ToString() + " Seconds";

                    messaging.SendEmail(connection, request.EventArgumentList, notifyEmail, clientId);
                }
            }
        }

        public void ValidateRecordsPerReport(string requestId, string clientId, int numOfRecords)
        {
            EventLog.WriteEntry("ValidateRecordsPerReport", "Start", EventLogEntryType.Error);
            AriaEnviromentVariables env = new AriaEnviromentVariables(clientId);
            double maxReportRecords = (env.ClientMaxRecordsPerReport != null && env.ClientMaxRecordsPerReport != 0) ? env.ClientMaxRecordsPerReport : (env.MaxRecordsPerReport != null && env.MaxRecordsPerReport != 0) ? env.MaxRecordsPerReport : 600;
            EventLog.WriteEntry("ValidateRecordsPerReport", "numOfRecords:" + numOfRecords + "maxReportRecords:" + maxReportRecords, EventLogEntryType.Error);
            if (numOfRecords > maxReportRecords)
            {
                AriaRequest request = GetRequest(requestId, clientId);
                AriaDbConnection connection = new AriaDbConnection();
                AriaMessagingManager messaging = new AriaMessagingManager();
                connection.Context = request.Context;
                connection.CompanyName = request.Context.CompanyName;
                connection.Context.UserName = request.UserName;
                connection.Context.MethodName = request.MethodName;

                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Max Number of Records per Report exceeded", new AriaLogElement("Request Number", request.RequestID), new AriaLogElement("Number of Records", numOfRecords.ToString()));

                if (!string.IsNullOrEmpty(env.ExceedLimitNotificationEmail) || !string.IsNullOrEmpty(env.ClientExceedLimitNotificationEmail))
                {
                    AriaEmail notifyEmail = new AriaEmail();
                    notifyEmail.To = string.IsNullOrEmpty(env.ExceedLimitNotificationEmail) ? "" : env.ExceedLimitNotificationEmail;
                    //notifyEmail.To = (string.IsNullOrEmpty(notifyEmail.To) || string.IsNullOrEmpty(env.ClientExceedLimitNotificationEmail)) ? "" : (notifyEmail.To + ";") + (string.IsNullOrEmpty(env.ClientExceedLimitNotificationEmail) ? "" : env.ClientExceedLimitNotificationEmail);
                    notifyEmail.To += ((string.IsNullOrEmpty(notifyEmail.To) || string.IsNullOrEmpty(env.ClientExceedLimitNotificationEmail)) ? "" : notifyEmail.To + ";") + (string.IsNullOrEmpty(env.ClientExceedLimitNotificationEmail) ? "" : env.ClientExceedLimitNotificationEmail);
                    notifyEmail.Subject = "Max Number of Records per Report exceeded <<" + request.RequestNumber + ">>";
                    notifyEmail.Body = "Client = " + clientId + "\nRequest Number = " + request.RequestNumber + "\nNumber of Records Per Report = " + numOfRecords.ToString();

                    messaging.SendEmail(connection, request.EventArgumentList, notifyEmail, clientId);
                }
            }
        }
        //SAB 04-03-2014 Add validation on the Execution Time [End]

    }
}
