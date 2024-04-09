namespace Aria.EnterpriseServices.RequestHandler
{
    using Aria.Data;
    using Aria.DataTypes;
    using Aria.DataTypes.Messaging;
    using Aria.DataTypes.ObjectDictionary;
    using Aria.DataTypes.ObjectDictionary.Settings.Object;
    using Aria.DataTypes.RequestHandler;
    using Aria.DataTypes.Settings;
    using Aria.EnterpriseServices.Messaging;
    using Aria.EnterpriseServices.ObjectDictionary;
    using Aria.Environment;
    using Aria.Reflection;
    using Aria.Utilities.Log;
    using Aria.Utilities.ParameterSubstitution;
    using Aria.Xml;
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Data;
    using System.Diagnostics;
    using System.Drawing.Printing;
    using System.IO;
    using System.Runtime.InteropServices;
    using System.Xml;

    public class AriaRequestAgent : MarshalByRefObject
    {
        private const string _chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        private readonly Random _rng = new Random();

        public int GetMaxRunningRequest()
        {
            int maxRunningRequest = 0;

            XmlDocument xmlDocument = new XmlDocument();

            try
            {
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

                            if (xmlNode.Name == "MaxRunningRequest")
                            {
                                maxRunningRequest = int.Parse(xmlNode.InnerText);

                                break;
                            }
                        }
                    }
                }
            }
            catch (Exception)
            {
            }

            return maxRunningRequest;
        }

        public int GetRunningRequestCount()
        {
            var processes = Process.GetProcesses();

            int count = 0;

            foreach (var process in processes)
            {
                if (process.ProcessName.ToUpper() == "RequestHandler".ToUpper()) count++;
            }

            return count;
        }


        private void CloneOneTimeOnlyRequest(AriaRequest request, string clientId)
        {
            AriaArgument argument;
            AriaRequest record = new AriaRequest();
            record.SetRequestDateRangeAfterOccurence(request.NextChildRequestDateTime, 1);
            record.Description = request.Description;
            string filename = "";
            string fileName = "";
            string key = "";
            string path = "";
            AriaOptionGridXmlDataSet set = new AriaOptionGridXmlDataSet();
            XmlDocument document = new XmlDocument();
            if ((request.MethodObjectName != null) && (request.MethodArgumentList != null))
            {
                foreach (object obj2 in request.MethodArgumentList)
                {
                    argument = (AriaArgument)obj2;
                    if (argument.Value is AriaOptionGridXmlDataSet)
                    {
                        filename = Path.GetRandomFileName().Replace(".", "") + ".xml";
                        set = argument.Value as AriaOptionGridXmlDataSet;
                        document.Load(set.FileName);
                        path = document.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText;
                        key = Path.GetDirectoryName(path) + @"\" + filename.Replace(".xml", Path.GetExtension(path));
                        document.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText = key;
                        filename = Path.GetDirectoryName(set.FileName) + @"\" + filename;
                        document.Save(filename);
                        fileName = set.FileName;
                        set.FileName = filename;
                    }
                }
            }
            ((AriaArgument)request.MethodArgumentList[0]).Value = set;
            record.SetRequestMethodSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList);
            record.SetRequestLoginSettings(request.LoginType, request.UserName, request.Password);
            record.SetOneTimeOnlyRequestSettings(request.RequestStartTime);
            record.SetRequestContextSettings(request.Context.CompanyName, request.Context.CompanyName);
            record.RepeatTask = request.RepeatTask;
            record.RepeatTaskEvery = request.RepeatTaskEvery;
            record.RepeatTaskUntilDuration = request.RepeatTaskUntilDuration;
            record.RepeatTaskUntillTime = request.RepeatTaskUntillTime;
            record.RepeatTaskUntilType = request.RepeatTaskUntilType;
            record.HoldTaskIfStillRunning = request.HoldTaskIfStillRunning;
            record.EventObjectName = request.EventObjectName;
            record.EventConditionList = request.EventConditionList;
            record.CompleteNotification = request.CompleteNotification;
            if (record.CompleteNotification != null)
            {
                foreach (DictionaryEntry entry in record.CompleteNotification.Attachment)
                {
                    if (((entry.Value.ToString().Trim() != "") && (entry.Value != null)) && (entry.Key.ToString().Trim() == path))
                    {
                        record.CompleteNotification.Attachment.Remove(entry.Key);
                        record.CompleteNotification.Attachment.Add(key, entry.Value);
                        break;
                    }
                }
            }
            record.ErrorNotification = request.ErrorNotification;
            Guid guid = new Guid(request.RequestID);
            record.ParentRequestID = guid.ToString();
            record.ParentRequestNumber = request.RequestNumber;
            AriaDataProvider provider = new AriaDataProvider();
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            provider.InsertObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, record, new string[] { "RequestNumber" }, clientId);
            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Create Request Occurance", new AriaLogElement[] { new AriaLogElement("Date", record.StartAfterDate.ToString()), new AriaLogElement("Time", record.RequestStartTime.ToString()) });
            if (record.RepeatTask)
            {
                ArrayList repeatableNextChildDateTimes = this.GetRepeatableNextChildDateTimes(record);
                for (int i = 0; i < repeatableNextChildDateTimes.Count; i++)
                {
                    AriaRequest request3 = new AriaRequest();
                    request3.SetRequestDateRangeAfterOccurence(request.NextChildRequestDateTime, 1);
                    request3.Description = request.Description;
                    string str5 = "";
                    string str6 = "";
                    string str7 = "";
                    string innerText = "";
                    AriaOptionGridXmlDataSet set2 = new AriaOptionGridXmlDataSet();
                    XmlDocument document2 = new XmlDocument();
                    if ((request.MethodObjectName != null) && (request.MethodArgumentList != null))
                    {
                        foreach (object obj2 in request.MethodArgumentList)
                        {
                            argument = (AriaArgument)obj2;
                            if (argument.Value is AriaOptionGridXmlDataSet)
                            {
                                str5 = Path.GetRandomFileName().Replace(".", "") + ".xml";
                                set2 = argument.Value as AriaOptionGridXmlDataSet;
                                document2.Load(set2.FileName);
                                innerText = document2.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText;
                                str7 = Path.GetDirectoryName(innerText) + @"\" + str5.Replace(".xml", Path.GetExtension(innerText));
                                document2.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText = str7;
                                str5 = Path.GetDirectoryName(set2.FileName) + @"\" + str5;
                                document2.Save(str5);
                                str6 = set2.FileName;
                                set2.FileName = str5;
                            }
                        }
                    }
                    ((AriaArgument)request.MethodArgumentList[0]).Value = set2;
                    request3.SetRequestMethodSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList);
                    request3.SetRequestLoginSettings(request.LoginType, request.UserName, request.Password);
                    request3.SetOneTimeOnlyRequestSettings(request.RequestStartTime);
                    request3.SetRequestContextSettings(request.Context.CompanyName, request.Context.CompanyName);
                    request3.EventObjectName = request.EventObjectName;
                    request3.EventConditionList = request.EventConditionList;
                    request3.CompleteNotification = request.CompleteNotification;
                    if (request3.CompleteNotification != null)
                    {
                        foreach (DictionaryEntry entry in request3.CompleteNotification.Attachment)
                        {
                            if (((entry.Value.ToString().Trim() != "") && (entry.Value != null)) && (entry.Key.ToString().Trim() == innerText))
                            {
                                request3.CompleteNotification.Attachment.Remove(entry.Key);
                                request3.CompleteNotification.Attachment.Add(str7, entry.Value);
                                break;
                            }
                        }
                    }
                    request3.ErrorNotification = request.ErrorNotification;
                    request3.ParentRequestID = new Guid(request.RequestID).ToString();
                    request3.ParentRequestNumber = request.RequestNumber;
                    request3.RequestStartTime = (DateTime)repeatableNextChildDateTimes[i];
                    request3.StartAfterDate = request3.StartAfterDate.AddDays((double)request3.RequestStartTime.Subtract(new DateTime(0x76c, 1, 1)).Days);
                    provider.InsertObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, request3, new string[] { "RequestNumber" }, clientId);
                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Create Request Occurance", new AriaLogElement[] { new AriaLogElement("Date", request3.StartAfterDate.ToString()), new AriaLogElement("Time", request3.RequestStartTime.ToString()) });
                }
            }
        }

        private AriaRequest CloneOneTimeOnlyRequest(AriaRequest request, DateTime startAfterDateTime, string clientId)
        {
            AriaRequest record = new AriaRequest();
            record.SetRequestDateRangeAfterOccurence(startAfterDateTime, 1);
            record.Description = request.Description;
            record.SetRequestLoginSettings(request.LoginType, request.UserName, request.Password);
            record.SetOnEventRequestSettings(request.EventObjectName, request.EventName, request.EventArgumentList, request.EventConditionList);
            record.SetRequestMethodSettings(request.MethodObjectName, request.MethodName, request.MethodArgumentList);
            record.SetOneTimeOnlyRequestSettings(new DateTime(0x76c, 1, 1, startAfterDateTime.Hour, startAfterDateTime.Minute, startAfterDateTime.Second));
            record.ParentRequestID = new Guid(request.RequestID).ToString();
            record.ParentRequestNumber = request.RequestNumber;
            record.SetRequestContextSettings(request.Context.CompanyName, request.Context.CompanyName);
            record.RepeatTask = request.RepeatTask;
            record.RepeatTaskEvery = request.RepeatTaskEvery;
            record.RepeatTaskUntilDuration = request.RepeatTaskUntilDuration;
            record.RepeatTaskUntillTime = request.RepeatTaskUntillTime;
            record.RepeatTaskUntilType = request.RepeatTaskUntilType;
            record.HoldTaskIfStillRunning = request.HoldTaskIfStillRunning;
            record.CompleteNotification = request.CompleteNotification;
            record.ErrorNotification = request.ErrorNotification;
            AriaDataProvider provider = new AriaDataProvider();
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            provider.InsertObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, record, new string[] { "RequestNumber" }, clientId);
            List<AriaLogElement> list = new List<AriaLogElement>();
            if (record.EventArgumentList != null)
            {
                list.Add(new AriaLogElement("Date", record.StartAfterDate.ToString()));
                list.Add(new AriaLogElement("Time", record.RequestStartTime.ToString()));
                for (int i = 0; i < record.EventArgumentList.Count; i++)
                {
                    AriaArgument argument = record.EventArgumentList[0] as AriaArgument;
                    if (argument.Value is AriaDataObjectPointer)
                    {
                        AriaDataObjectPointer pointer = argument.Value as AriaDataObjectPointer;
                        for (int j = 0; j < pointer.KeyFields.Count; j++)
                        {
                            AriaDataObjectPointerKeyField field = pointer.KeyFields[j] as AriaDataObjectPointerKeyField;
                            list.Add(new AriaLogElement(field.FieldName, field.Value.ToString()));
                        }
                    }
                }
            }
            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Create Response to Event Request", list.ToArray());
            return record;
        }

        public void ExecuteImmediateRequest(string requestId, string clientId)
        {
            string commandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RequestID", requestId));
            AriaDataProvider provider = new AriaDataProvider();
            AriaRequest record = (AriaRequest)provider.GetObjectList(command, typeof(AriaRequest))[0];
            record.Status = AriaRequestStatusTypes.Running;
            record.Occurence++;
            provider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", record, new string[] { "Status", "Occurence" }, "RequestId = @RequestId", clientId);
            AriaThreadManager manager = new AriaThreadManager();
            record.clientID = clientId;
            manager.StartThread(record, clientId);
        }

        public void ExecuteImmediateRequests(string clientId)
        {
            string commandText = "SELECT * FROM AriaRequest WHERE RecurrenceType = @RecurrenceType and Status = @OnHoldStatus ";
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RecurrenceType", AriaRequestRecurrenceTypes.Immediate.ToString()));
            command.Parameters.Add(new AriaDbParameter("OnHoldStatus", AriaRequestStatusTypes.OnHold.ToString()));
            AriaDataProvider provider = new AriaDataProvider();
            ArrayList objectList = provider.GetObjectList(command, typeof(AriaRequest));
            for (int i = 0; i < objectList.Count; i++)
            {
                AriaRequest record = (AriaRequest)objectList[i];
                record.Status = AriaRequestStatusTypes.Running;
                record.Occurence++;
                provider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", record, new string[] { "Status", "Occurence" }, "RequestId = @RequestId", clientId);
                AriaThreadManager manager = new AriaThreadManager();
                record.clientID = clientId;
                manager.StartThread(record, clientId);
            }
        }

        public void ExecuteOneTimeOnlyRequests(string clientId)
        {
            // Mahmoud 06/19/2018 Restrict the number of concurrent requests Begin
            string top = "*";
            if (GetMaxRunningRequest() > 0)
            {
                top = Math.Abs(GetMaxRunningRequest() - GetRunningRequestCount()).ToString();
            }

            //string commandText = "SELECT * FROM AriaRequest WHERE StartAfterDate <= @Today AND RequestStartTime <= @Now AND Status = @OnHoldStatus AND RecurrenceType = @Once ";
            string commandText = "SELECT " + top + " FROM AriaRequest WHERE StartAfterDate <= @Today AND RequestStartTime <= @Now AND Status = @OnHoldStatus AND RecurrenceType = @Once ";
            // Mahmoud 06/19/2018 Restrict the number of concurrent requests End

            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("Today", DateTime.Now.Date));
            command.Parameters.Add(new AriaDbParameter("Now", new DateTime(0x76c, 1, 1, DateTime.Now.Hour, DateTime.Now.Minute, DateTime.Now.Second)));
            command.Parameters.Add(new AriaDbParameter("OnHoldStatus", AriaRequestStatusTypes.OnHold.ToString()));
            command.Parameters.Add(new AriaDbParameter("Once", AriaRequestRecurrenceTypes.Once.ToString()));
            AriaDataProvider provider = new AriaDataProvider();
            ArrayList objectList = provider.GetObjectList(command, typeof(AriaRequest));
            for (int i = 0; i < objectList.Count; i++)
            {
                AriaRequest record = (AriaRequest)objectList[i];
                AriaDbCommand command2 = new AriaDbCommand("SELECT * FROM AriaRequest Where RequestID > @RequestID AND ParentRequestID = @ParentRequestID AND StartAfterDate <= @Today AND RequestStartTime <= @Now AND Status = @OnHoldStatus AND RecurrenceType = @Once", connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                command2.Parameters.Add(new AriaDbParameter("RequestID", record.RequestID));
                command2.Parameters.Add(new AriaDbParameter("ParentRequestID", record.ParentRequestID));
                command2.Parameters.Add(new AriaDbParameter("Today", DateTime.Now.Date));
                command2.Parameters.Add(new AriaDbParameter("Now", new DateTime(0x76c, 1, 1, DateTime.Now.Hour, DateTime.Now.Minute, DateTime.Now.Second)));
                command2.Parameters.Add(new AriaDbParameter("OnHoldStatus", AriaRequestStatusTypes.OnHold.ToString()));
                command2.Parameters.Add(new AriaDbParameter("Once", AriaRequestRecurrenceTypes.Once.ToString()));
                if ((provider.GetObjectList(command2, typeof(AriaRequest)).Count > 0) && ((record.EventName == null) || (record.EventName.Trim().Length == 0)))
                {
                    record.Status = AriaRequestStatusTypes.Canceled;
                    provider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", record, new string[] { "Status", "NextChildRequestDateTime", "Occurence" }, "RequestId = @RequestId", clientId);
                }
                else if (this.GetRequestStartAfterDateTime(record) <= DateTime.Now)
                {
                    record.Status = AriaRequestStatusTypes.Running;
                    record.Occurence++;
                    provider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", record, new string[] { "Status", "NextChildRequestDateTime", "Occurence" }, "RequestId = @RequestId", clientId);
                    try
                    {
                        AriaThreadManager manager = new AriaThreadManager();
                        record.clientID = clientId;
                        manager.StartThread(record, clientId);
                    }
                    catch (Exception exception)
                    {
                        EventLog.WriteEntry("Get One Time Only Requests", exception.Message, EventLogEntryType.Information);
                    }
                }
            }
        }

        public void GenerateEventRequests(AriaDbConnection callerConnection, string eventObjectName, string eventName, AriaArgumentList eventArguments, string clientId)
        {
            this.GetRequestEventArgumentsSettings(eventObjectName, eventName, eventArguments, clientId);
            string commandText = "SELECT * FROM AriaRequest WHERE RecurrenceType = @RecurrenceType AND EventObjectName = @EventObjectName AND EventName = @EventName And Status <> @RemovedStatus ";
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RecurrenceType", AriaRequestRecurrenceTypes.Event.ToString()));
            command.Parameters.Add(new AriaDbParameter("EventObjectName", eventObjectName));
            command.Parameters.Add(new AriaDbParameter("EventName", eventName));
            command.Parameters.Add(new AriaDbParameter("RemovedStatus", AriaRequestStatusTypes.Removed.ToString()));
            AriaDataProvider provider = new AriaDataProvider();
            ArrayList objectList = provider.GetObjectList(command, typeof(AriaRequest));
            for (int i = 0; i < objectList.Count; i++)
            {
                AriaRequest record = null;
                record = (AriaRequest)objectList[i];
                if (callerConnection.CompanyName == record.Context.CompanyName)
                {
                    if (record.Status == AriaRequestStatusTypes.OnHold)
                    {
                        record.Status = AriaRequestStatusTypes.Started;
                    }
                    record.Occurence++;
                    record.Status = AriaRequestStatusTypes.Started;
                    provider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", record, new string[] { "Status", "Occurence" }, "RequestId = @RequestId", clientId);
                    AriaDbConnection connection2 = new AriaDbConnection("Aria", record.Context.CompanyName);
                    connection2.Context = record.Context;
                    connection2.CompanyName = record.Context.CompanyName;
                    connection2.Context.UserName = record.UserName;
                    connection2.Context.MethodName = record.MethodName;
                    eventArguments.BusinessObjectParameterName = "OrderPointer";
                    new AriaParameterSubstituter(connection2, eventArguments, clientId).DeepSubstitute(record.EventConditionList, clientId);
                    if (record.EventConditionList.AreConditionsValid())
                    {
                        record.EventArgumentList = eventArguments;
                        AriaRequest request2 = this.CloneOneTimeOnlyRequest(record, DateTime.Now.Date.AddHours((double)DateTime.Now.Hour).AddMinutes((double)DateTime.Now.Minute).AddSeconds((double)DateTime.Now.Second), clientId);
                    }
                }
            }
        }

        public void GenerateOnComputerStartupRequests(string clientId)
        {
            string commandText = "SELECT * FROM AriaRequest WHERE RecurrenceType = @RecurrenceType and Status <> @RemovedStatus ";
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RecurrenceType", AriaRequestRecurrenceTypes.ComputerStart.ToString()));
            command.Parameters.Add(new AriaDbParameter("RemovedStatus", AriaRequestRecurrenceTypes.ComputerStart.ToString()));
            AriaDataProvider provider = new AriaDataProvider();
            ArrayList objectList = provider.GetObjectList(command, typeof(AriaRequest));
            AriaRequest request = null;
            AriaRequestScheduler scheduler = new AriaRequestScheduler();
            for (int i = 0; i < objectList.Count; i++)
            {
                request = (AriaRequest)objectList[i];
                if (request.Status == AriaRequestStatusTypes.OnHold)
                {
                    request.Status = AriaRequestStatusTypes.Started;
                }
                this.CloneOneTimeOnlyRequest(request, clientId);
                request.Occurence++;
                provider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request, new string[] { "Status", "NextChildRequestDateTime", "Occurence" }, "RequestId = @RequestId", clientId);
            }
        }

        private string GenerateOptionGridXmlFileForObjectKey(object[] argumentsArray, AriaDataObjectPointer dataObjectPointer, AriaObjectRevision rootObjectRevision, out AriaOptionGridXmlDataSet optionGrid, out string OldFile, out string NewOutPutFile, out string OldOutPutFile, out bool ReplaceAgentValues)
        {
            optionGrid = null;
            OldFile = null;
            NewOutPutFile = null;
            OldOutPutFile = null;
            ReplaceAgentValues = false;
            foreach (object obj2 in argumentsArray)
            {
                if (!(obj2 is AriaOptionGridXmlDataSet))
                {
                    continue;
                }
                optionGrid = obj2 as AriaOptionGridXmlDataSet;
                XmlDocument document = new XmlDocument();
                document.Load(optionGrid.FileName);
                XmlNode node = document.SelectSingleNode("//*/row[Name='lcrpExp']")["Value"];
                if ((((node != null) && (node.InnerText != null)) && node.InnerText.Contains("{")) && node.InnerText.Contains("}"))
                {
                    ReplaceAgentValues = true;
                    string str = node.InnerText.Substring(node.InnerText.IndexOf("{") + 1, (node.InnerText.IndexOf("}") - node.InnerText.IndexOf("{")) - 1);
                    string str2 = "";
                    string str3 = "";
                    string fieldName = "";
                    string str5 = "";
                    int length = 0;
                    foreach (object obj3 in dataObjectPointer.KeyFields)
                    {
                        AriaDataObjectPointerKeyField field = obj3 as AriaDataObjectPointerKeyField;
                        if (field.FieldName.ToUpper().Trim() == str.ToUpper().Trim())
                        {
                            str3 = field.Value.ToString();
                            fieldName = field.FieldName;
                            if (field.Value is string)
                            {
                                str5 = "C";
                            }
                            else if ((((field.Value is int) || (field.Value is double)) || (field.Value is decimal)) || (field.Value is float))
                            {
                                str5 = "N";
                            }
                            else if (((field.Value is bool) || (field.Value.ToString() == ".T.")) || (field.Value.ToString() == ".F."))
                            {
                                str5 = "L";
                            }
                            length = str3.Length;
                            break;
                        }
                    }
                    str2 = str3;
                    node.InnerText = node.InnerText.Replace("{" + str + "}", "'" + str2 + "'");
                    string str6 = ((AriaDataObjectSettings)rootObjectRevision.ObjectRevisionSettings).TableName + "." + fieldName;
                    string str7 = document.SelectSingleNode("//*/row[starts-with(translate(Value,'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ'),'" + str6.ToUpper() + "')]")["Name"].InnerText.Replace(",1]", ",6]");
                    XmlNode node3 = document.SelectSingleNode("//*/row[Name='" + str7 + "']")["Value"];
                    if (node3 != null)
                    {
                        str2 = "";
                        while (str2.Length < 8)
                        {
                            str2 = str2 + Path.GetRandomFileName().Replace(".", "");
                        }
                        str2 = this.RandomString(8);
                        node3.InnerText = str2;
                        XmlNode newChild = document.CreateNode(XmlNodeType.Element, "row", "");
                        string str8 = "";
                        str8 = "<DataType>System.Table</DataType>";
                        object obj4 = (((str8 + "<Name>" + str2 + "</Name>") + "<Value></Value>" + "<CursorStrucutre>") + "<Field>" + "<Name>KEYEXP</Name>") + "<Type>" + str5 + "</Type>";
                        obj4 = (((string.Concat(new object[] { obj4, "<Width>", length, "</Width>" }) + "<Decimals>0</Decimals>") + "</Field>" + "<Field>") + "<Name>" + fieldName + "</Name>") + "<Type>" + str5 + "</Type>";
                        str8 = (string.Concat(new object[] { obj4, "<Width>", length, "</Width>" }) + "<Decimals>0</Decimals>") + "</Field>" + "</CursorStrucutre>";
                        newChild.InnerXml = str8;
                        newChild["Value"].InnerText = "<?xml version=\"1.0\"?> <xdoc> <" + str2 + "> <row> <keyexp>" + str3 + "</keyexp> <" + fieldName.ToLower() + ">" + str3 + "</" + fieldName.ToLower() + "> </row> </" + str2 + "> </xdoc>";
                        document.SelectSingleNode("//Parameters").AppendChild(newChild);
                    }
                    string filename = Path.GetRandomFileName().Replace(".", "") + ".xml";
                    OldOutPutFile = document.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText;
                    NewOutPutFile = Path.GetDirectoryName(OldOutPutFile) + @"\" + filename.Replace(".xml", Path.GetExtension(OldOutPutFile));
                    document.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText = NewOutPutFile;
                    filename = Path.GetDirectoryName(((AriaOptionGridXmlDataSet)obj2).FileName) + @"\" + filename;
                    document.Save(filename);
                    OldFile = optionGrid.FileName;
                    optionGrid.FileName = filename;
                    break;
                }
            }
            return "";
        }

        public void GenerateScheduleRequests(string clientId)
        {
            string commandText = "SELECT * FROM AriaRequest WHERE NextChildRequestDateTime <= @Now AND ( Status = @OnHoldStatus OR Status = @StartedStatus ) AND (RecurrenceType = @Daily OR RecurrenceType = @Weekly OR RecurrenceType = @Monthly)";
            AriaDbCommand command = new AriaDbCommand(commandText, new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("Now", DateTime.Now));
            command.Parameters.Add(new AriaDbParameter("OnHoldStatus", AriaRequestStatusTypes.OnHold.ToString()));
            command.Parameters.Add(new AriaDbParameter("StartedStatus", AriaRequestStatusTypes.Started.ToString()));
            command.Parameters.Add(new AriaDbParameter("Daily", AriaRequestRecurrenceTypes.Daily.ToString()));
            command.Parameters.Add(new AriaDbParameter("Weekly", AriaRequestRecurrenceTypes.Weekly.ToString()));
            command.Parameters.Add(new AriaDbParameter("Monthly", AriaRequestRecurrenceTypes.Monthly.ToString()));
            AriaDataProvider provider = new AriaDataProvider();
            ArrayList objectList = provider.GetObjectList(command, typeof(AriaRequest));
            for (int i = 0; i < objectList.Count; i++)
            {
                AriaRequest request = null;
                request = (AriaRequest)objectList[i];
                if (this.GetRequestStartAfterDateTime(request) <= DateTime.Now)
                {
                    if (request.Status == AriaRequestStatusTypes.OnHold)
                    {
                        request.Status = AriaRequestStatusTypes.Started;
                    }
                    AriaRequestScheduler scheduler = new AriaRequestScheduler();
                    AriaXmlSerializer serializer = new AriaXmlSerializer();
                    AriaRequest request2 = (AriaRequest)serializer.ConvertFromXml(serializer.ConvertToXml(request));
                    this.CloneOneTimeOnlyRequest(request, clientId);
                    do
                    {
                        scheduler.Schedule(request2);
                    }
                    while ((request2.NextChildRequestDateTime < DateTime.Now) && (request2.Status == AriaRequestStatusTypes.Started));
                    provider.UpdateObject(new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request2, new string[] { "Status", "NextChildRequestDateTime", "Occurence" }, "RequestId = @RequestId", clientId);
                }
            }
        }

        public ArrayList GetAgentRequests(AriaDbConnection connection, string objctName, AriaConditionList conditions, string clientId, string requestID)
        {
            string str2;
            string str3;
            int num;
            int num3;
            string str10;
            string str = "";
            str = str + "SELECT DISTINCT ";
            AriaEnviromentVariables variables = new AriaEnviromentVariables();
            variables.ClientID = clientId;
            variables.ConnectionsRefresh();
            AriaObjectDictionaryDBCentric centric = new AriaObjectDictionaryDBCentric();
            AriaObjectRevision revision = centric.LoadActiveRevision(new AriaDbConnection("", ""), objctName, clientId);
            AriaDataObjectSettings objectRevisionSettings = (AriaDataObjectSettings)revision.ObjectRevisionSettings;
            ArrayList list = centric.LoadAriaObjectProperties(connection, objctName, revision.ObjectRevision, false, clientId);
            ArrayList list2 = new ArrayList();
            if (objectRevisionSettings.DatabaseType == AriaDatabaseTypes.Aria40Data)
            {
                str2 = "[";
                str3 = "]";
            }
            else
            {
                str2 = "";
                str3 = "";
            }
            string newValue = "";
            List<string> list3 = new List<string>();
            List<string> list4 = new List<string>();
            string conditionsString = "";
            for (num = 0; num < conditions.Items.Count; num++)
            {
                conditionsString = conditionsString + this.GetCondition(null, num, objctName, (AriaCondition)conditions.Items[num], clientId);
            }
            bool flag = false;
            bool flag2 = false;
            for (num = 0; num < list.Count; num++)
            {
                AriaObjectProperty property = (AriaObjectProperty)list[num];
                if (((AriaFieldSettings)property.PropertySettings).IsPrimaryKey)
                {
                    if (flag)
                    {
                        str = str + ", ";
                    }
                    str10 = str;
                    str = str10 + "[" + objctName + "].[" + ((AriaFieldSettings)property.PropertySettings).FieldName.Trim() + "] AS [" + ((AriaFieldSettings)property.PropertySettings).FieldName.Trim() + "]";
                    flag = true;
                    list2.Add(((AriaFieldSettings)property.PropertySettings).FieldName.TrimEnd(new char[0]));
                }
                string str6 = ((AriaFieldSettings)property.PropertySettings).FieldName.Trim().ToUpper();
                bool flag3 = (((str6.IndexOf("(") > 0) || (str6.IndexOf("+") > 0)) || ((str6.IndexOf("-") > 0) || (str6.IndexOf("/") > 0))) || (str6.IndexOf("*") > 0);
                if (((AriaFieldSettings)property.PropertySettings).IsPrimaryKey || (conditionsString.ToUpper().Replace(" ", "").Contains(str6) && !flag3))
                {
                    if (flag2)
                    {
                        newValue = newValue + ", ";
                    }
                    newValue = newValue + str2 + ((AriaFieldSettings)property.PropertySettings).FieldName.Trim() + str3;
                    list3.Add(((AriaFieldSettings)property.PropertySettings).FieldName.ToUpper().Trim());
                    flag2 = true;
                }
                list4.Add(((AriaFieldSettings)property.PropertySettings).FieldName.Trim());
            }
            if (objectRevisionSettings.DatabaseType == AriaDatabaseTypes.Aria40Data)
            {
                str10 = str;
                str = str10 + " FROM OPENROWSET('MSDASQL','" + variables.GetAria04CompanyDataConnectionStringODBC(connection.CompanyName) + "','SELECT {RelaredFields}  FROM " + objectRevisionSettings.TableName + (((objectRevisionSettings.FixedFilter == null) || (objectRevisionSettings.FixedFilter.Trim().Length == 0)) ? "" : (" WHERE " + objectRevisionSettings.FixedFilter)) + " ') AS [" + objctName + "]";
            }
            else
            {
                string str7 = " OPENROWSET('MSDASQL','" + variables.GetConnectionString(connection.CompanyName, objectRevisionSettings.DatabaseType) + "','SELECT {RelaredFields}  FROM " + objectRevisionSettings.TableName + "') AS [" + objctName + "]";
                str = str + " FROM " + str7;
            }
            ArrayList list5 = centric.LoadAriaObjectChildObjectsOfType(new AriaDbConnection("", ""), objctName, AriaObjectTypes.RelatedData, clientId);
            for (num = 0; num < list5.Count; num++)
            {
                AriaObject obj2 = (AriaObject)list5[num];
                AriaRelatedDataObjectSettings settings2 = (AriaRelatedDataObjectSettings)centric.LoadActiveRevision(new AriaDbConnection("", ""), obj2.ObjectName, clientId).ObjectRevisionSettings;
                AriaDataObjectSettings settings3 = (AriaDataObjectSettings)centric.LoadActiveRevision(new AriaDbConnection("", ""), settings2.DataObjectName, clientId).ObjectRevisionSettings;
                string[] strArray = obj2.ObjectName.Split(new char[] { '.' });
                if (settings3.DatabaseType == AriaDatabaseTypes.Aria40Data)
                {
                    str10 = str;
                    str = (str10 + " LEFT JOIN OPENROWSET('MSDASQL','" + variables.GetAria04CompanyDataConnectionStringODBC(connection.CompanyName) + "','SELECT " + this.GetKeyAndFilterFields(connection, obj2.ObjectName, clientId, true, conditionsString) + " FROM " + settings3.TableName + "') AS [" + obj2.ObjectName.Trim() + "]") + " ON " + settings2.Filter.Split(new char[] { '|' })[1];
                }
                else
                {
                    string str8 = " OPENROWSET('MSDASQL','" + variables.GetConnectionString(connection.CompanyName, settings3.DatabaseType) + "','SELECT " + this.GetKeyAndFilterFields(connection, obj2.ObjectName, clientId, false, conditionsString) + " FROM " + settings3.TableName + "')";
                    str10 = str;
                    str = (str10 + " LEFT JOIN " + str8 + " AS [" + obj2.ObjectName.Trim() + "]") + " ON " + settings2.Filter.Split(new char[] { '|' })[1];
                }
                for (int j = 0; j < list4.Count; j++)
                {
                    if (settings2.Filter.Split(new char[] { '|' })[1].ToUpper().Replace(" ", "").Contains(list4[j].ToUpper().Trim()) && !list3.Contains(list4[j].ToUpper().Trim()))
                    {
                        str10 = newValue;
                        newValue = str10 + "," + str2 + list4[j] + str3;
                        list3.Add(list4[j].ToUpper().Trim());
                    }
                }
            }
            str = str.Replace("{RelaredFields}", newValue);
            AriaDbCommand command = new AriaDbCommand("", connection, AriaDatabaseTypes.AriaOpenRowSet, clientId);
            num = 0;
            while (num < conditions.Items.Count)
            {
                if (num == 0)
                {
                    str = str + " WHERE ";
                }
                str = str + this.GetCondition(command, num, objctName, (AriaCondition)conditions.Items[num], clientId);
                if (num < (conditions.Items.Count - 1))
                {
                    str = str + " AND ";
                }
                num++;
            }
            for (num3 = 0; num3 < command.Parameters.Count; num3++)
            {
                if ((command.Parameters[num3].ParameterValue.ToString().IndexOf("{") == 0) && (command.Parameters[num3].ParameterValue.ToString().IndexOf("}") == (command.Parameters[num3].ParameterValue.ToString().Trim().Length - 1)))
                {
                    string str9 = command.Parameters[num3].ParameterValue.ToString().Replace("{", "").Replace("}", "");
                    str = str.Replace("@" + command.Parameters[num3].ParameterName, str9);
                }
            }
            command.CommandText = str;
            List<AriaLogElement> list6 = new List<AriaLogElement>();
            list6.Add(new AriaLogElement("SelectCommand", str));
            for (num3 = 0; num3 < command.Parameters.Count; num3++)
            {
                list6.Add(new AriaLogElement(command.Parameters[num3].ParameterName, command.Parameters[num3].ParameterValue.ToString()));
            }
            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(requestID), "Get Request Matached Records", list6.ToArray());
            DataTable dataTable = command.GetDataTable();
            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(requestID), "Number of Request Matached Records", new AriaLogElement[] { new AriaLogElement("RecordsCount", dataTable.Rows.Count.ToString()) });
            ArrayList list7 = new ArrayList();
            for (int i = 0; i < dataTable.Rows.Count; i++)
            {
                AriaDataObjectPointer pointer = new AriaDataObjectPointer();
                for (num = 0; num < list2.Count; num++)
                {
                    pointer.KeyFields.Add(new AriaDataObjectPointerKeyField((string)list2[num], dataTable.Rows[i][(string)list2[num]]));
                }
                list7.Add(pointer);
            }
            return list7;
        }

        private ArrayList GetAriaObjects(AriaObject ariaObject, string clientId)
        {
            ArrayList list = new AriaObjectDictionaryDBCentric().LoadAriaObjectChildObjects(new AriaDbConnection("", ""), ariaObject.ObjectName, clientId);
            for (int i = 0; i < list.Count; i++)
            {
                list.AddRange(this.GetAriaObjects((AriaObject)list[i], clientId));
            }
            return list;
        }

        private string GetCondition(AriaDbCommand command, int conditionNo, string objectName, AriaCondition condition, string clientId)
        {
            string str = "(" + (condition.IsOperator ? "" : " NOT ") + "(";
            string str2 = objectName.Trim();
            AriaObjectDictionaryDBCentric centric = new AriaObjectDictionaryDBCentric();
            AriaDataObjectSettings objectRevisionSettings = (AriaDataObjectSettings)centric.LoadActiveRevision(new AriaDbConnection("", ""), objectName, clientId).ObjectRevisionSettings;
            str = str + this.GetDataPathFieldName(objectName + "." + condition.LeftHandSide.PropertyDataPathDictionary["Value"].ToString(), clientId);
            switch (condition.Operator)
            {
                case AriaConditionOperators.Like:
                    str = str + " = ";
                    break;

                case AriaConditionOperators.GreaterThan:
                    str = str + " > ";
                    break;

                case AriaConditionOperators.LessThan:
                    str = str + " < ";
                    break;

                case AriaConditionOperators.GreaterOrEqual:
                    str = str + " >= ";
                    break;

                case AriaConditionOperators.LessOrEqual:
                    str = str + " <= ";
                    break;

                case AriaConditionOperators.Between:
                    str = str + " BETWEEN ";
                    break;

                case AriaConditionOperators.Contains:
                    str = str + " LIKE ";
                    break;
            }
            if (condition.Operator == AriaConditionOperators.Between)
            {
                AriaRange rightHandSide = (AriaRange)condition.RightHandSide;
                AriaStandardDataType from = (AriaStandardDataType)rightHandSide.From;
                AriaStandardDataType to = (AriaStandardDataType)rightHandSide.To;
                if (to.PropertyDataPathDictionary.Count == 0)
                {
                    if (command != null)
                    {
                        command.Parameters.Add(new AriaDbParameter("Param" + conditionNo.ToString().TrimEnd(new char[0]) + "_1", from.Value));
                    }
                    if (command != null)
                    {
                        command.Parameters.Add(new AriaDbParameter("Param" + conditionNo.ToString().TrimEnd(new char[0]) + "_2", to.Value));
                    }
                    str = (str + "@Param" + conditionNo.ToString().TrimEnd(new char[0]) + "_1 AND ") + "@Param" + conditionNo.ToString().TrimEnd(new char[0]) + "_2";
                }
                else
                {
                    str = (str + this.GetDataPathFieldName(objectName + "." + from.PropertyDataPathDictionary["Value"].ToString(), clientId) + " AND ") + this.GetDataPathFieldName(objectName + "." + to.PropertyDataPathDictionary["Value"].ToString(), clientId);
                }
            }
            else if (condition.RightHandSide.PropertyDataPathDictionary.Count == 0)
            {
                if (command != null)
                {
                    command.Parameters.Add(new AriaDbParameter("Param" + conditionNo.ToString().TrimEnd(new char[0]), ((AriaStandardDataType)condition.RightHandSide).Value));
                }
                if (condition.Operator == AriaConditionOperators.Contains)
                {
                    str = str + "'%' + @Param" + conditionNo.ToString().TrimEnd(new char[0]) + " + '%'";
                }
                else
                {
                    str = str + "@Param" + conditionNo.ToString().TrimEnd(new char[0]);
                }
            }
            else if (condition.Operator == AriaConditionOperators.Contains)
            {
                str = str + "'%' + " + this.GetDataPathFieldName(objectName + "." + condition.RightHandSide.PropertyDataPathDictionary["Value"].ToString(), clientId) + " + '%'";
            }
            else
            {
                str = str + this.GetDataPathFieldName(objectName + "." + condition.RightHandSide.PropertyDataPathDictionary["Value"].ToString(), clientId);
            }
            return (str + "))");
        }

        private string GetDataPathFieldName(string DataPath, string clientId)
        {
            string objectName = DataPath.Substring(0, DataPath.LastIndexOf('.'));
            string propertyName = DataPath.Substring(DataPath.LastIndexOf('.') + 1);
            AriaObjectDictionaryDBCentric centric = new AriaObjectDictionaryDBCentric();
            AriaObjectRevision revision = centric.LoadActiveRevision(new AriaDbConnection("", ""), objectName, clientId);
            AriaObjectProperty property = centric.LoadAriaObjectProperty(new AriaDbConnection("", ""), objectName, revision.ObjectRevision, propertyName, clientId);
            string fieldName = "";
            if (property.PropertyType == AriaDataTypes.AriaField)
            {
                fieldName = ((AriaFieldSettings)property.PropertySettings).FieldName;
            }
            else
            {
                fieldName = ((AriaRelatedFieldSettings)property.PropertySettings).FieldName;
            }
            if ((((fieldName.IndexOf("(") > 0) || (fieldName.IndexOf("+") > 0)) || ((fieldName.IndexOf("-") > 0) || (fieldName.IndexOf("/") > 0))) || (fieldName.IndexOf("*") > 0))
            {
                if (fieldName.Contains("|"))
                {
                    fieldName = fieldName.Split(new char[] { '|' })[1];
                }
                return fieldName;
            }
            return ("[" + objectName + "].[" + fieldName + "]");
        }

        public string GetKeyAndFilterFields(AriaDbConnection connection, string objctName, string clientId, bool withPracets, string conditionsString)
        {
            string str = "";
            AriaEnviromentVariables variables = new AriaEnviromentVariables();
            variables.ClientID = clientId;
            variables.ConnectionsRefresh();
            AriaObjectDictionaryDBCentric centric = new AriaObjectDictionaryDBCentric();
            AriaObjectRevision revision = centric.LoadActiveRevision(new AriaDbConnection("", ""), objctName, clientId);
            ArrayList list = centric.LoadAriaObjectProperties(connection, objctName, revision.ObjectRevision, false, clientId);
            ArrayList list2 = new ArrayList();
            string str2 = withPracets ? "[" : "";
            string str3 = withPracets ? "]" : "";
            bool flag = false;
            for (int i = 0; i < list.Count; i++)
            {
                AriaObjectProperty property = (AriaObjectProperty)list[i];
                if ((property.PropertySettings.GetType().ToString() == typeof(AriaFieldSettings).ToString()) && (((AriaFieldSettings)property.PropertySettings).IsPrimaryKey || conditionsString.ToUpper().Replace(" ", "").Contains(((AriaFieldSettings)property.PropertySettings).FieldName.Trim().ToUpper())))
                {
                    if (flag)
                    {
                        str = str + ", ";
                    }
                    str = str + str2 + ((AriaFieldSettings)property.PropertySettings).FieldName.Trim() + str3;
                    flag = true;
                }
                if (((property.PropertySettings.GetType().ToString() == typeof(AriaRelatedFieldSettings).ToString()) && ((AriaRelatedFieldSettings)property.PropertySettings).IsPrimaryKey) || conditionsString.ToUpper().Replace(" ", "").Contains(((AriaRelatedFieldSettings)property.PropertySettings).FieldName.Trim().ToUpper()))
                {
                    if (flag)
                    {
                        str = str + ", ";
                    }
                    str = str + str2 + ((AriaRelatedFieldSettings)property.PropertySettings).FieldName.Trim() + str3;
                    flag = true;
                }
            }
            return str;
        }

        public string[] GetPrinters()
        {
            List<string> list = new List<string>();
            for (int i = 0; i < PrinterSettings.InstalledPrinters.Count; i++)
            {
                list.Add(PrinterSettings.InstalledPrinters[i]);
            }
            return list.ToArray();
        }

        private ArrayList GetRepeatableNextChildDateTimes(AriaRequest request)
        {
            DateTime time2;
            ArrayList list = new ArrayList();
            if (request.RepeatTaskUntilType == AriaRequestRepeatUntilTypes.Duration)
            {
                time2 = request.RequestStartTime.AddMinutes((double)request.RepeatTaskUntilDuration);
            }
            else
            {
                time2 = new DateTime(request.RequestStartTime.Year, request.RequestStartTime.Month, request.RequestStartTime.Day, request.RepeatTaskUntillTime.Hour, request.RepeatTaskUntillTime.Minute, request.RepeatTaskUntillTime.Second);
            }
            for (int i = request.RepeatTaskEvery; request.RequestStartTime.AddMinutes((double)i) <= time2; i += request.RepeatTaskEvery)
            {
                list.Add(request.RequestStartTime.AddMinutes((double)i));
            }
            return list;
        }

        public AriaRequest GetRequest(string requestId, string clientId)
        {
            string commandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RequestID", requestId));
            AriaDataProvider provider = new AriaDataProvider();
            return (AriaRequest)provider.GetObjectList(command, typeof(AriaRequest))[0];
        }

        public string GetRequestCompany(string requestId, string clientId)
        {
            string commandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RequestID", requestId));
            AriaDataProvider provider = new AriaDataProvider();
            AriaRequest request = (AriaRequest)provider.GetObjectList(command, typeof(AriaRequest))[0];
            return request.Context.CompanyName;
        }

        public void GetRequestEventArgumentsSettings(string objectName, string eventName, AriaArgumentList arguments, string clientId)
        {
            AriaObjectDictionaryDBCentric centric = new AriaObjectDictionaryDBCentric();
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            string objectRevision = centric.LoadActiveRevision(connection, objectName, clientId).ObjectRevision;
            for (int i = 0; i < arguments.Count; i++)
            {
                AriaArgument argument = (AriaArgument)arguments[i];
                argument.Settings = ((AriaObjectEventParameter)centric.LoadAriaObjectEventParameters(connection, objectName, objectRevision, eventName, clientId)[0]).ParameterSettings;
            }
            arguments.BusinessObjectParameterName = centric.LoadAriaObjectEvent(connection, objectName, objectRevision, eventName, clientId).BusinessObjectParameterName;
        }

        public void GetRequestMethodArgumentsSettings(string objectName, string methodName, AriaArgumentList arguments, string clientId)
        {
            AriaObjectDictionaryDBCentric centric = new AriaObjectDictionaryDBCentric();
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            string objectRevision = centric.LoadActiveRevision(connection, objectName, clientId).ObjectRevision;
            for (int i = 0; i < arguments.Count; i++)
            {
                AriaArgument argument = (AriaArgument)arguments[i];
                argument.Settings = ((AriaObjectMethodParameter)centric.LoadAriaObjectMethodParameters(connection, objectName, objectRevision, methodName, clientId)[0]).ParameterSettings;
            }
            arguments.BusinessObjectParameterName = centric.LoadAriaObjectMethod(connection, objectName, objectRevision, methodName, clientId).BusinessObjectParameterName;
        }

        private DateTime GetRequestStartAfterDateTime(AriaRequest request)
        {
            return request.StartAfterDate.AddHours((double)request.RequestStartTime.Hour).AddMinutes((double)request.RequestStartTime.Minute).AddSeconds((double)request.RequestStartTime.Second);
        }

        public override object InitializeLifetimeService()
        {
            return null;
        }

        public void InvokeRequestMethod(object requestObject, string clientId)
        {
            string path = null;
            Exception exception;
            bool flag = false;
            AriaRequest request = (AriaRequest)requestObject;
            if (request.MethodArgumentList != null)
            {
                foreach (object obj2 in request.MethodArgumentList)
                {
                    AriaArgument argument = (AriaArgument)obj2;
                    if (argument.Value is AriaOptionGridXmlDataSet)
                    {
                        AriaOptionGridXmlDataSet set = argument.Value as AriaOptionGridXmlDataSet;
                        XmlDocument document = new XmlDocument();
                        document.Load(set.FileName);
                        path = document.SelectSingleNode("//row[Name='gcOutFile']/Value").InnerText;
                        if (path == null)
                        {
                            path = "";
                        }
                        if (((path != null) && (path != "")) && (request.CompleteNotification != null))
                        {
                            foreach (DictionaryEntry entry in request.CompleteNotification.Attachment)
                            {
                                if ((entry.Value != null) && (path.ToString().Trim().ToUpper() == entry.Value.ToString().Trim().ToUpper()))
                                {
                                    flag = true;
                                }
                            }
                        }
                        break;
                    }
                }
            }
            if (!flag)
            {
                path = null;
            }
            AriaRequest request2 = (AriaRequest)requestObject;
            request2.Error = null;
            AriaDbConnection connection = new AriaDbConnection();
            connection.Context = request2.Context;
            connection.CompanyName = request2.Context.CompanyName;
            connection.Context.UserName = request2.UserName;
            connection.Context.MethodName = request2.MethodName;
            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Execute", new AriaLogElement[0]);
            AriaDataProvider provider = new AriaDataProvider();
            AriaMessagingManager messaging = new AriaMessagingManager();
            try
            {
                AriaObjectDictionaryDBCentric centric;
                AriaObjectRevisionSettings objectRevisionSettings;
                object[] objArray;
                AriaReflector reflector;
                DateTime now;
                string str2;
                AriaDbCommand command;
                AriaDataProvider provider2;
                AriaRequest request3;
                AriaObjectDictionaryDBCentric centric2;
                AriaOptionGridXmlDataSet set2;
                string str3;
                string str4;
                string str5;
                bool flag3;
                AriaDataObjectPointerSettings settings2;
                bool flag2 = false;
                if ((request2.RecurrenceType == AriaRequestRecurrenceTypes.Immediate) && !flag2)
                {
                    flag2 = true;
                    request2.Status = AriaRequestStatusTypes.Running;
                    centric = new AriaObjectDictionaryDBCentric();
                    objectRevisionSettings = centric.LoadActiveRevision(connection, request2.MethodObjectName, clientId).ObjectRevisionSettings;
                    objArray = new object[request2.MethodArgumentList.Count + 3];
                    objArray[0] = request2.RequestID.ToString();
                    objArray[1] = ((AriaArgument)request2.MethodArgumentList[0]).Value;
                    try
                    {
                        objArray[2] = clientId.ToString();
                    }
                    catch (Exception)
                    {
                        objArray[2] = "";
                    }
                    this.GetRequestMethodArgumentsSettings(request2.MethodObjectName, request2.MethodName, request2.MethodArgumentList, clientId);
                    connection.Context = request2.Context;
                    connection.CompanyName = request2.Context.CompanyName;
                    connection.Context.UserName = request2.UserName;
                    connection.Context.MethodName = request2.MethodName;
                    reflector = new AriaReflector();
                    objArray[1] = ((AriaOptionGridXmlDataSet)objArray[1]).FileName;
                    if (objectRevisionSettings is AriaServerObjectSettings)
                    {
                        objArray[3] = ((AriaServerObjectSettings)objectRevisionSettings).ClassName.Split(new char[] { '.' })[0];
                        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), string.Concat(new object[] { "ExecuteMethod Parameters <RequestHandler.RequestHandler>, <", request2.MethodName.ToString(), ">, <", objArray[3], ">" }), new AriaLogElement[0]);
                        now = DateTime.Now;
                        reflector.ExecuteMethod("RequestHandler.RequestHandler", request2.MethodName, objArray);
                        this.ValidateExectionTime(now, request2, messaging, connection, clientId);
                    }
                    else if (objectRevisionSettings is AriaReportObjectSettings)
                    {
                        objArray[3] = ((AriaReportObjectSettings)objectRevisionSettings).ClassName.Split(new char[] { '.' })[0];
                        now = DateTime.Now;
                        reflector.ExecuteMethod("RequestHandler.RequestHandler", request2.MethodName, objArray);
                        this.ValidateExectionTime(now, request2, messaging, connection, clientId);
                    }
                    str2 = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";
                    command = new AriaDbCommand(str2, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                    command.Parameters.Add(new AriaDbParameter("RequestID", request2.RequestID));
                    provider2 = new AriaDataProvider();
                    request3 = (AriaRequest)provider2.GetObjectList(command, typeof(AriaRequest))[0];
                    if (request3.Status == AriaRequestStatusTypes.Canceled)
                    {
                        return;
                    }
                    if (request3.Status == AriaRequestStatusTypes.Failed)
                    {
                        if ((request3.ErrorNotification != null) && request3.ErrorNotification.IsValid())
                        {
                            messaging.SendEmail(connection, request3.MethodArgumentList, request3.ErrorNotification, clientId);
                            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Send Failed Email", messaging.GetLog());
                        }
                        return;
                    }
                    request2.Status = AriaRequestStatusTypes.Completed;
                    if (((request2.CompleteNotification != null) && request2.CompleteNotification.IsValid()) && ((path == null) || File.Exists(path)))
                    {
                        messaging.SendEmail(connection, request2.MethodArgumentList, this.GetRequest(request2.RequestID, clientId).CompleteNotification, clientId);
                        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Send Email", messaging.GetLog());
                    }
                    request2.Occurence++;
                    provider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request2, new string[] { "Status", "Occurence" }, "RequestId = @RequestId", clientId);
                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Complete Request", new AriaLogElement[0]);
                }
                if ((request2.EventName.Trim().Length > 0) && !flag2)
                {
                    flag2 = true;
                    if (!string.IsNullOrEmpty(request2.MethodObjectName))
                    {
                        centric = new AriaObjectDictionaryDBCentric();
                        objectRevisionSettings = centric.LoadActiveRevision(connection, request2.MethodObjectName, clientId).ObjectRevisionSettings;
                        centric2 = new AriaObjectDictionaryDBCentric();
                        AriaObjectRevision rootObjectRevision = centric2.LoadActiveRevision(connection, request2.EventObjectName, clientId);
                        objArray = new object[request2.MethodArgumentList.Count + 3];
                        objArray[0] = request2.RequestID.ToString();
                        objArray[1] = ((AriaArgument)request2.MethodArgumentList[0]).Value;
                        try
                        {
                            objArray[2] = clientId.ToString();
                        }
                        catch (Exception)
                        {
                            objArray[2] = "";
                        }
                        AriaDataObjectPointer dataObjectPointer = new AriaDataObjectPointer();
                        dataObjectPointer = (AriaDataObjectPointer)((AriaArgument)request2.EventArgumentList[0]).Value;
                        this.GenerateOptionGridXmlFileForObjectKey(objArray, dataObjectPointer, rootObjectRevision, out set2, out str3, out str4, out str5, out flag3);
                        this.GetRequestMethodArgumentsSettings(request2.MethodObjectName, request2.MethodName, request2.MethodArgumentList, clientId);
                        connection.Context = request2.Context;
                        connection.CompanyName = request2.Context.CompanyName;
                        connection.Context.UserName = request2.UserName;
                        connection.Context.MethodName = request2.MethodName;
                        reflector = new AriaReflector();
                        objArray[1] = ((AriaOptionGridXmlDataSet)objArray[1]).FileName;
                        if (objectRevisionSettings is AriaServerObjectSettings)
                        {
                            objArray[3] = ((AriaServerObjectSettings)objectRevisionSettings).ClassName.Split(new char[] { '.' })[0];
                            now = DateTime.Now;
                            reflector.ExecuteMethod("RequestHandler.RequestHandler", request2.MethodName, objArray);
                            this.ValidateExectionTime(now, request2, messaging, connection, clientId);
                        }
                        else if (objectRevisionSettings is AriaReportObjectSettings)
                        {
                            objArray[3] = ((AriaReportObjectSettings)objectRevisionSettings).ClassName.Split(new char[] { '.' })[0];
                            now = DateTime.Now;
                            reflector.ExecuteMethod("RequestHandler.RequestHandler", request2.MethodName, objArray);
                            this.ValidateExectionTime(now, request2, messaging, connection, clientId);
                        }
                        str2 = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";
                        command = new AriaDbCommand(str2, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                        command.Parameters.Add(new AriaDbParameter("RequestID", request2.RequestID));
                        provider2 = new AriaDataProvider();
                        request3 = (AriaRequest)provider2.GetObjectList(command, typeof(AriaRequest))[0];
                        if (request3.Status == AriaRequestStatusTypes.Canceled)
                        {
                            return;
                        }
                        if (request3.Status == AriaRequestStatusTypes.Failed)
                        {
                            if ((request3.ErrorNotification != null) && request3.ErrorNotification.IsValid())
                            {
                                messaging.SendEmail(connection, request3.MethodArgumentList, request3.ErrorNotification, clientId);
                                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Send Failed Email", messaging.GetLog());
                            }
                            return;
                        }
                        settings2 = new AriaDataObjectPointerSettings();
                        settings2.DataObjectName = request2.EventObjectName;
                        settings2.DataObjectRevision = centric2.LoadActiveRevision(connection, request2.EventObjectName, clientId).ObjectRevision;
                        this.SendAgentMail(request2, clientId, dataObjectPointer, settings2, connection, messaging, path, set2, str3, str4, str5, flag3);
                    }
                    request2.Status = AriaRequestStatusTypes.Completed;
                    this.GetRequestEventArgumentsSettings(request2.EventObjectName, request2.EventName, request2.EventArgumentList, clientId);
                    if (((request2.CompleteNotification != null) && request2.CompleteNotification.IsValid()) && ((path == null) || File.Exists(path)))
                    {
                        messaging.SendEmail(connection, request2.EventArgumentList, this.GetRequest(request2.RequestID, clientId).CompleteNotification, clientId);
                        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Send Email", messaging.GetLog());
                    }
                    request2.Occurence++;
                    provider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request2, new string[] { "Status", "Occurence" }, "RequestId = @RequestId", clientId);
                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Complete Request", new AriaLogElement[0]);
                }
                if (((request2.EventName.Trim().Length == 0) && (request2.EventObjectName.Length != 0)) && !flag2)
                {
                    flag2 = true;
                    request2.Status = AriaRequestStatusTypes.Completed;
                    ArrayList list = this.GetAgentRequests(new AriaDbConnection(request2.Context.CustomerName, request2.Context.CompanyName), request2.EventObjectName, request2.EventConditionList, clientId, request2.RequestID);
                    AriaEnviromentVariables variables = new AriaEnviromentVariables();
                    if ((variables.AriaMaxRecordsPerAgent != 0) && (list.Count > variables.AriaMaxRecordsPerAgent))
                    {
                        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Max Number of Records per Agent exceeded", new AriaLogElement[] { new AriaLogElement("Request Number", request2.RequestID), new AriaLogElement("Agent Records Count", list.Count.ToString()) });
                        if (!(string.IsNullOrEmpty(variables.ExceedLimitNotificationEmail) && string.IsNullOrEmpty(variables.ClientExceedLimitNotificationEmail)))
                        {
                            AriaEmail mailMessage = new AriaEmail();
                            mailMessage.To = string.IsNullOrEmpty(variables.ExceedLimitNotificationEmail) ? "" : variables.ExceedLimitNotificationEmail;
                            mailMessage.To = (string.IsNullOrEmpty(mailMessage.To) || string.IsNullOrEmpty(variables.ClientExceedLimitNotificationEmail)) ? "" : (mailMessage.To + ";" + (string.IsNullOrEmpty(variables.ClientExceedLimitNotificationEmail) ? "" : variables.ClientExceedLimitNotificationEmail));
                            mailMessage.Subject = "Max Number of Records per Agent exceeded <<" + request2.RequestNumber + ">>";
                            mailMessage.Body = string.Concat(new object[] { "Client = ", clientId, "\nRequest Number = ", request2.RequestNumber, "\nAgent Records Count = ", list.Count.ToString() });
                            messaging.SendEmail(connection, request2.EventArgumentList, mailMessage, clientId);
                        }
                        throw new Exception("Agent request has exceeded the maximum number of allowed records '" + variables.AriaMaxRecordsPerAgent.ToString() + "'.");
                    }
                    centric2 = new AriaObjectDictionaryDBCentric();
                    settings2 = new AriaDataObjectPointerSettings();
                    settings2.DataObjectName = request2.EventObjectName;
                    settings2.DataObjectRevision = centric2.LoadActiveRevision(connection, request2.EventObjectName, clientId).ObjectRevision;
                    for (int i = 0; i < list.Count; i++)
                    {
                        set2 = null;
                        str3 = null;
                        str4 = null;
                        str5 = null;
                        flag3 = false;
                        if ((request2.MethodObjectName == null) || (request2.MethodObjectName.Trim() == ""))
                        {
                            goto Label_1152;
                        }
                        centric = new AriaObjectDictionaryDBCentric();
                        objectRevisionSettings = centric.LoadActiveRevision(connection, request2.MethodObjectName, clientId).ObjectRevisionSettings;
                        AriaObjectRevision revision3 = centric2.LoadActiveRevision(connection, request2.EventObjectName, clientId);
                        objArray = new object[request2.MethodArgumentList.Count + 3];
                        objArray[0] = request2.RequestID.ToString();
                        objArray[1] = ((AriaArgument)request2.MethodArgumentList[0]).Value;
                        try
                        {
                            objArray[2] = clientId.ToString();
                        }
                        catch (Exception)
                        {
                            objArray[2] = "";
                        }
                        this.GenerateOptionGridXmlFileForObjectKey(objArray, list[i] as AriaDataObjectPointer, revision3, out set2, out str3, out str4, out str5, out flag3);
                        this.GetRequestMethodArgumentsSettings(request2.MethodObjectName, request2.MethodName, request2.MethodArgumentList, clientId);
                        connection.Context = request2.Context;
                        connection.CompanyName = request2.Context.CompanyName;
                        connection.Context.UserName = request2.UserName;
                        connection.Context.MethodName = request2.MethodName;
                        reflector = new AriaReflector();
                        objArray[1] = ((AriaOptionGridXmlDataSet)objArray[1]).FileName;
                        int num2 = 0;
                    Label_0F32: ;
                        try
                        {
                            if (objectRevisionSettings is AriaServerObjectSettings)
                            {
                                objArray[3] = ((AriaServerObjectSettings)objectRevisionSettings).ClassName.Split(new char[] { '.' })[0];
                                now = DateTime.Now;
                                reflector.ExecuteMethod("RequestHandler.RequestHandler", request2.MethodName, objArray);
                                this.ValidateExectionTime(now, request2, messaging, connection, clientId);
                            }
                            else if (objectRevisionSettings is AriaReportObjectSettings)
                            {
                                objArray[3] = ((AriaReportObjectSettings)objectRevisionSettings).ClassName.Split(new char[] { '.' })[0];
                                now = DateTime.Now;
                                reflector.ExecuteMethod("RequestHandler.RequestHandler", request2.MethodName, objArray);
                                this.ValidateExectionTime(now, request2, messaging, connection, clientId);
                            }
                        }
                        catch (Exception exception4)
                        {
                            exception = exception4;
                            num2++;
                            AriaLogElement element = new AriaLogElement("Trail Number", num2.ToString());
                            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Retry Reques", new AriaLogElement[] { element });
                            if (num2 >= 10)
                            {
                                throw exception;
                            }
                            goto Label_0F32;
                        }
                        str2 = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";
                        command = new AriaDbCommand(str2, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                        command.Parameters.Add(new AriaDbParameter("RequestID", request2.RequestID));
                        provider2 = new AriaDataProvider();
                        request3 = (AriaRequest)provider2.GetObjectList(command, typeof(AriaRequest))[0];
                        if (request3.Status == AriaRequestStatusTypes.Canceled)
                        {
                            return;
                        }
                        if (request3.Status == AriaRequestStatusTypes.Failed)
                        {
                            if ((request3.ErrorNotification != null) && request3.ErrorNotification.IsValid())
                            {
                                messaging.SendEmail(connection, request3.MethodArgumentList, request3.ErrorNotification, clientId);
                                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Send Failed Email", messaging.GetLog());
                            }
                            return;
                        }
                    Label_1152:
                        this.SendAgentMail(request2, clientId, list[i] as AriaDataObjectPointer, settings2, connection, messaging, path, set2, str3, str4, str5, flag3);
                    }
                    request2.Occurence++;
                    provider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request2, new string[] { "Status", "Occurence" }, "RequestId = @RequestId", clientId);
                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Update Request Status", new AriaLogElement[] { new AriaLogElement("Status", request2.Status.ToString()) });
                }
                if (((request2.EventName.Trim().Length == 0) && ((request2.EventConditionList == null) || (request2.EventConditionList.Items.Count < 0))) && !flag2)
                {
                    flag2 = true;
                    centric = new AriaObjectDictionaryDBCentric();
                    objectRevisionSettings = centric.LoadActiveRevision(connection, request2.MethodObjectName, clientId).ObjectRevisionSettings;
                    objArray = new object[request2.MethodArgumentList.Count + 3];
                    objArray[0] = request2.RequestID.ToString();
                    objArray[1] = ((AriaArgument)request2.MethodArgumentList[0]).Value;
                    try
                    {
                        objArray[2] = clientId.ToString();
                    }
                    catch (Exception)
                    {
                        objArray[2] = "";
                    }
                    this.GetRequestMethodArgumentsSettings(request2.MethodObjectName, request2.MethodName, request2.MethodArgumentList, clientId);
                    connection.Context = request2.Context;
                    connection.CompanyName = request2.Context.CompanyName;
                    connection.Context.UserName = request2.UserName;
                    connection.Context.MethodName = request2.MethodName;
                    reflector = new AriaReflector();
                    objArray[1] = ((AriaOptionGridXmlDataSet)objArray[1]).FileName;
                    if (objectRevisionSettings is AriaServerObjectSettings)
                    {
                        objArray[3] = ((AriaServerObjectSettings)objectRevisionSettings).ClassName.Split(new char[] { '.' })[0];
                        now = DateTime.Now;
                        reflector.ExecuteMethod("RequestHandler.RequestHandler", request2.MethodName, objArray);
                        this.ValidateExectionTime(now, request2, messaging, connection, clientId);
                    }
                    else if (objectRevisionSettings is AriaReportObjectSettings)
                    {
                        objArray[3] = ((AriaReportObjectSettings)objectRevisionSettings).ClassName.Split(new char[] { '.' })[0];
                        now = DateTime.Now;
                        reflector.ExecuteMethod("RequestHandler.RequestHandler", request2.MethodName, objArray);
                        this.ValidateExectionTime(now, request2, messaging, connection, clientId);
                    }
                    str2 = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";
                    command = new AriaDbCommand(str2, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                    command.Parameters.Add(new AriaDbParameter("RequestID", request2.RequestID));
                    provider2 = new AriaDataProvider();
                    request3 = (AriaRequest)provider2.GetObjectList(command, typeof(AriaRequest))[0];
                    if (request3.Status == AriaRequestStatusTypes.Canceled)
                    {
                        return;
                    }
                    if (request3.Status == AriaRequestStatusTypes.Failed)
                    {
                        if ((request3.ErrorNotification != null) && request3.ErrorNotification.IsValid())
                        {
                            messaging.SendEmail(connection, request3.MethodArgumentList, request3.ErrorNotification, clientId);
                            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Send Failed Email", messaging.GetLog());
                        }
                        return;
                    }
                    request2.Status = AriaRequestStatusTypes.Completed;
                    if (((request2.CompleteNotification != null) && request2.CompleteNotification.IsValid()) && ((path == null) || File.Exists(path)))
                    {
                        messaging.SendEmail(connection, request2.MethodArgumentList, this.GetRequest(request2.RequestID, clientId).CompleteNotification, clientId);
                        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Send Email", messaging.GetLog());
                    }
                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Complete Request", new AriaLogElement[0]);
                }
            }
            catch (Exception exception6)
            {
                exception = exception6;
                request2.Error = exception;
                request2.Status = AriaRequestStatusTypes.Failed;
                if ((request2.ErrorNotification != null) && request2.ErrorNotification.IsValid())
                {
                    if ((request2.EventObjectName != null) && (request2.EventObjectName.Trim().Length != 0))
                    {
                        messaging.SendEmail(connection, request2.EventArgumentList, this.GetRequest(request2.RequestID, clientId).ErrorNotification, clientId);
                        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Send Failed Email", messaging.GetLog());
                    }
                    else
                    {
                        messaging.SendEmail(connection, request2.MethodArgumentList, this.GetRequest(request2.RequestID, clientId).ErrorNotification, clientId);
                        AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Send Failed Email", messaging.GetLog());
                    }
                }
            }
            if (request2.Error == null)
            {
                provider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request2, new string[] { "Status", "NextChildRequestDateTime", "Occurence" }, "RequestId = @RequestId", clientId);
                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Update Request Status", new AriaLogElement[] { new AriaLogElement("Status", request2.Status.ToString()) });
            }
            else
            {
                provider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", request2, new string[] { "Status", "NextChildRequestDateTime", "Occurence", "Error" }, "RequestId = @RequestId AND Status = 'Running'", clientId);
                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request2.RequestID), "Update Request Status", new AriaLogElement[] { new AriaLogElement("Status", request2.Status.ToString()), new AriaLogElement("Error", request2.Error.Message) });
            }
        }

        private string RandomString(int size)
        {
            char[] chArray = new char[size];
            for (int i = 0; i < size; i++)
            {
                chArray[i] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[this._rng.Next("ABCDEFGHIJKLMNOPQRSTUVWXYZ".Length)];
            }
            return new string(chArray);
        }

        private void SendAgentMail(AriaRequest request, string clientId, AriaDataObjectPointer dataObjectPointer, AriaDataObjectPointerSettings pointerSetting, AriaDbConnection connection, AriaMessagingManager messaging, string outputFileName, AriaOptionGridXmlDataSet optionGrid, string OldFile, string NewOutPutFile, string OldOutPutFile, bool ReplaceAgentValues)
        {
            AriaEmail completeNotification = this.GetRequest(request.RequestID, clientId).CompleteNotification;
            if (ReplaceAgentValues)
            {
                if ((NewOutPutFile != null) && (NewOutPutFile.Trim() != ""))
                {
                    bool flag = false;
                    if (completeNotification.Attachment.ContainsKey(OldOutPutFile))
                    {
                        completeNotification.Attachment.Remove(OldOutPutFile);
                        flag = true;
                    }
                    completeNotification.Attachment[NewOutPutFile] = NewOutPutFile;
                    try
                    {
                        if ((completeNotification != null) && completeNotification.IsValid())
                        {
                            request.EventArgumentList = new AriaArgumentList();
                            request.EventArgumentList.AddArgument("Pointer", dataObjectPointer);
                            request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                            ((AriaArgument)request.EventArgumentList[0]).Settings = pointerSetting;
                        }
                        if ((NewOutPutFile == null) || File.Exists(NewOutPutFile))
                        {
                            messaging.SendEmail(connection, request.EventArgumentList, completeNotification, clientId);
                            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
                        }
                    }
                    catch (Exception exception)
                    {
                        throw exception;
                    }
                    completeNotification.Attachment.Remove(NewOutPutFile);
                    if (flag)
                    {
                        completeNotification.Attachment.Add(OldOutPutFile, OldOutPutFile);
                    }
                }
                else
                {
                    if ((completeNotification != null) && completeNotification.IsValid())
                    {
                        request.EventArgumentList = new AriaArgumentList();
                        request.EventArgumentList.AddArgument("Pointer", dataObjectPointer);
                        request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                        ((AriaArgument)request.EventArgumentList[0]).Settings = pointerSetting;
                    }
                    if ((outputFileName == null) || File.Exists(outputFileName))
                    {
                        messaging.SendEmail(connection, request.EventArgumentList, completeNotification, clientId);
                    }
                }
            }
            else
            {
                if ((completeNotification != null) && completeNotification.IsValid())
                {
                    request.EventArgumentList = new AriaArgumentList();
                    request.EventArgumentList.AddArgument("Pointer", dataObjectPointer);
                    request.EventArgumentList.BusinessObjectParameterName = "Pointer";
                    ((AriaArgument)request.EventArgumentList[0]).Settings = pointerSetting;
                }
                if ((outputFileName == null) || File.Exists(outputFileName))
                {
                    messaging.SendEmail(connection, request.EventArgumentList, completeNotification, clientId);
                    AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Send Email", messaging.GetLog());
                }
            }
            if (((ReplaceAgentValues && !string.IsNullOrEmpty(OldFile)) && (OldFile.Trim() != "")) && (optionGrid != null))
            {
                optionGrid.FileName = OldFile;
            }
        }

        public void UpdateObjectProgress(string requestId, AriaRequestProgress progress, string clientId)
        {
            string commandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RequestID", requestId));
            AriaDataProvider provider = new AriaDataProvider();
            AriaRequest record = (AriaRequest)provider.GetObjectList(command, typeof(AriaRequest))[0];
            if (record.Status == AriaRequestStatusTypes.Running)
            {
                record.Progress = progress;
                connection = new AriaDbConnection("Aria", "");
                provider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", record, new string[] { "Progress" }, "RequestId = @RequestId", clientId);
            }
            AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(record.RequestID), "Update Request Progress", new AriaLogElement[] { new AriaLogElement("Status", record.Status.ToString()), new AriaLogElement("Description", progress.Description), new AriaLogElement("Percent", progress.Percent.ToString()) });
        }

        public void UpdateRequestProcessID(string requestId, string clientId, int procID)
        {
            string commandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RequestID", requestId));
            AriaDataProvider provider = new AriaDataProvider();
            AriaRequest record = (AriaRequest)provider.GetObjectList(command, typeof(AriaRequest))[0];
            if (record.Status == AriaRequestStatusTypes.Running)
            {
                record.ProcessID = procID;
                connection = new AriaDbConnection("Aria", "");
                provider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", record, new string[] { "ProcessID" }, "RequestId = @RequestId", clientId);
            }
        }

        public void UpdateRequestStatus(string requestId, AriaRequestStatusTypes status, string error, string clientId)
        {
            string commandText = "SELECT * FROM AriaRequest WHERE RequestID = @RequestID";
            AriaDbConnection connection = new AriaDbConnection("Aria", "");
            AriaDbCommand command = new AriaDbCommand(commandText, connection, AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add(new AriaDbParameter("RequestID", requestId));
            AriaDataProvider provider = new AriaDataProvider();
            AriaRequest record = (AriaRequest)provider.GetObjectList(command, typeof(AriaRequest))[0];
            if ((error != null) && (error.Trim() != ""))
            {
                record.Error = new Exception(error);
            }
            record.Status = status;
            connection = new AriaDbConnection("Aria", "");
            provider.UpdateObject(connection, AriaDatabaseTypes.Aria50ClientSystemFiles, "AriaRequest", record, new string[] { "Status", "Error" }, "RequestId = @RequestId", clientId);
            if ((error != null) && (error.Trim() != ""))
            {
                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(record.RequestID), "Update Request Status", new AriaLogElement[] { new AriaLogElement("Status", record.Status.ToString()), new AriaLogElement("Error", error) });
            }
            else
            {
                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(record.RequestID), "Update Request Status", new AriaLogElement[] { new AriaLogElement("Status", record.Status.ToString()) });
            }
        }

        private void ValidateExectionTime(DateTime startExecTime, AriaRequest request, AriaMessagingManager messaging, AriaDbConnection connection, string clientId)
        {
            double totalSeconds = DateTime.Now.Subtract(startExecTime).TotalSeconds;
            AriaEnviromentVariables variables = new AriaEnviromentVariables(clientId);
            int clientMaxExecutionTimePerRequest = variables.ClientMaxExecutionTimePerRequest;
            double num2 = (variables.ClientMaxExecutionTimePerRequest != 0) ? ((double)variables.ClientMaxExecutionTimePerRequest) : ((variables.MaxExecutionTimePerRequest != 0) ? ((double)variables.MaxExecutionTimePerRequest) : ((double)600));
            if (totalSeconds > num2)
            {
                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Max Execution Time per Request exceeded", new AriaLogElement[] { new AriaLogElement("Request Number", request.RequestID), new AriaLogElement("Execution Time", totalSeconds.ToString()) });
                if (!(string.IsNullOrEmpty(variables.ExceedLimitNotificationEmail) && string.IsNullOrEmpty(variables.ClientExceedLimitNotificationEmail)))
                {
                    AriaEmail mailMessage = new AriaEmail();
                    mailMessage.To = string.IsNullOrEmpty(variables.ExceedLimitNotificationEmail) ? "" : variables.ExceedLimitNotificationEmail;
                    mailMessage.To = mailMessage.To + ((string.IsNullOrEmpty(mailMessage.To) || string.IsNullOrEmpty(variables.ClientExceedLimitNotificationEmail)) ? "" : (mailMessage.To + ";")) + (string.IsNullOrEmpty(variables.ClientExceedLimitNotificationEmail) ? "" : variables.ClientExceedLimitNotificationEmail);
                    mailMessage.Subject = "Max Execution Time per Request exceeded <<" + request.RequestNumber + ">>";
                    mailMessage.Body = string.Concat(new object[] { "Client = ", clientId, "\nRequest Number = ", request.RequestNumber, "\nExecution Time = ", totalSeconds.ToString(), " Seconds" });
                    messaging.SendEmail(connection, request.EventArgumentList, mailMessage, clientId);
                }
            }
        }

        public void ValidateRecordsPerReport(string requestId, string clientId, int numOfRecords)
        {
            AriaEnviromentVariables variables = new AriaEnviromentVariables(clientId);
            int clientMaxRecordsPerReport = variables.ClientMaxRecordsPerReport;
            double num = (variables.ClientMaxRecordsPerReport != 0) ? ((double)variables.ClientMaxRecordsPerReport) : ((variables.MaxRecordsPerReport != 0) ? ((double)variables.MaxRecordsPerReport) : ((double)600));
            if (numOfRecords > num)
            {
                AriaRequest request = this.GetRequest(requestId, clientId);
                AriaDbConnection connection = new AriaDbConnection();
                AriaMessagingManager manager = new AriaMessagingManager();
                connection.Context = request.Context;
                connection.CompanyName = request.Context.CompanyName;
                connection.Context.UserName = request.UserName;
                connection.Context.MethodName = request.MethodName;
                AriaDatabaseLogManager.AriaAddToLog(clientId, connection, new Guid(request.RequestID), "Max Number of Records per Report exceeded", new AriaLogElement[] { new AriaLogElement("Request Number", request.RequestID), new AriaLogElement("Number of Records", numOfRecords.ToString()) });
                if (!(string.IsNullOrEmpty(variables.ExceedLimitNotificationEmail) && string.IsNullOrEmpty(variables.ClientExceedLimitNotificationEmail)))
                {
                    AriaEmail mailMessage = new AriaEmail();
                    mailMessage.To = string.IsNullOrEmpty(variables.ExceedLimitNotificationEmail) ? "" : variables.ExceedLimitNotificationEmail;
                    mailMessage.To = mailMessage.To + ((string.IsNullOrEmpty(mailMessage.To) || string.IsNullOrEmpty(variables.ClientExceedLimitNotificationEmail)) ? "" : (mailMessage.To + ";")) + (string.IsNullOrEmpty(variables.ClientExceedLimitNotificationEmail) ? "" : variables.ClientExceedLimitNotificationEmail);
                    mailMessage.Subject = "Max Number of Records per Report exceeded <<" + request.RequestNumber + ">>";
                    mailMessage.Body = string.Concat(new object[] { "Client = ", clientId, "\nRequest Number = ", request.RequestNumber, "\nNumber of Records Per Report = ", numOfRecords.ToString() });
                    manager.SendEmail(connection, request.EventArgumentList, mailMessage, clientId);
                }
            }
        }
    }
}

