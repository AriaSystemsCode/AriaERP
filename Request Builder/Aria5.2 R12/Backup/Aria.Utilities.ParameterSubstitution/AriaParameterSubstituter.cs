using System;
using System.Data;
using System.Collections.Generic;
using Aria.DataTypes.ObjectDictionary;
using Aria.DataTypes;
using Aria.Data.DataTypes;
using Aria.Reflection;
using Aria.Data;
using Aria.Environment;
using Aria.DataTypes.RequestHandler;
using System.Collections;
using System.Reflection;
using System.Diagnostics;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.DataTypes.Settings;
//[Trace]Ahmed Maher
using System.IO;
using Aria.HelperClass.SAAS;

namespace Aria.Utilities.ParameterSubstitution
{
    /// <summary>
    /// Contain some method to substituted parameter
    /// </summary>
    public class AriaParameterSubstituter : MarshalByRefObject
    {
        protected AriaDataTypeAdapter _argumentsDataProvider;

        private AriaArgumentList _argumentList;
        public AriaArgumentList ArgumentList
        {
            get { return _argumentList; }
            set { _argumentList = value; }
        }

        private AriaDbConnection _connection;
        public AriaDbConnection Connection
        {
            get { return _connection; }
            set { _connection = value; }
        }

        private string _objectName = "";
        private bool _isData = false;

        public AriaParameterSubstituter(AriaDbConnection connection, AriaArgumentList argumentList,string clientId)
        {
            _argumentList = argumentList;
            Connection = connection;

            BuildArgumentsDataProvider(connection, argumentList, clientId);
        }
        //MOH T20100915.0013 10-1-2011 add constructor without paramters so this dll will be accissable from Fox [Start]
        public AriaParameterSubstituter()
        
        {
            EventLog.WriteEntry("Request Error0", "start", EventLogEntryType.Information);
        }
        public string ClientID { get; set; }
        public void BuildArgumentsDataProvider1()
        {
            BuildArgumentsDataProvider(Connection, ArgumentList, ClientID);
        }

        public void BuildArgumentsDataProvider()
        {
            BuildArgumentsDataProvider(Connection, ArgumentList, ClientID);
        }
        //MOH T20100915.0013 10-1-2011 add constructor without paramters so this dll will be accissable from Fox [End]


        private void BuildArgumentsDataProvider(AriaDbConnection connection, AriaArgumentList sourceArguments, string  clientId)
        {

            EventLog.WriteEntry("Request Error1", "start", EventLogEntryType.Information);

            //SAB 04-09-2013 Remove Write to Event Log Test Code [Start]
            ////[Add]Ahmed Maher -Date: 01/04/2010
            //AriaLogManage M = new AriaLogManage();
            ////[END]
            //SAB 04-09-2013 Remove Write to Event Log Test Code [End]
            try
            {
                //SAB 04-09-2013 Remove Write to Event Log Test Code [Start]
                //M.AddLog("Trace", "BuildArgumentsDataProvider", "sourceArguments Count = " + sourceArguments.Count, clientId);
                //SAB 04-09-2013 Remove Write to Event Log Test Code [End]
                for (int argumentIndex = 0; argumentIndex < sourceArguments.Count; argumentIndex++)
                {
                    //SAB 04-09-2013 Remove Write to Event Log Test Code [Start]
                    //M.AddLog("Trace", "BuildArgumentsDataProvider", "sourceArguments = " + sourceArguments[argumentIndex].ToString(), clientId);
                    //SAB 04-09-2013 Remove Write to Event Log Test Code [End]
                    if (((AriaArgument)sourceArguments[argumentIndex]).Value.GetType().GetCustomAttributes(typeof(AriaDataTypeDataAdapterAttribute), true).Length > 0)
                    {
                        string className = ((AriaDataTypeDataAdapterAttribute)((AriaArgument)sourceArguments[argumentIndex]).Value.GetType().
                                            GetCustomAttributes(typeof(AriaDataTypeDataAdapterAttribute), true)[0]).ClassName;
                        if (className == "Aria.Data.DataTypes.AriaDataObjectPointerAdapter")
                        {
                            Aria.Data.DataTypes.AriaDataObjectPointerAdapter dataProvider =
                                    new AriaDataObjectPointerAdapter(connection,
                                        (Aria.DataTypes.Settings.AriaDataObjectPointerSettings)((AriaArgument)sourceArguments[argumentIndex]).Settings,
                                            (AriaDataObjectPointer)((AriaArgument)sourceArguments[argumentIndex]).Value, clientId);

                            _objectName = ((Aria.DataTypes.Settings.AriaDataObjectPointerSettings)((AriaArgument)sourceArguments[argumentIndex]).Settings).DataObjectName;
                            _isData = true;

                            _argumentsDataProvider = (AriaDataTypeAdapter)dataProvider;
                        }

                        else if (className == "Aria.Data.DataTypes.AriaOptionGridXmlDataSetAdapter")
                        {
                            Aria.Data.DataTypes.AriaOptionGridXmlDataSetAdapter dataProvider =
                                new Aria.Data.DataTypes.AriaOptionGridXmlDataSetAdapter(connection,
                                        ((AriaArgument)sourceArguments[argumentIndex]).Settings,
                                            ((AriaArgument)sourceArguments[argumentIndex]).Value);

                            _argumentsDataProvider = (AriaDataTypeAdapter)dataProvider;
                        }
                    }
                }
            }
            catch (Exception Ex)
            {
                EventLog.WriteEntry("Request Error1", Ex.Message, EventLogEntryType.Information);
                EventLog.WriteEntry("Request Error1", Ex.GetBaseException().Message, EventLogEntryType.Information);
                //SAB 04-09-2013 Remove Write to Event Log Test Code [Start]
                //M.AddLog("Error", "Aria.Utilities.ParameterSubstitution.BuildArgumentsDataProvider", Ex.Message, clientId);
                //SAB 04-09-2013 Remove Write to Event Log Test Code [End]
            }
        }

        private object GetValue(string paramterizeValue,string clientId)
        {

            EventLog.WriteEntry("Request Error3", paramterizeValue, EventLogEntryType.Information);
            //Trace
            AriaLogManage M = new AriaLogManage();
            try
            {
                if (paramterizeValue.IndexOf(':') < 0)
                {
                    if (_isData)
                    {
                        string fullPath = _objectName + "." + paramterizeValue;
                        string propertyObjectName = fullPath.Substring(0, fullPath.LastIndexOf('.'));
                        string propertyName = fullPath.Substring(fullPath.LastIndexOf('.') + 1);

                        AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
                        AriaDataTypeSettings setting = objectDictionary.LoadAriaObjectProperty(new AriaDbConnection("", ""),
                                                                propertyObjectName,
                                                                objectDictionary.LoadActiveRevision(new AriaDbConnection("", ""),
                                                                                propertyObjectName, clientId).ObjectRevision,
                                                                                    propertyName, clientId).PropertySettings;
                        //SAB 01-20-2014 Remove the time part from the Date fields [Start]
                        bool isDate = false;
                        //SAB 01-20-2014 Remove the time part from the Date fields [End]
                        bool isEmail = false;
                        //SAB 01-16-2014 Add the ability to use multiple separator for emails [Start]
                        //char emailSep = '|';
                        char[] emailSep = ("|,;").ToCharArray();
                        //SAB 01-16-2014 Add the ability to use multiple separator for emails [End]
                        bool isInternal = false;
                        if (setting is AriaFieldSettings)
                        {
                            isEmail = ((AriaFieldSettings)setting).IsEmail;
                            if (((AriaFieldSettings)setting).EmailAddressSeparator != null &&
                               ((AriaFieldSettings)setting).EmailAddressSeparator.ToString().TrimEnd() != "")
                            {
                                //SAB 01-16-2014 Add the ability to use multiple separator for emails [Start]
                                //emailSep = ((AriaFieldSettings)setting).EmailAddressSeparator.ToCharArray()[0];
                                emailSep = ((AriaFieldSettings)setting).EmailAddressSeparator.ToCharArray();
                                //SAB 01-16-2014 Add the ability to use multiple separator for emails [End]
                            }
                            isInternal = ((AriaFieldSettings)setting).InternalEmail;
                            //SAB 01-20-2014 Remove the time part from the Date fields [Start]
                            isDate = (((AriaFieldSettings)setting).DataType == AriaStandardDataTypes.Date);
                            //SAB 01-20-2014 Remove the time part from the Date fields [End]
                        }
                        if (setting is AriaRelatedFieldSettings)
                        {   
                            if (((AriaRelatedFieldSettings)setting).EmailAddressSeparator != null &&
                               ((AriaRelatedFieldSettings)setting).EmailAddressSeparator.ToString().TrimEnd() != "")
                            {
                                //SAB 01-16-2014 Add the ability to use multiple separator for emails [Start]
                                //emailSep = ((AriaRelatedFieldSettings)setting).EmailAddressSeparator.ToCharArray()[0];
                                emailSep = ((AriaRelatedFieldSettings)setting).EmailAddressSeparator.ToCharArray();
                                //SAB 01-16-2014 Add the ability to use multiple separator for emails [End]
                            }
                            isInternal = ((AriaRelatedFieldSettings)setting).InternalEmail;
                            //SAB 01-20-2014 Remove the time part from the Date fields [Start]
                            isDate = (((AriaRelatedFieldSettings)setting).DataType == AriaStandardDataTypes.Date);
                            //SAB 01-20-2014 Remove the time part from the Date fields [End]
                        }

                        if (isEmail)
                        {
                            string result = "";
                            //SAB 01-16-2014 Add the ability to use multiple separator for emails [Start]
                            //string[] users = _argumentsDataProvider.GetData(paramterizeValue, clientId).ToString().Split(new char[] { emailSep });
                            string[] users = _argumentsDataProvider.GetData(paramterizeValue, clientId).ToString().Split(emailSep);
                            //SAB 01-16-2014 Add the ability to use multiple separator for emails [End]
                            
                            for (int index = 0; index < users.Length; index++)
                            {
                                
                                #region isInternal
                                if (isInternal)
                                {
                                    //[Modified]Ahmed Maher
                                    string commandText = "SELECT Cemail_add FROM Syuuser WHERE UPPER(Cuser_id) = UPPER('@UserId@')";
                                    AriaDbCommand command = new AriaDbCommand(commandText, Connection, AriaDatabaseTypes.Aria27SystemFiles, clientId);
                                    command.Parameters.Add(new AriaDbParameter("UserId", users[index].TrimEnd()));



                                    if (string.IsNullOrEmpty(((AriaFieldSettings)setting).InternalEmailSelect))
                                    {
                                        if (command.GetDataTable().Rows.Count > 0)
                                        {
                                            if (result.TrimEnd().Length > 0) result += ", ";

                                            result += command.GetDataTable().Rows[0][0].ToString().TrimEnd()  ;
                                        }
                                    }
                                    else
                                    {
                                        commandText = ((AriaFieldSettings)setting).InternalEmailSelect;
                                        string Parameters = ((AriaFieldSettings)setting).InternalEmailParameter;
                                        string[] ParameterList = Parameters.Split(';');
                                        for (int i = 0; i < ParameterList.Length; i++)
                                        {
                                            string[] ParaRefer = ParameterList[i].Split(',');
                                            if (ParaRefer[0] == "UserId")
                                                commandText = commandText.Replace(ParaRefer[1],
                                                users[index]);
                                            else
                                                commandText = commandText.Replace(ParaRefer[1],
                                                    _argumentsDataProvider.GetData(ParaRefer[0], clientId).ToString());
                                        }
                                        AriaDatabaseTypes DBType = (AriaDatabaseTypes)
                                            Enum.Parse(typeof(AriaDatabaseTypes),
                                            ((AriaFieldSettings)setting).InternalEmailConnectionType, true);

                                        AriaDbCommand command1 = new AriaDbCommand
                                            (commandText, Connection, DBType, clientId);

                                        if (command1.GetDataTable().Rows.Count > 0)
                                        {
                                            if (string.IsNullOrEmpty(command1.GetDataTable().Rows[0][0].ToString().Trim()))
                                            {
                                                if (command.GetDataTable().Rows.Count > 0)
                                                {
                                                    if (result.TrimEnd().Length > 0) result += ", ";

                                                    result += command.GetDataTable().Rows[0][0];
                                                }
                                            }
                                            else
                                            {
                                                if (result.TrimEnd().Length > 0) result += ", ";

                                                result += command1.GetDataTable().Rows[0][0];
                                            }
                                        }
                                        else
                                        {
                                            if (command.GetDataTable().Rows.Count > 0)
                                            {
                                                if (result.TrimEnd().Length > 0) result += ", ";

                                                result += command.GetDataTable().Rows[0][0];
                                            }
                                        }
                                    }


                                    //[END]
                                }
                                else
                                {
                                    if (result.TrimEnd().Length > 0) result += ", ";
                                    result += users[index];
                                }
                                #endregion
                            }
                            result = result.TrimEnd(); 
                            if (result.Substring(result.Length-1)=="," )
                            { result = result.Substring(result.Length - 1); };

                            return result;
                        }
                        else
                        {
                            //SAB Trim empty space from the subistitued text 04-18-2013 [Start]
                            //return _argumentsDataProvider.GetData(paramterizeValue, clientId);
                            try
                            {
                                object returnedResult = _argumentsDataProvider.GetData(paramterizeValue, clientId);
                                //SAB 01-20-2014 Remove the time part from the Date fields [Start]
                                if (isDate && returnedResult != DBNull.Value)
                                {
                                    returnedResult = ((DateTime)returnedResult).ToShortDateString();
                                }
                                //SAB 01-20-2014 Remove the time part from the Date fields [End]
                                if (returnedResult.GetType().Name == "String")
                                {
                                    return returnedResult.ToString().Trim();
                                }
                                else
                                {
                                    return returnedResult;
                                }
                            }
                            catch (Exception Ex)
                            {
                                EventLog.WriteEntry("Request Error3", Ex.Message, EventLogEntryType.Information);
                                EventLog.WriteEntry("Request Error3", Ex.GetBaseException().Message, EventLogEntryType.Information);

                                return _argumentsDataProvider.GetData(paramterizeValue, clientId);
                            }
                            //SAB Trim empty space from the subistitued text 04-18-2013 [End]
                        }
                    }
                    else
                    {
                        return _argumentsDataProvider.GetData(paramterizeValue, clientId);
                    }
                }
                else
                {
                    string ParameterDataPath = paramterizeValue.Split(':')[1];
                    string ParameterName = paramterizeValue.Split(':')[0];

                    if (ParameterName.Equals("AriaUser"))
                    {
                        string commandText = "SELECT Cemail_add FROM Syuuser WHERE UPPER(Cusr_name) = UPPER('@UserName@')";

                        AriaDbCommand command = new AriaDbCommand(commandText, Connection, AriaDatabaseTypes.Aria27SystemFiles, clientId);

                        command.Parameters.Add(new AriaDbParameter("UserName", ParameterDataPath.Trim()));

                        if (command.GetDataTable().Rows.Count == 0)
                        {
                            return "";
                        }

                        return command.GetDataTable().Rows[0][0];
                    }
                    else if (ParameterName.Equals("Request"))
                    {
                        if (ParameterDataPath.Equals("UserName"))
                        {
                            string commandText = "SELECT cusr_name FROM Syuuser WHERE UPPER(Cuser_Id) = UPPER('@UserName@')";

                            AriaDbCommand command = new AriaDbCommand(commandText, Connection, AriaDatabaseTypes.Aria27SystemFiles, clientId);

                            command.Parameters.Add(new AriaDbParameter("UserName", Connection.Context.UserName));

                            if (command.GetDataTable().Rows.Count == 0)
                            {
                                return "";
                            }

                            return command.GetDataTable().Rows[0][0];
                        }
                        else if (ParameterDataPath.Equals("UserEmail"))
                        {
                            string commandText = "SELECT Cemail_add FROM Syuuser WHERE UPPER(Cuser_Id) = UPPER('@UserName@')";

                            AriaDbCommand command = new AriaDbCommand(commandText, Connection, AriaDatabaseTypes.Aria27SystemFiles, clientId);

                            command.Parameters.Add(new AriaDbParameter("UserName", Connection.Context.UserName));

                            if (command.GetDataTable().Rows.Count == 0)
                            {
                                return "";
                            }

                            return command.GetDataTable().Rows[0][0];
                        }
                        else if (ParameterDataPath.Equals("MethodName"))
                        {
                            return Connection.Context.MethodName;
                        }
                        else if (ParameterDataPath.Equals("DateTimeStamp"))
                        {
                            AriaDataProvider provider = new AriaDataProvider();
                            AriaDbCommand command = new AriaDbCommand("SELECT * FROM AriaRequest WHERE RequestId = @RequestID", new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
                            command.Parameters.Add("RequestID", Connection.Context.RequestId);
                            AriaRequest request = (AriaRequest)provider.GetObjectList(command, typeof(AriaRequest))[0];



                            return request.StartAfterDate.AddHours(request.RequestStartTime.Hour)
                                                         .AddMinutes(request.RequestStartTime.Minute)
                                                         .AddSeconds(request.RequestStartTime.Second).ToString();

                        }
                        else
                        {
                            return "";
                        }
                    }
                    else
                    {
                        return _argumentsDataProvider.GetData(ParameterDataPath, clientId);
                    }
                }
            }
            catch (Exception Ex)
            {
                //SAB 04-09-2013 Remove Write to Event Log Test Code [Start]
                //M.AddLog("Error", "Aria.Utilities.ParameterSubstitution.GetValue", Ex.Message, clientId);
                //SAB 04-09-2013 Remove Write to Event Log Test Code [End]

                EventLog.WriteEntry("Request Error3", Ex.Message, EventLogEntryType.Information);
                EventLog.WriteEntry("Request Error3", Ex.GetBaseException().Message, EventLogEntryType.Information);

            }
            return "";
        }

        public string GetSubstitutedText(string targetText,string clientId)
        {
            EventLog.WriteEntry("Aria Sub1", "Start");

            string LeftRectangularBracket = Guid.NewGuid().ToString();

            targetText = targetText.Replace("<<", LeftRectangularBracket);

            string RightRectangularBracket = Guid.NewGuid().ToString();

            targetText = targetText.Replace(">>", RightRectangularBracket);

            string[] tempTags = targetText.Split('<', '>');

            string[] tags = new string[tempTags.Length / 2];

            for (int index = 1; index < tempTags.Length; index += 2)
            {
                tags[index / 2] = tempTags[index];

            }

            for (int tagIndex = 0; tagIndex < tags.Length; tagIndex++)
            {
                try
                {                    
                    targetText = targetText.Replace("<" + tags[tagIndex] + ">", GetValue(tags[tagIndex], clientId).ToString());
                }
                catch (Exception ex)
                {
                    EventLog.WriteEntry("Request Error2", ex.Message, EventLogEntryType.Information);
                    EventLog.WriteEntry("Request Error2", ex.GetBaseException().Message, EventLogEntryType.Information);
                }
            }

            targetText = targetText.Replace(LeftRectangularBracket, "<");

            targetText = targetText.Replace(RightRectangularBracket, ">");

            return targetText;
        }

        public void DeepSubstitute(AriaDataType ariaDataType, string clientId)
        {
            foreach (DictionaryEntry entry in ariaDataType.PropertyDataPathDictionary)
            {
                ariaDataType.GetType().GetProperty(entry.Key.ToString()).SetValue(ariaDataType, GetValue(entry.Value.ToString(), clientId), null);

            }

            PropertyInfo[] properties = ariaDataType.GetType().GetProperties();

            for (int index = 0; index < properties.Length; index++)
            {
                if (properties[index].GetValue(ariaDataType, null) is Aria.DataTypes.AriaDataType)
                    DeepSubstitute((AriaDataType)properties[index].GetValue(ariaDataType,null), clientId);
                    

                else if (properties[index].GetValue(ariaDataType, null) is System.Collections.ArrayList)
                {
                    ArrayList items = (ArrayList)properties[index].GetValue(ariaDataType, null);

                    for (int itemIndex = 0; itemIndex < items.Count; itemIndex++)
                    {
                        if (items[itemIndex] is Aria.DataTypes.AriaDataType)
                            DeepSubstitute((AriaDataType)items[itemIndex], clientId);
                    }
                }
            }
        }

        public object GetSubstitutedValue(AriaParameterizedData parameterizedData)
        {
            throw new NotImplementedException();
        }

        public string GetTimeStamp(string  clientId)
        {
            AriaDataProvider provider = new AriaDataProvider();
            AriaDbCommand command = new AriaDbCommand("SELECT * FROM AriaRequest WHERE RequestId = @RequestID", new AriaDbConnection("Aria", ""), AriaDatabaseTypes.Aria50ClientSystemFiles, clientId);
            command.Parameters.Add("RequestID", Connection.Context.RequestId);
            AriaRequest request = (AriaRequest)provider.GetObjectList(command, typeof(AriaRequest))[0];

            DateTime result = request.RequestStartTime;
            result.AddYears(request.StartAfterDate.Year);
            result.AddMonths(request.StartAfterDate.Month);
            result.AddDays(request.StartAfterDate.Day);

            return result.ToUniversalTime().ToString().Trim();
        }

        public override object InitializeLifetimeService()
        {
            return null;
        }
    }
}
