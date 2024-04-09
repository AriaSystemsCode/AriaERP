using System;
using System.Data.SqlClient;
using System.Data.Odbc;
using System.Data;
using Aria.Environment;
using System.Windows.Forms;
using System.Data.OleDb;
using System.Diagnostics;
using System.IO;
using System.Text.RegularExpressions;

namespace Aria.Data
{
    /// <summary>
    /// Used to execute the SQL statement used some information as connection and database type
    /// </summary>
    public class AriaDbCommand
    {
        private AriaDbConnection _connection = null;
        public AriaDbConnection Connection
        {
            get { return _connection; }
            set { _connection = value; }
        }

        private AriaDatabaseTypes _databaseType;
        public AriaDatabaseTypes DatabaseType
        {
            get { return _databaseType; }
            set { _databaseType = value; }
        }

        private string _commandText;
        public string CommandText
        {
            get { return _commandText; }
            set { _commandText = value; }
        }

        private AriaDbParameterCollection _parameters = new AriaDbParameterCollection();
        public AriaDbParameterCollection Parameters
        {
            get { return _parameters; }
            set { _parameters = value; }
        }   

        private string _connectionString;

        public AriaDbCommand(string commandText, AriaDbConnection connection, AriaDatabaseTypes commandDatabaseType) : this(commandText, connection, commandDatabaseType, "")
        {
        }
        
        public AriaDbCommand(string commandText, AriaDbConnection connection, AriaDatabaseTypes commandDatabaseType, string clientId)
        {
            _commandText = commandText;
            _connection = connection;
            _databaseType = commandDatabaseType;

            AriaEnviromentVariables env = new AriaEnviromentVariables();
            //T20100512.0026 Hassan 2010 05 23 [Begin]
            env.ClientID = clientId;
            env.ConnectionsRefresh();
            //T20100512.0026 Hassan 2010 05 23 [END]
            switch (_databaseType)
            {
                case AriaDatabaseTypes.Aria27Data:
                    _connectionString = env.GetAria27CompanyDataConnectionString(connection.CompanyName);
                    break;

                case AriaDatabaseTypes.Aria27SystemFiles:
                    _connectionString = env.Aria27SystemFilesConnectionString;
                    break;

                case AriaDatabaseTypes.Aria40Data:
                    _connectionString = env.GetAria04CompanyDataConnectionString(connection.CompanyName);
                    break;

                case AriaDatabaseTypes.Aria40SystemFiles:
                    _connectionString = env.Aria40SystemFilesConnectionString;
                    break;

                case AriaDatabaseTypes.Aria50SystemFiles:
                    // T20110803.0001 MAH 8/2/2011
                    // _connectionString = env.Aria50SystemFilesConnectionString;
                    if(string.IsNullOrEmpty(clientId))
                    {
                        _connectionString = env.Aria50SystemFilesConnectionString;
                    }
                    else
                    {
                        _connectionString = env.Aria50ClientSystemFilesConnectionString; 
                    }

                    // T20110803.0001 MAH 8/2/2011
                    break;

                case AriaDatabaseTypes.Aria50ClientSystemFiles:
                    _connectionString = env.Aria50ClientSystemFilesConnectionString;
                    break;

                case AriaDatabaseTypes.AriaOpenRowSet:
                    _connectionString = env.AriaOpenRowSetConnectionString;
                    break;
            }

            _connectionString = _connectionString.Replace("<CustomerName>", _connection.CustomerName)
                                                  .Replace("<CompanyName>", _connection.CompanyName);
        }

        public int ExecuteNonQuery()
        {
            switch (_databaseType)
            {
                case AriaDatabaseTypes.Aria27Data:
                    return FoxExcuteNonQuery();

                case AriaDatabaseTypes.Aria27SystemFiles:
                    return FoxExcuteNonQuery();

                case AriaDatabaseTypes.Aria40Data:
                    return SqlExcuteNonQuery();

                case AriaDatabaseTypes.Aria40SystemFiles:
                    return FoxExcuteNonQuery();

                case AriaDatabaseTypes.Aria50SystemFiles:
                    return SqlExcuteNonQuery();

                case AriaDatabaseTypes.Aria50ClientSystemFiles:
                    return SqlExcuteNonQuery();
            }

            return -1;
        }

        private int SqlExcuteNonQuery()
        {
            SqlCommand command = new SqlCommand(_commandText);

            // T20110803.0001 MAH 8/2/2011
            command.CommandTimeout = 0;
            // T20110803.0001 MAH 8/2/2011 End

            command.Connection = new SqlConnection(_connectionString);

            for (int parameterIndex = 0; parameterIndex < _parameters.Count; parameterIndex++)
            {
                command.Parameters.Add(new SqlParameter(_parameters[parameterIndex].ParameterName, _parameters[parameterIndex].ParameterValue));
            }

            command.Connection.Open();

            int returnValue = 0;
            try
            {
                returnValue = command.ExecuteNonQuery();
            }
            catch(Exception)
            {
            }
            
            command.Connection.Close();

            return returnValue;
        }

        private int FoxExcuteNonQuery()
        {            
            OdbcCommand command = new OdbcCommand(_commandText);
            command.Connection = new OdbcConnection(_connectionString);

            for (int parameterIndex = 0; parameterIndex < _parameters.Count; parameterIndex++)
            {
                // T20110803.0001 MAH 8/2/2011
                //string value = _parameters[parameterIndex].ParameterValue.ToString();
                //value = value.Replace("'", "' + chr(39) + '");
                string value = "";

                if (_parameters[parameterIndex].ParameterValue.GetType().FullName == typeof(DateTime).FullName)
                {
                    DateTime dateTimeValue = Convert.ToDateTime(_parameters[parameterIndex].ParameterValue);

                    value = "^" + dateTimeValue.Year.ToString() + 
                            "/" + dateTimeValue.Month.ToString().PadLeft(2, '0') +
                            "/" + dateTimeValue.Day.ToString().PadLeft(2, '0');
                }
                else
                {
                    value = _parameters[parameterIndex].ParameterValue.ToString();
                    value = value.Replace("'", "' + chr(39) + '");
                }
                // T20110803.0001 MAH 8/2/2011 End


                // T20110803.0001 MAH 8/2/2011
                // command.CommandText = command.CommandText.Replace("@" + _parameters[parameterIndex].ParameterName.TrimEnd() + "@", value);
                if (command.CommandText.ToUpper().Contains("@" + _parameters[parameterIndex].ParameterName.ToUpper() + "@"))
                {
                    int start = command.CommandText.ToUpper().IndexOf("@" + _parameters[parameterIndex].ParameterName.ToUpper() + "@");
                    string actParamName = command.CommandText.Substring(start, ("@" + _parameters[parameterIndex].ParameterName.ToUpper() + "@").Length);

                    command.CommandText = command.CommandText.Replace(actParamName, value);
                }
                // T20110803.0001 MAH 8/2/2011 End
            }

            command.Connection.Open();

            OdbcCommand ansiCommand = new OdbcCommand("SET ANSI OFF", command.Connection);
            ansiCommand.ExecuteNonQuery();

            try
            {
                int returnValue = command.ExecuteNonQuery();

                command.Connection.Close();

                return returnValue;
            }
            catch (Exception ex)
            {
                command.Connection.Close();

                throw ex;
            }
        }

        public DataTable GetDataTable()
        {
            switch (_databaseType)
            {
                case AriaDatabaseTypes.Aria27Data:
                    return FoxGetDataTable();

                case AriaDatabaseTypes.Aria27SystemFiles:
                    return FoxGetDataTable();

                case AriaDatabaseTypes.Aria40Data:
                    return SqlGetDataTable();

                case AriaDatabaseTypes.Aria40SystemFiles:
                    return FoxGetDataTable();

                case AriaDatabaseTypes.Aria50SystemFiles:
                    return SqlGetDataTable();

                case AriaDatabaseTypes.Aria50ClientSystemFiles:
                    return SqlGetDataTable();

                case AriaDatabaseTypes.AriaOpenRowSet:
                    return SqlGetDataTable();
            }

            return null;
        }

        private DataTable SqlGetDataTable()
        {
            SqlCommand command = new SqlCommand(_commandText);

            // T20110803.0001 MAH 8/2/2011
            command.CommandTimeout = 0;
            // T20110803.0001 MAH 8/2/2011 End

            command.Connection = new SqlConnection(_connectionString);
            
            command.Connection.Open();

			// MOH T20100226.0004 Start
            Regex regexObj = new Regex("'@.+@'");
            Match matchResults = regexObj.Match(command.CommandText);
            while (matchResults.Success)
            {
                for (int i = 0; i < matchResults.Groups.Count; i++)
                {
                    Group groupObj = matchResults.Groups[i];
                    if (groupObj.Success)
                    {
                        string newvalue = groupObj.Value.Replace("'@", "@").Replace("@'", "");
                        command.CommandText = command.CommandText.Replace(groupObj.Value, newvalue);
                    }
                }
                matchResults = matchResults.NextMatch();
            }
            // MOH T20100226.0004 Start
			
            for (int parameterIndex = 0; parameterIndex < _parameters.Count; parameterIndex++)
            {
                command.Parameters.Add(new SqlParameter(_parameters[parameterIndex].ParameterName, _parameters[parameterIndex].ParameterValue));            
            }
            
            DataTable table = new DataTable();
            
            SqlDataAdapter adapter = new SqlDataAdapter(command);
            

            


            adapter.Fill(table);
            
            command.Connection.Close();

            return table;
        }

        private DataTable FoxGetDataTable()
        {
            OdbcCommand command = new OdbcCommand(this.CommandText);
            
            OdbcConnection connection = new OdbcConnection(_connectionString);

            connection.Open();

            OdbcCommand ansiCommand = new OdbcCommand("SET ANSI OFF", connection);
            ansiCommand.ExecuteNonQuery();

            for (int parameterIndex = 0; parameterIndex < _parameters.Count; parameterIndex++)
            {
                // T20110803.0001 MAH 8/2/2011
                //string value = _parameters[parameterIndex].ParameterValue.ToString();
                //value = value.Replace("'", "' + chr(39) + '");

                string value;
                if (_parameters[parameterIndex].ParameterValue.GetType().FullName == typeof(DateTime).FullName)
                {
                    DateTime dateTimeValue = Convert.ToDateTime(_parameters[parameterIndex].ParameterValue);

                    value = "^" + dateTimeValue.Year.ToString() +
                            "/" + dateTimeValue.Month.ToString().PadLeft(2, '0') +
                            "/" + dateTimeValue.Day.ToString().PadLeft(2, '0');
                }
                else
                {
                    value = _parameters[parameterIndex].ParameterValue.ToString();
                    value = value.Replace("'", "' + chr(39) + '");
                }
                // T20110803.0001 MAH 8/2/2011

                // T20110803.0001 MAH 8/2/2011
                //command.CommandText = command.CommandText.ToUpper().Replace("@" + _parameters[parameterIndex].ParameterName.ToUpper() + "@", value);

                if(command.CommandText.ToUpper().Contains("@" + _parameters[parameterIndex].ParameterName.ToUpper() + "@"))
                {
                    int start = command.CommandText.ToUpper().IndexOf("@" + _parameters[parameterIndex].ParameterName.ToUpper() + "@");
                    string actParamName = command.CommandText.Substring(start, ("@" + _parameters[parameterIndex].ParameterName.ToUpper() + "@").Length);

                    command.CommandText = command.CommandText.Replace(actParamName, value);
                }
                // T20110803.0001 MAH 8/2/2011
            }

            DataTable table = new DataTable();

            OdbcDataAdapter adapter = new OdbcDataAdapter(command.CommandText, connection);

            adapter.Fill(table);

            connection.Dispose();
            connection = null;
            return table;
        }
    }
}
