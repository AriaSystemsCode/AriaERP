using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Data;
using System.Data.SqlClient;
using Aria.Environment;

namespace Aria.HelperClass.SAAS
{
    public enum LogType
    {
        BreakPoint,
        Error
    }
    public class AriaLogManage
    {
        public SqlConnection SQLConnection(string clientId)
        {
            AriaEnviromentVariables Env = new AriaEnviromentVariables();
            //T20100512.0026 Hassan 2010 05 23 [Begin]
            Env.ClientID = clientId;
            Env.ConnectionsRefresh();
            //T20100512.0026 Hassan 2010 05 23 [END]

            SqlConnection Conn = new SqlConnection();
            Conn.ConnectionString = Env.Aria50SystemFilesConnectionString;
            Conn.Open();
            return Conn;
        }

        public bool AddLog(LogType logType, string Caller, string Message, string Note,string clientId)
        {
            try
            {
                string InsertStat = "INSERT INTO [Log]([LogType],[Caller],[Message],[Note])"
                    + " VALUES('"
                    + logType.ToString() + "','"
                    + Caller + "','"
                    + Message + "','"
                    + Note + "')";
                SqlCommand Cmd = new SqlCommand(InsertStat, SQLConnection(clientId));
                int x = Cmd.ExecuteNonQuery();
                if (x > 0) return true; else return false;
            }
            catch (Exception Ex)
            {
                //StreamWriter SS = new StreamWriter("C:\\AriaLogError.txt");
                //SS.WriteLine(Ex.Message);
                //SS.Flush();
                //SS.Close();
            }
            return false;
        }

        public bool AddLog(string logType, string Caller, string Message, string clientId)
        {
            try
            {
                string InsertStat = "INSERT INTO [Log]([LogType],[Caller],[Message])"
                    + " VALUES('"
                    + logType.ToString() + "','"
                    + Caller + "','"
                    + Message + "')";
                SqlCommand Cmd = new SqlCommand(InsertStat, SQLConnection(clientId));
                int x = Cmd.ExecuteNonQuery();
                if (x > 0) return true; else return false;
            }
            catch (Exception Ex)
            {
                //StreamWriter SS = new StreamWriter("C:\\AriaLogError.txt");
                //SS.WriteLine(Ex.Message);
                //SS.Flush();
                //SS.Close();
            }
            return false;
        }

    }
}
