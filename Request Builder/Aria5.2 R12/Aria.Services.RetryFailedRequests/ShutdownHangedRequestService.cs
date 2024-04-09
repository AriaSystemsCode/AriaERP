using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.ServiceProcess;
using System.Text;
using Aria.Environment;
using System.Collections;
using System.Data.SqlClient;

namespace Aria.Services.ShutdownHangedRequestService
{
    public partial class ShutdownHangedRequestService : ServiceBase
    {

        DataTable Clients = new DataTable(); //tmi        [T20140415.0001] define this datatableto be called only once the service ran

        public ShutdownHangedRequestService()
        {
            InitializeComponent();
        }

        int minutes = 15;

         protected override void OnStart(string[] args)
        {
            int time;

            if (args.Length > 0 && Int32.TryParse(args[0], out time))
            {
                if(time > 0)
                {
                    minutes = time;
                }
            }


            ////tmi [T20140415.0001][start] get clients list running request builder service on this pc
            //AriaEnviromentVariables EnviromentVariables = new AriaEnviromentVariables();
            //SqlCommand clientsCom = new SqlCommand("SELECT * FROM CLIENTS where REQSERVER = @server");            
            //clientsCom.Parameters.Add(new SqlParameter("@server", System.Environment.MachineName));
            //clientsCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            //clientsCom.Connection.Open();
            //Clients.Load(clientsCom.ExecuteReader());
            //clientsCom.Connection.Close();
            ////tmi [T20140415.0001][end] get clients list            

            EventLog.WriteEntry("Aria Shutdown Hanged Request Service", "Start", EventLogEntryType.Information);
            Timer.Start();
        }

        protected override void OnStop()
        {
            Timer.Stop();
        }

        void Timer_Elapsed(object sender, System.Timers.ElapsedEventArgs e)
        {
            ServiceBody();
        }

        private void ServiceBody()
        {

            EventLog.WriteEntry("Aria Shutdown Hanged Request Service", "Run", EventLogEntryType.Information);

            //tmi [start] get process id list that are Running agents with processID field is not empty
            //List<int> processIdList = getProcessIdList();
            //tmi [end]

            foreach (var proc in Process.GetProcesses())
            {
                //tmi[start] skip process IDs not in the collected list
                //if (!processIdList.Exists(procId => procId == proc.Id)) continue;
                //tmi [end]

                //tmi , [T20140415.0001] for some processes,it does not define the StartTime and throws and exception
                //if (proc.ProcessName.ToUpper() == "RequestHandler".ToUpper() && DateTime.Now.Subtract(proc.StartTime).TotalMinutes > minutes)
                if (proc.ProcessName.ToUpper() == "RequestHandler".ToUpper() )
                {
                    string id = "";
                    try
                    {
                        id = proc.Id.ToString();

                        // tmi [T20140415.0001] [start] check if the request is eligible to be killed
                        bool timedout = (DateTime.Now.Subtract(proc.StartTime).TotalMinutes > minutes);
                        if (timedout) // && requsetIsAnAgent(proc.Id))
                        { // tmi [T20140415.0001][end] 
                            //  [T20150226.0005]  multiple request handler sessions open [Start]
                            if (DateTime.Now.DayOfWeek == DayOfWeek.Friday ||
                                DateTime.Now.DayOfWeek == DayOfWeek.Monday ||
                                DateTime.Now.DayOfWeek == DayOfWeek.Tuesday ||
                                DateTime.Now.DayOfWeek == DayOfWeek.Wednesday ||
                                DateTime.Now.DayOfWeek == DayOfWeek.Thursday
                                )
                            {
                                if (DateTime.Now.Hour > 7 && DateTime.Now.Hour < 19)
                                {

                                    EventLog.WriteEntry("Aria Shutdown Hanged Request Service", "Before Shutdown ProcessID=" + id, EventLogEntryType.Information);
                                    proc.CloseMainWindow();
                                    proc.Kill();
                                    EventLog.WriteEntry("Aria Shutdown Hanged Request Service", "After Shutdown ProcessID=" + id, EventLogEntryType.Information);
                                }
                                //  [T20150226.0005]  multiple request handler sessions open [END]
                               
                            }
                        }
                    }
                    catch (Exception ex)
                    {
                        EventLog.WriteEntry("Aria Shutdown Hanged Request Service", "RequestID=" + id + " Reason:" + ex.Message, EventLogEntryType.Error);
                    }
                }
            }
        }

        /// <summary>
        /// In killing processes, you loop over all processes, if you found one exceeded permitted time then go to its 
        /// record in the ariarequest table, if it is agent then kill it.
        /// This requires an index on the processID field	
        /// </summary>
        /// <param name="p"></param>
        /// <returns></returns>
        private bool requsetIsAnAgent(int p)
        {
            bool isAgent = false;
            for (int i = 0; i < this.Clients.Rows.Count; i++)
            {
                string commandText = "SELECT * FROM AriaRequest WHERE Status = @status " +
                                                                 "and processID = @processid " +
                                                                 "and LTRIM(EventObjectName)<>'' " +
                                                                 "and LTRIM(eventname)=''";
                SqlCommand req = new SqlCommand(commandText);
                req.Parameters.Add(new SqlParameter("@status", "Running"));
                req.Parameters.Add(new SqlParameter("@processid", p));
                string connStr = "Data Source=" + Clients.Rows[i]["CCONSERVER"] + ";" +
                                                      "Initial Catalog=" + Clients.Rows[i]["CCONDBNAME"] + ";" +
                                                      "User id=" + Clients.Rows[i]["CCONUSERID"] + ";" +
                                                      "Password=" + Clients.Rows[i]["CCONPASWRD"] + ";Trusted_Connection=no;";
                req.Connection = new SqlConnection(connStr);
                req.Connection.Open();
                DataTable cl = new DataTable();
                cl.Load(req.ExecuteReader());
                req.Connection.Close();
                isAgent = (cl.Rows.Count > 0);

                // set processID field to 0 if killed 
                if (isAgent)
                {
                    commandText = "UPDATE AriaRequest SET processID = 0 WHERE Status = @status " +
                                                                     "and processID = @processid " +
                                                                     "and LTRIM(EventObjectName)<>'' " +
                                                                     "and LTRIM(eventname)=''";
                    SqlCommand updReq = new SqlCommand(commandText);
                    updReq.Parameters.Add(new SqlParameter("@status", "Running"));
                    updReq.Parameters.Add(new SqlParameter("@processid", p));
                    updReq.Connection = new SqlConnection(connStr);
                    updReq.Connection.Open();
                    updReq.ExecuteNonQuery();
                    updReq.Connection.Close();
                    break;
                }
            }
            return isAgent;
        }
        
        #region commented code
        /// <summary>
        /// gets an integer list of all agent requests running on the current server from all ariarequest tables 
        /// TMI 
        /// </summary>
        /// <returns>integer List</returns>
        /*
        private List<int> getProcessIdList()
        {
            List<int> requestList = new List<int>(); 
            for (int i = 0; i < this.Clients.Rows.Count; i++)
            {
                string commandText = "SELECT processID FROM AriaRequest " +
                    "WHERE Status = 'Running' "+
                    "  and processID <> 0"+
                    "  and LTRIM(EventObjectName)<>'' "+
                    "  and LTRIM(eventname)=''";
                SqlCommand clientsCom = new SqlCommand(commandText);
                string connStr = "Data Source=" + Clients.Rows[i]["CCONSERVER"] + ";" +
                                                      "Initial Catalog=" + Clients.Rows[i]["CCONDBNAME"] + ";" +
                                                      "User id=" + Clients.Rows[i]["CCONUSERID"] + ";" +
                                                      "Password=" + Clients.Rows[i]["CCONPASWRD"] + ";Trusted_Connection=no;";
                clientsCom.Connection = new SqlConnection(connStr);
                clientsCom.Connection.Open();
                DataTable cl = new DataTable();
                cl.Load(clientsCom.ExecuteReader());
                clientsCom.Connection.Close();

                for (int j = 0; j < cl.Rows.Count;j++)
                {
                    int r = (int)cl.Rows[j]["processID"];
                    requestList.Add(r);
                }
            }
            return requestList;
        }
        */
        #endregion
    }
}


