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
using System.IO;
using System.Windows.Forms;

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
            DataSet Information = new DataSet();
            string ConnectionString = null;
            String CurrentProgram = "";
            string id = "";
            foreach (var proc in Process.GetProcesses())
            {
                int minutes = 15;
                // bool Connected = false;

                if (proc.ProcessName.ToUpper() == "RequestHandler".ToUpper())
                {
                    string path = System.IO.Path.GetDirectoryName(Application.ExecutablePath);
                    EventLog.WriteEntry("Aria Shutdown Service", path, EventLogEntryType.Information);
                    EventLog.WriteEntry("Aria Shutdown Hanged Request Service", "Run1", EventLogEntryType.Information);
                    // MessageBox.Show("a RequestHandler Process Found    " + proc.ProcessName + "   " + proc.Id);
                    if (File.Exists(path+"\\Information.xml"))
                    {
                        Information.ReadXml(path + "\\Information.xml");
                        if (Information.Tables["ConnectionString"] != null)
                        {
                            SqlCommand Qurey = new SqlCommand("Select TOP 1 MethodObjectName from AriaRequest where Processid=" + proc.Id + " ORDER BY StartAfterDate DESC");
                            try
                            {
                                ConnectionString = Information.Tables["ConnectionString"].Rows[0].ItemArray[0].ToString();
                                Qurey.Connection = new SqlConnection(ConnectionString);
                                Qurey.Connection.Open();
                                DataTable QureyResult = new DataTable();
                                QureyResult.Load(Qurey.ExecuteReader());
                                CurrentProgram = QureyResult.Rows[0].ItemArray[0].ToString();
                                Qurey.Connection.Close();
                                //Connected = true;

                                id = proc.Id.ToString();
                                DataTable AppsTable = new DataTable();
                                AppsTable = Information.Tables["AppListTable"];

                                for (int i = 0; i < AppsTable.Rows.Count; i++)
                                {
                                    // MessageBox.Show(i.ToString());
                                    if (AppsTable.Rows[i].ItemArray[0].ToString() != null && AppsTable.Rows[i].ItemArray[1].ToString() != null)
                                    {
                                        String AppName = AppsTable.Rows[i].ItemArray[0].ToString();
                                        String Interval = AppsTable.Rows[i].ItemArray[1].ToString();
                                        // MessageBox.Show("XML " + AppName);
                                        // MessageBox.Show("SQL " + CurrentProgram);
                                        if (AppName == CurrentProgram)
                                        {
                                            int.TryParse(Interval, out minutes);
                                            // MessageBox.Show("Found : time will set as :" + minutes);
                                            break;
                                        }
                                        // else { MessageBox.Show("! Found"); }
                                    }

                                }

                            }
                            catch (Exception ex)
                            {
                                EventLog.WriteEntry("Aria Shutdown Service", "Cannot excute query" + id, EventLogEntryType.Information);
                                // MessageBox.Show("Cannot excute query!!");
                                //Connected = false;
                            }
                        }
                        else
                        {
                            EventLog.WriteEntry("Aria Shutdown Service", "Connection String empty" + id, EventLogEntryType.Information);
                            //MessageBox.Show("Connection String empty");
                        }
                    }
                    else
                    {
                        EventLog.WriteEntry("Aria Shutdown Service", "Information XML not exist" + id, EventLogEntryType.Information);
                        // MessageBox.Show("Information XML not exist!");
                    }


                    bool timedout = (DateTime.Now.Subtract(proc.StartTime).TotalMinutes > minutes);
                    if (timedout)
                    {
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
                        }
                    }
                }
            }


            //tmi [start] get process id list that are Running agents with processID field is not empty
            //List<int> processIdList = getProcessIdList();
            //tmi [end]

            //foreach (var proc in Process.GetProcesses())
            //{
            //    //tmi[start] skip process IDs not in the collected list
            //    //if (!processIdList.Exists(procId => procId == proc.Id)) continue;
            //    //tmi [end]

            //    //tmi , [T20140415.0001] for some processes,it does not define the StartTime and throws and exception
            //    //if (proc.ProcessName.ToUpper() == "RequestHandler".ToUpper() && DateTime.Now.Subtract(proc.StartTime).TotalMinutes > minutes)
            //    if (proc.ProcessName.ToUpper() == "RequestHandler".ToUpper() )
            //    {
            //        string id = "";
            //        try
            //        {
            //            id = proc.Id.ToString();

            //            // tmi [T20140415.0001] [start] check if the request is eligible to be killed
            //            bool timedout = (DateTime.Now.Subtract(proc.StartTime).TotalMinutes > minutes);
            //            if (timedout) // && requsetIsAnAgent(proc.Id))
            //            { // tmi [T20140415.0001][end] 
            //                //  [T20150226.0005]  multiple request handler sessions open [Start]
            //                if (DateTime.Now.DayOfWeek == DayOfWeek.Friday ||
            //                    DateTime.Now.DayOfWeek == DayOfWeek.Monday ||
            //                    DateTime.Now.DayOfWeek == DayOfWeek.Tuesday ||
            //                    DateTime.Now.DayOfWeek == DayOfWeek.Wednesday ||
            //                    DateTime.Now.DayOfWeek == DayOfWeek.Thursday
            //                    )
            //                {
            //                    if (DateTime.Now.Hour > 7 && DateTime.Now.Hour < 19)
            //                    {

            //                        EventLog.WriteEntry("Aria Shutdown Hanged Request Service", "Before Shutdown ProcessID=" + id, EventLogEntryType.Information);
            //                        proc.CloseMainWindow();
            //                        proc.Kill();
            //                        EventLog.WriteEntry("Aria Shutdown Hanged Request Service", "After Shutdown ProcessID=" + id, EventLogEntryType.Information);
            //                    }
            //                    //  [T20150226.0005]  multiple request handler sessions open [END]
                               
            //                }
            //            }
            //        }
            //        catch (Exception ex)
            //        {
            //            EventLog.WriteEntry("Aria Shutdown Hanged Request Service", "RequestID=" + id + " Reason:" + ex.Message, EventLogEntryType.Error);
            //        }
            //    }

            //}
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


