using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using Aria.EnterpriseServices.RequestHandler;
using System.Diagnostics;
using System.Data.SqlClient;
using System.Data;
using Aria.Environment;

namespace _test
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {

            /*Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new Form1());
             */

            // tmi 05/29/2014
            //AriaRequestAgent ag = new AriaRequestAgent();
            //ag.UpdateRequestProcessID("C5060293-FB4A-4A79-8C8D-8DD538A413C8","Aria",Process.GetCurrentProcess().Id);

            // tmi 05/29/2014
            //test x = new test();
            //bool t = x.requsetIsAnAgent(100);

        }


    }
    /*
    class test
    {
        public bool requsetIsAnAgent(int p)
        {
            DataTable Clients = new DataTable(); //tmi        
            //tmi [start] get clients list running request builder service on this pc
            AriaEnviromentVariables EnviromentVariables = new AriaEnviromentVariables();
            SqlCommand clientsCom = new SqlCommand("SELECT * FROM CLIENTS where REQSERVER = @server");
            //clientsCom.Parameters.Add("server", SqlDbType.NChar, 128, "REQSERVER");
            clientsCom.Parameters.Add(new SqlParameter("@server",System.Environment.MachineName));
            clientsCom.Connection = new SqlConnection(EnviromentVariables.Aria50SystemFilesConnectionString);
            clientsCom.Connection.Open();
            Clients.Load(clientsCom.ExecuteReader());
            clientsCom.Connection.Close();
            //tmi [end] get clients list            

            bool isAgent = false;
            for (int i = 0; i < Clients.Rows.Count; i++)
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
                                                                     "and LTRIM(eventname)='' ";
                    SqlCommand updReq = new SqlCommand(commandText);
                    updReq.Parameters.Add(new SqlParameter("@status", "Running"));
                    updReq.Parameters.Add(new SqlParameter("@processid", p));
                    updReq.Connection = new SqlConnection(connStr);
                    updReq.Connection.Open();
                    updReq.ExecuteNonQuery();
                    updReq.Connection.Close();
                }
            }
            return isAgent;
        }
    
    }*/
}
