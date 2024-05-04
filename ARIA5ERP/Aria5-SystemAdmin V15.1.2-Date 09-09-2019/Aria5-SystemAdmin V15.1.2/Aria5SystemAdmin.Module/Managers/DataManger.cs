using DevExpress.Xpo;
using DevExpress.Xpo.DB;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Configuration;
using DevExpress.Data.Filtering;
using Aria5SystemAdmin.Module.BusinessObjects;
using System.Text.RegularExpressions;
using System.IO;
using System.Net;
using System.Web;

namespace Aria5SystemAdmin.Module.Managers
{
    public class DataManger
    {
        // Sara.N 12/08/2015 [Tracking# + Aria5-DevExpress-DataManager]_Programming,Aria 5 - 1TouchAway - Iteration # 2015-05 [Start]
        # region "Declarations"

        // private static string FTBScriptDirectory = System.Configuration.ConfigurationManager.AppSettings["FTBScriptDirectory"];
        private static string FTBScriptDirectory = @"ftp://waws-prod-ch1-001.ftp.azurewebsites.windows.net/site/wwwroot/images/";

        private static string _userName = @"1TouchAwayStaging\AriaFTP";
        private static string _password = "aria_123";
        string localPath = @"D:\Download\";
        string fileNameStructure = "AZScript.txt";
        string fileNameData = "AZScript.txt";
        # endregion

        public void CreateSchema(string SchemaName)
        {
            Session session = XpoDefault.Session;
            // Hasssan , as per Sara and Mina send from Sara, by mistake to Mahmoud[Begin]
            //XpoDefault.Session.ConnectionString = "Data Source=NSDE_SARHN;Initial Catalog=Azxyz;User ID=sa;Password=aria_123";
            //XpoDefault.Session.ConnectionString = ConfigurationManager.ConnectionStrings[0].ConnectionString;
            // Hasssan , as per Sara and Mina send from Sara, by mistake to Mahmoud[End]

            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                string Query = String.Format("IF (NOT EXISTS (SELECT * FROM sys.schemas WHERE name = '" + SchemaName + "')) BEGIN EXEC ('CREATE SCHEMA [" + SchemaName + "] AUTHORIZATION [dbo]') END", SchemaName, "dbo");
                session.ExecuteNonQuery(Query);
            }
        }

        public void CopySchema(Guid ConfigurationItemDemoAccount, Guid applicationOid, string schema)
        {
            Session session = XpoDefault.Session;
            // Hasssan , as per Sara and Mina send from Sara, by mistake to Mahmoud[Begin]
            //XpoDefault.Session.ConnectionString = "Data Source=NSDE_SARHN;Initial Catalog=Azxyz;User ID=sa;Password=aria_123";
           // XpoDefault.Session.ConnectionString = "Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
            //XpoDefault.Session.ConnectionString = ConfigurationManager.ConnectionStrings[0].ConnectionString;
            // Hasssan , as per Sara and Mina send from Sara, by mistake to Mahmoud[End]
            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                Application_T application = session.FindObject<Application_T>(CriteriaOperator.Parse("Oid='" + applicationOid + "'"));
               // Account div = session.FindObject<Account>(CriteriaOperator.Parse("Oid='" + divOid + "'"));
                Account DemoAccount = session.FindObject<Aria5SystemAdmin.Module.BusinessObjects.Account>(CriteriaOperator.Parse("[Oid] = '" + ConfigurationItemDemoAccount + "'"));

               if (application != null && DemoAccount != null && !string.IsNullOrWhiteSpace(schema))
                {
                    if (!string.IsNullOrWhiteSpace(DemoAccount.Name) && !string.IsNullOrWhiteSpace(application.Name))
                    {
                      //  fileName = div.Name + "_" + application.Name + ".txt";
                        fileNameStructure = DemoAccount.Name + "_" + application.Name + "_Structure" + ".txt";
                        fileNameData = DemoAccount.Name + "_" + application.Name + "_Data" + ".txt";
                    }
                    RunScript(session, ReplaceText(LoadScript(FTBScriptDirectory, _userName, _password, fileNameStructure), schema, fileNameStructure, DemoAccount.DBSchema));
                    RunScript(session, ReplaceText(LoadScript(FTBScriptDirectory, _userName, _password, fileNameData), schema, fileNameData, DemoAccount.DBSchema));
                }
               
            }
        }
        public void DownloadScript(string Directory, string UserName, string password, string fileName)
        {
            // Get the object used to communicate with the server.
            FtpWebRequest requestFileDownload = (FtpWebRequest)WebRequest.Create(Directory + fileName);
            requestFileDownload.Credentials = new NetworkCredential(_userName, _password);
            requestFileDownload.Method = WebRequestMethods.Ftp.DownloadFile;

            FtpWebResponse responseFileDownload = (FtpWebResponse)requestFileDownload.GetResponse();

            Stream responseStream = responseFileDownload.GetResponseStream();
            FileStream writeStream = new FileStream(localPath + fileName, FileMode.Create);

            int Length = 2048;
            Byte[] buffer = new Byte[Length];
            int bytesRead = responseStream.Read(buffer, 0, Length);

            while (bytesRead > 0)
            {
                writeStream.Write(buffer, 0, bytesRead);
                bytesRead = responseStream.Read(buffer, 0, Length);
            }

            responseStream.Close();
            writeStream.Close();

            requestFileDownload = null;
            responseFileDownload = null;

        }

        public string ReplaceText(string scriptContent, string NewSchemaName,string filename,string DemoSchemaName)
        {
            if (!string.IsNullOrWhiteSpace(scriptContent) && !string.IsNullOrWhiteSpace(NewSchemaName))
            {
                if (filename.Contains("Structure"))
                {

                    return scriptContent.Replace(DemoSchemaName, NewSchemaName);
                  
                }
                else if (filename.Contains("Data"))
                {

                  return scriptContent.Replace("CLIENTSCHEMA", NewSchemaName);
                }
                else
                {
                    return "";
                }
            }
            else
            {
                return "";
            }

           
        }

        public void RunScript(Session session, string scriptText)
        {
            // Create an regex expression to split the script by the word GO.
            Regex regex = new Regex(@"\r{0,1}\nGO\r{0,1}\n");
            string[] commands = regex.Split(scriptText);



            // Iterate over all sections.
            for (int i = 0; i < commands.Length; i++)
            {
                // If the command is not empty.
                if (!string.IsNullOrWhiteSpace(commands[i]))
                {
                    // Execute the script part.
                    try
                    {
                        session.ExecuteNonQuery(commands[i]);
                    }
                    catch (Exception)
                    {
  
                    }
                   
                }
            }
        }
        // Sara.N 12/08/2015 [Tracking# + Aria5-DevExpress-DataManager]_Programming,Aria 5 - 1TouchAway - Iteration # 2015-05 [END]
        public string LoadScript(string Directory, string UserName, string password, string fileName)
        {
            //CREATE AN FTP REQUEST WITH THE DOMAIN AND CREDENTIALS
            System.Net.FtpWebRequest tmpReq = (System.Net.FtpWebRequest)System.Net.FtpWebRequest.Create(Directory + fileName);
            tmpReq.Credentials = new NetworkCredential(_userName, _password);

            //GET THE FTP RESPONSE
            using (System.Net.WebResponse tmpRes = tmpReq.GetResponse())
            {
                //GET THE STREAM TO READ THE RESPONSE FROM
                using (System.IO.Stream tmpStream = tmpRes.GetResponseStream())
                {
                    //CREATE A TXT READER (COULD BE BINARY OR ANY OTHER TYPE YOU NEED)
                    using (System.IO.TextReader tmpReader = new System.IO.StreamReader(tmpStream))
                    {
                        //STORE THE FILE CONTENTS INTO A STRING
                        string fileContents = tmpReader.ReadToEnd();
                        return fileContents;
                        //DO SOMETHING WITH SAID FILE CONTENTS
                    }
                }
            }
        }
    }
}

