using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Azure.WebJobs;
using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module.Managers;
using DevExpress.Xpo;
using Aria5SystemAdmin.Module;
using DevExpress.Data.Filtering;
using System.Data.SqlClient;
using DevExpress.Persistent.BaseImpl;

namespace updateprojectswebjob
{
    // To learn more about Microsoft Azure WebJobs SDK, please see http://go.microsoft.com/fwlink/?LinkID=320976
    class Program
    {
        // Please set the following connection strings in app.config for this WebJob to run:
        // AzureWebJobsDashboard and AzureWebJobsStorage
        //MMT
        //const string connectionstr = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
        const string connectionstr = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
        
        //MMT
        // const string connectionstr = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
        //const string connectionstr = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin22;User ID=sa;Password=aria_123";
   //  const string connectionstr = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin_testing;User ID=sa;Password=aria_123";
        static void Main()
        {
            var host = new JobHost();
            // host.RunAndBlock();
            // The following code ensures that the WebJob will be running continuously
            //  SqlConnection CS = new SqlConnection(connectionstr);
            // SqlCommand CMD = new SqlCommand("Select * from Projecttemplate where [startdate] >= '" + DateTime.Today.Subtract(new TimeSpan(110, 0, 0, 0)) + "'", CS);
            //  CMD.Connection.Open();
            // SqlDataReader dr = CMD.ExecuteReader();
            BaselineNCsManager NcsManager = new BaselineNCsManager();
            string emailto = "ahmed.t@ariany.com";
            string body = "web job start work now";
            NcsManager.SendEmail(body, "Base line Notification", emailto);

            // while (dr.Read())
            //{
            DevExpress.Xpo.Session sess = new DevExpress.Xpo.Session();
            sess.ConnectionString = connectionstr;
            sess.Connect();
            System.Collections.ICollection Projects = sess.GetObjects(sess.Dictionary.GetClassInfo(typeof(ProjectTemplate)), CriteriaOperator.Parse("[StartDate] >= '" + DateTime.Today.Subtract(new TimeSpan(125, 0, 0, 0)) + "' and [Type] = 0"), null, 1000, false, false);
            IList<ProjectTemplate> allprojects = Projects.Cast<ProjectTemplate>().ToList();

            //ProjectTemplate pT = sess.FindObject<ProjectTemplate>(CriteriaOperator.Parse("[Name] = 'Aria Systems INC.-Aria3EDI Maintenance #0'"));
            //if (pT != null)
            //{
            //    foreach (QAProjectEntity pentity in pT.ProjectEntities)
            //    {
            //        if (pentity.MainFeatures.Count == 0)
            //        {
            //            QAMainFeature MF = new QAMainFeature(sess);
            //            MF.Name = "Add new '" + pentity.Name + "' related to Module '"+pentity.Requirement.Title+"'";
            //            MF.ProjectEntity = pentity;
            //            MF.Enddate = pentity.Enddate;
            //            MF.Description = "Ability to add new '" + pentity.Name + "'";
            //            MF.Save();
            //            QAMainFeature EMF = new QAMainFeature(sess);
            //            EMF.Name = "Edit Existing '" + pentity.Name + "' related to Module '" + pentity.Requirement.Title + "'";
            //            EMF.ProjectEntity = pentity;
            //            EMF.Enddate = pentity.Enddate;
            //            EMF.Description = "Ability to Edit Existing '" + pentity.Name + "'";
            //            EMF.Save();
            //            QAMainFeature DMF = new QAMainFeature(sess);
            //            DMF.Name = "Delete Existing '" + pentity.Name + "' related to Module '" + pentity.Requirement.Title + "'";
            //            DMF.ProjectEntity = pentity;
            //            DMF.Enddate = pentity.Enddate;
            //            DMF.Description = "Ability to Delete Existing '" + pentity.Name + "'";
            //            DMF.Save();
            //            QAMainFeature ExMF = new QAMainFeature(sess);
            //            ExMF.Name = "Export Existing '" + pentity.Name + "' related to Module '" + pentity.Requirement.Title + "'";
            //            ExMF.ProjectEntity = pentity;
            //            ExMF.Enddate = pentity.Enddate;
            //            ExMF.Description = "Ability to Export Existing '" + pentity.Name + "' as Excel, PDF or XML";
            //            ExMF.Save();
            //            QAMainFeature PMF = new QAMainFeature(sess);
            //            PMF.Name = "Print Existing '" + pentity.Name + "' related to Module '" + pentity.Requirement.Title + "'";
            //            PMF.ProjectEntity = pentity;
            //            PMF.Enddate = pentity.Enddate;
            //            PMF.Description = "Ability to Print Existing '" + pentity.Name + "'";
            //            PMF.Save();
            //        }
                   

            //       // sess.CommitTransaction();
            //    }
            //}
            // ProjectTemplate Notification_Project = sess.FindObject<ProjectTemplate>(CriteriaOperator.Parse("[oid] = '" + dr[0] + "'"));
            //Objectspace.ConnectionString = connectionstr;
            //Objectspace.Connect(); 
            foreach (ProjectTemplate Notification_Project in allprojects)
            {
                if (Notification_Project != null)
                {
                    if (Notification_Project.ProjectOwner != null)
                    {
                        NcsManager.Checkprojectalarmtimes(Notification_Project, sess);
                        NcsManager.CheckNCssolved(Notification_Project);
                    }
                }
            }

            //host.Call(typeof(Functions).GetMethod("Checkprojectalarmtimes"), new {value = Notification_Project });
            // }
            // CMD.Connection.Close();
            body = "web job End work now";
            NcsManager.SendEmail(body, "Base line notification ", emailto);


        }
    }
}
