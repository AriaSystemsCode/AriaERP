using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Azure.WebJobs;
using DevExpress.ExpressApp;
using Aria5SystemAdmin.Module.SubAutoTask1;
using System.ServiceModel;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using DevExpress.Xpo.Metadata;
using System.Data.SqlClient;
using Aria5SystemAdmin.Module.Managers;
using System.Data;

namespace Get_DOT_AHT_Data
{
    // To learn more about Microsoft Azure WebJobs SDK, please see http://go.microsoft.com/fwlink/?LinkID=320976
    class Program
    {
        // Please set the following connection strings in app.config for this WebJob to run:
        // AzureWebJobsDashboard and AzureWebJobsStorage
        // const string connectionstr = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin22;User ID=sa;Password=aria_123";
        //MMT
        //const string connectionstr = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
        const string connectionstr = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
        //MMT
       //const string connectionstr = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
    //   const string connectionstr = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin_testing;User ID=sa;Password=aria_123";
 
        static void Main()
         {
            var host = new JobHost();

            DevExpress.Xpo.Session Objectspace = new DevExpress.Xpo.Session();
            Email mail = new Email();
            mail.FromEmail = "distribution@ariasystems.biz";
                mail.EmailPassword = "aria_123";
                mail.ToEmail = "IT@ariasystems.biz,Ahmed.r@ariasystems.biz";
                mail.EmailBody = "Web Job Start Work ";
                mail.EmailTitle = "Get AHT and DOT data Web Job";
                mail.EmailSubject = "Get AHT and DOT data Web Job Start work";
                mail.SendEmail();

             Objectspace.ConnectionString = connectionstr;
            Objectspace.Connect();
            // The following code ensures that the WebJob will be running continuously
            AHTandDOTCalculation AHTDOTobject = new AHTandDOTCalculation();
            AHTDOTobject.CheckresourcesSync(Objectspace);
       AHTDOTobject.getDOTdata(DateTime.Now.Subtract(new TimeSpan(2, 0, 0, 0)), DateTime.Now, Objectspace);
         // AHTDOTobject.getDOTdata(DateTime.Parse("5/1/2017"), DateTime.Parse("5/27/2017"), Objectspace);
           // AHTDOTobject.getAHTdata(DateTime.Parse("3/24/2017"), DateTime.Parse("3/31/2017"), Objectspace);
       AHTDOTobject.getAHTdata(DateTime.Now.Subtract(new TimeSpan(2, 0, 0, 0)), DateTime.Now, Objectspace);
            Objectspace.Disconnect();
            mail.EmailBody = "Get AHT and DOT data Web Job end work";
            mail.SendEmail();
            //host.RunAndBlock();
        }
    }
}
