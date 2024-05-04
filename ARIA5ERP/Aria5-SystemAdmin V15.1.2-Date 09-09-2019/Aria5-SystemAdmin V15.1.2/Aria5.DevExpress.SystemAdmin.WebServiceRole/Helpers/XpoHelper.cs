using System;
using DevExpress.Xpo;
using DevExpress.Xpo.DB;
using DevExpress.Xpo.Metadata;
using Aria5SystemAdmin.Module.BusinessObjects;
using System.Web.Configuration;
using System.Configuration;
using DevExpress.Persistent.BaseImpl;
using Aria5.DevExpress.OneTouchAway.Module.BusinessObjects;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Helpers
{
    public static class XpoHelper
    {
        public static Session GetNewSession()
        {
            return new Session(DataLayer);
        }

        public static UnitOfWork GetNewUnitOfWork()
        {
            return new UnitOfWork(DataLayer);
        }

        private readonly static object lockObject = new object();

        //Volatile means used by diffrent threads.
        static volatile IDataLayer fDataLayer;
        public static IDataLayer DataLayer
        {
            get
            {
                if (fDataLayer == null)
                {
                    lock (lockObject)
                    {
                        if (fDataLayer == null)
                        {
                            fDataLayer = GetDataLayer();
                        }
                    }
                }
                return fDataLayer;
            }
        }

        public static IDataLayer GetDataLayer()
        {
            XpoDefault.Session = null;

            //string conn = MSSqlConnectionProvider.GetConnectionString(WebConfigurationManager.ConnectionStrings["DatabaseServer"].ToString(), WebConfigurationManager.ConnectionStrings["DatabaseName"].ToString(), WebConfigurationManager.ConnectionStrings["DatabaseUserName"].ToString(), WebConfigurationManager.ConnectionStrings["DatabasePassword"].ToString());
            // XpoProvider=MSSqlServer;data source=u4hfi3e5h9.database.windows.net;user id=azuresqladmin;password=aria_123;initial catalog=Aria5SystemAdmin;Persist Security Info=true

            //string conn1 = MSSqlConnectionProvider.GetConnectionString("u4hfi3e5h9.database.windows.net", "azuresqladmin", "aria_123", "Aria5SystemAdmin");
            //string conn = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123;Connect Timeout=1000;";

            // conn = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123;Connect Timeout=1000;";

           // string conn = ConfigurationManager.AppSettings["ProductionConnectionString"].ToString();
#if PRODUCTION
//string conn = ConfigurationManager.AppSettings["ProductionConnectionString"].ToString();
            string conn = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123;Connect Timeout=1000";


#endif

#if STAGING
            //  string conn = ConfigurationManager.AppSettings["StagingConnectionString"].ToString();

            string conn = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123;Connect Timeout=1000;";
#endif

             //string conn = ConfigurationManager.AppSettings["StagingConnectionString"].ToString();
            XPDictionary dict = new ReflectionDictionary();
            IDataStore store = XpoDefault.GetConnectionProvider(conn, AutoCreateOption.DatabaseAndSchema);
            // mahmoud to change
            //dict.GetDataStoreSchema(typeof(Account).Assembly);

            dict.CollectClassInfos(typeof(Account).Assembly);
            dict.CollectClassInfos(typeof(ClientEntity).Assembly);
            dict.CollectClassInfos(typeof(AuditDataItemPersistent).Assembly);
            dict.CollectClassInfos(typeof(AuditedObjectWeakReference).Assembly);
            dict.CollectClassInfos(typeof(XPWeakReference).Assembly);

            IDataLayer dl = new ThreadSafeDataLayer(dict, store);
            return dl;

        }

        public static IDataLayer GetNewDataLayer()
        {
            //string conn = MSSqlConnectionProvider.GetConnectionString(WebConfigurationManager.ConnectionStrings["DatabaseServer"].ToString(), WebConfigurationManager.ConnectionStrings["DatabaseName"].ToString(), WebConfigurationManager.ConnectionStrings["DatabaseUserName"].ToString(), WebConfigurationManager.ConnectionStrings["DatabasePassword"].ToString());
            // XpoProvider=MSSqlServer;data source=u4hfi3e5h9.database.windows.net;user id=azuresqladmin;password=aria_123;initial catalog=Aria5SystemAdmin;Persist Security Info=true

            //string conn1 = MSSqlConnectionProvider.GetConnectionString("u4hfi3e5h9.database.windows.net", "azuresqladmin", "aria_123", "Aria5SystemAdmin");


            // string conn = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123;Connect Timeout=1000;";

            //    string conn = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123;Connect Timeout=1000;";

         //   string conn = ConfigurationManager.AppSettings["ProductionConnectionString"].ToString();
#if PRODUCTION
            // string conn = ConfigurationManager.AppSettings["ProductionConnectionString"].ToString();
            string conn = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123;Connect Timeout=1000";


#endif

#if STAGING
            //  string conn = ConfigurationManager.AppSettings["StagingConnectionString"].ToString();

            string conn = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123;Connect Timeout=1000;";
#endif

            //string conn = ConfigurationManager.AppSettings["StagingConnectionString"].ToString();
            XPDictionary dict = new ReflectionDictionary();
            IDataStore store = XpoDefault.GetConnectionProvider(conn, AutoCreateOption.DatabaseAndSchema);
            // mahmoud to change
            //dict.GetDataStoreSchema(typeof(Account).Assembly);

            dict.CollectClassInfos(typeof(Account).Assembly);

            dict.CollectClassInfos(typeof(ClientEntity).Assembly);
            dict.CollectClassInfos(typeof(AuditDataItemPersistent).Assembly);
            dict.CollectClassInfos(typeof(AuditedObjectWeakReference).Assembly);
            dict.CollectClassInfos(typeof(XPWeakReference).Assembly);

            IDataLayer dl = new ThreadSafeDataLayer(dict, store);
            return dl;

        }
    }
}