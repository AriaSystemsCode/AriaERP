using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net;
using System.Threading;
using Microsoft.WindowsAzure;
using Microsoft.WindowsAzure.Diagnostics;
using Microsoft.WindowsAzure.ServiceRuntime;
using Aria5SystemAdmin.Module;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
using System.Configuration;

namespace Aria5.DevExpress.SystemAdmin.WorkerRole
{
    public class WorkerRole : RoleEntryPoint
    {
        public override void Run()
        {
            // look how to adjust the time of the serive?
            if (DateTime.Now.TimeOfDay.Hours == 12)
           {
               XpoDefault.ConnectionString = ConfigurationSettings.AppSettings["ConnectionString"].ToString();
               AdjustSchema(ConfigurationSettings.AppSettings["SchemaName"].ToString());
                Session session = XpoDefault.Session;

                Aria5SystemAdmin.Module.Managers.ConfigurationItemManager.CheckEpiration(session);
           }
        }

        public override bool OnStart()
        {
            // Set the maximum number of concurrent connections 
            ServicePointManager.DefaultConnectionLimit = 12;

            // For information on handling configuration changes
            // see the MSDN topic at http://go.microsoft.com/fwlink/?LinkId=166357.


            //CheckEpiration();
            //startRun = true;
            return base.OnStart();
        }

        public static void AdjustSchema(string schemaName)
        {
            # region Change Schema region

            foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
            {
                Type[] types = null;

                try
                {
                    types = assembly.GetTypes();
                }
                catch (Exception)
                {
                }

                if (types != null) foreach (var type in types)
                    {
                        if (type == typeof(XPObjectType) || type.IsSubclassOf(typeof(XPBaseObject)))
                        {
                            var typeInfo = XpoDefault.Session.DataLayer.Dictionary.GetClassInfo(type);

                            if (typeInfo.Table != null)
                            {
                                if (!typeInfo.Table.Name.StartsWith(schemaName + "."))
                                {
                                    typeInfo.Table.Name = schemaName + "." + typeInfo.Table.Name;
                                }

                                foreach (var fk in typeInfo.Table.ForeignKeys)
                                {
                                    if (!fk.PrimaryKeyTable.StartsWith(schemaName + "."))
                                    {
                                        fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable;
                                    }
                                }
                            }
                        }
                    }
            }

            #endregion  Change Schema region

        }

    }
}
