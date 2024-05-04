using Aria5.DevExpress.MainSystem.Module.BusinessObjects;
using Aria5.DevExpress.OneTouchAway.Module.BusinessObjects;
using Aria5SystemAdmin.Module;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Security;
using DevExpress.ExpressApp.Security.Strategy;
using DevExpress.ExpressApp.Xpo;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Xpo.DB;
using DevExpress.Xpo.Metadata;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Configuration;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Helpers
{
    public class SchemaHelper
    {
        public static List<XPClassInfo> SetCleintSchema(Session session, string schemaName)
        {
            XPDictionary xpDictionary = session.Dictionary;

            var result = new List<XPClassInfo>();

            List<string> clientTables = new List<string>();
            foreach (var classes in xpDictionary.Classes)
            {


                dynamic typeInfo = classes;

                result.Add(xpDictionary.QueryClassInfo(typeInfo.ClassType));

                var table1 = typeInfo.Table;

                foreach (Attribute att in typeInfo.Attributes)
                {
                    // Sara.N,1 Clients Schema not All tables Created Just Client prefix tables created only [Start]
                    // if (att is IsClient && ((IsClient)att).ClientTable)
                    if (att is IsClient)
                    // Sara.N,1 Clients Schema not All tables Created Just Client prefix tables created only [End]
                    {
                        clientTables.Add(table1.Name);
                    }
                }
                // Sara.N, Remove Audit system tables from Clients tables created 22-05-2016 [Start]
                //if (typeInfo.ClassType == typeof(AuditDataItemPersistent) ||
                //    typeInfo.ClassType == typeof(AuditedObjectWeakReference) ||
                //    typeInfo.ClassType == typeof(XPWeakReference))
                //{
                //    clientTables.Add(table1.Name);
                //}
                // Sara.N, Remove Audit system tables from Clients tables created 22-05-2016 [End]
            }

            foreach (var classes in xpDictionary.Classes)
            {
                dynamic typeInfo = classes;

                bool isClient = false;
                foreach (Attribute att in typeInfo.Attributes)
                {
                    // Sara.N,1 Clients Schema not All tables Created Just Client prefix tables created only [Start]
                    // if (att is IsClient && ((IsClient)att).ClientTable)
                    if (att is IsClient)
                    {
                        isClient = true;
                    }
                    // Sara.N,1 Clients Schema not All tables Created Just Client prefix tables created only [End]

                }
                // Sara.N, Remove Audit system tables from Clients tables created 22-05-2016 [Start]
                //if (typeInfo.ClassType == typeof(AuditDataItemPersistent) ||
                //    typeInfo.ClassType == typeof(AuditedObjectWeakReference) ||
                //    typeInfo.ClassType == typeof(XPWeakReference))
                //{
                //    isClient = true;
                //}
                // Sara.N, Remove Audit system tables from Clients tables created 22-05-2016 [End]
                if (isClient)
                {
                    if (typeInfo.Table != null)
                    {
                        if (!typeInfo.Table.Name.StartsWith(schemaName + "."))
                        {
                            if (typeInfo.Table.Name.Contains("."))
                            {
                                typeInfo.Table.Name = schemaName + "." + typeInfo.Table.Name.Split('.')[1];
                            }
                            else
                            {
                                typeInfo.Table.Name = schemaName + "." + typeInfo.Table.Name;
                            }
                        }

                        foreach (var fk in typeInfo.Table.ForeignKeys)
                        {
                            if (!string.IsNullOrEmpty(fk.PrimaryKeyTable) && clientTables.Contains(fk.PrimaryKeyTable))
                            {
                                if (!fk.PrimaryKeyTable.StartsWith(schemaName + "."))
                                {
                                    if (fk.PrimaryKeyTable.Split('.').Length > 1)
                                    {
                                        fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable.Split('.')[1];
                                    }
                                    else
                                    {
                                        fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable.Split('.')[0];
                                    }
                                }
                            }
                        }
                    }
                }
            }

            return result;
        }

        public static void SwitchSchema()
        {
            XpoDefault.Session = XpoHelper.GetNewSession();
        }
    }
}