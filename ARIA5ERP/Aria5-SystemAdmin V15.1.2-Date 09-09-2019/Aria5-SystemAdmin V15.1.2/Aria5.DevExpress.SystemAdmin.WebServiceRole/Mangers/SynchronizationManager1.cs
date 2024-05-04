//using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
//using DevExpress.Data.Filtering;
//using DevExpress.Xpo;
//using DevExpress.Xpo.DB;
//using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.Web;
//using DotNetRuleEngine.Core;
//using DevExpress.Xpo.Metadata;
////using Microsoft.Azure.NotificationHubs;
//using DevExpress.ExpressApp;
//using DevExpress.ExpressApp.Xpo;
//using Aria5SystemAdmin.Module.BusinessObjects;
//using System.Reflection;
//using System.Threading.Tasks;
//using DevExpress.Persistent.AuditTrail;
//using DevExpress.Persistent.BaseImpl;
//using Aria5SystemAdmin.Module;
//using System.Data.SqlClient;
//using System.IO;
//using System.Drawing;
//using Aria5.DevExpress.SystemAdmin.WebServiceRole.Helpers;
//using Aria5.DevExpress.OneTouchAway.Module.BusinessObjects;
//using Aria5SystemAdmin.Module.Managers;
//using System.Xml.Serialization;

//namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Mangers
//{

//    // Sara.N,1 Synchronization Task [Start]
//    public class SynchronizationManager1
//    {
//        public static Object thisLock = new Object();
//        public List<SyncDataObject> PullData(String fromVersion, String toVersion, Guid accountOid)
//        {
//            // should return List<SyncDataObject>
//            SchemaHelper.SetCleintSchema("dbo");
//            Account _Account = XpoDefault.Session.FindObject<Account>(CriteriaOperator.Parse("[Oid] = '" + accountOid.ToString() + "'"));
//            List<XPClassInfo> typesInfo = SchemaHelper.SetCleintSchema(_Account.DBSchema);

//            string AccountSchema = "";
//            if (_Account != null)
//            {
//                if (!string.IsNullOrWhiteSpace(_Account.DBSchema))
//                {
//                    AccountSchema = _Account.DBSchema;
//                }
//                else
//                {
//                    AccountSchema = _Account.AccountCode;
//                }
//            }

//            //AccountSchema = "ARASST03_Demo";
//            //              // String stringquery = string.Format("select * from {0}.syncview where [timestamp] > CONVERT(varbinary(max),{1}, 1) and [timestamp]< CONVERT(varbinary(max),{2}, 1)", AccountSchema,fromVersion,toVersion);
//            //              string stringquery2 = string.Format(@"SELECT  [{0}].AuditDataItemPersistent.AuditedObject, [{0}].AuditedObjectWeakReference.GuidId, [{0}].AuditDataItemPersistent.PropertyName, [{0}].AuditDataItemPersistent.NewValue, 
//            //                                                  [{0}].AuditDataItemPersistent.timestamp, [{0}].AuditDataItemPersistent.UserName, [{0}].AuditedObjectWeakReference.Oid,dbo.XPObjectType.TypeName, [{0}].AuditDataItemPersistent.OperationType,[{0}].AuditDataItemPersistent.ModifiedOn
//            //                                                  FROM [{0}].XPWeakReference INNER JOIN
//            //                                                  dbo.XPObjectType ON [{0}].XPWeakReference.TargetType = dbo.XPObjectType.OID INNER JOIN
//            //                                                  [{0}].AuditDataItemPersistent INNER JOIN
//            //                                                  [{0}].AuditedObjectWeakReference ON [{0}].AuditDataItemPersistent.AuditedObject = [{0}].AuditedObjectWeakReference.Oid ON [{0}].XPWeakReference.Oid = [{0}].AuditedObjectWeakReference.Oid
//            //                                                  where  ([{0}].AuditedObjectWeakReference.GuidId IS NOT NULL)
//            //                                                  GROUP BY [{0}].AuditDataItemPersistent.AuditedObject, [{0}].AuditedObjectWeakReference.GuidId, [{0}].AuditDataItemPersistent.PropertyName, [{0}].AuditDataItemPersistent.NewValue,[{0}].AuditDataItemPersistent.timestamp, 
//            //                                                  [{0}].AuditDataItemPersistent.ModifiedOn, [{0}].AuditDataItemPersistent.UserName, [{0}].AuditedObjectWeakReference.Oid, dbo.XPObjectType.TypeName,[{0}].AuditDataItemPersistent.OperationType
//            //                                                  ORDER BY [{0}].AuditDataItemPersistent.AuditedObject, [{0}].AuditDataItemPersistent.PropertyName, [{0}].AuditDataItemPersistent.timestamp, [{0}].AuditDataItemPersistent.ModifiedOn DESC", AccountSchema);
            
//            string stringQuery = string.Format("SELECT  " +
//                                  "[{0}].AuditDataItemPersistent.AuditedObject, " +
//                                  "[{0}].AuditedObjectWeakReference.GuidId, " +
//                                  "[{0}].AuditDataItemPersistent.PropertyName, " +
//                                  "[{0}].AuditDataItemPersistent.NewValue, " +
//                                    "[{0}].AuditDataItemPersistent.timestamp, " +
//                                    "[{0}].AuditDataItemPersistent.UserName, " +
//                                    "[{0}].AuditedObjectWeakReference.Oid, " +
//                                    "dbo.XPObjectType.TypeName, " +
//                                    "[{0}].AuditDataItemPersistent.OperationType, " +
//                                    "CASE " +
//                                    "WHEN [{0}].AuditDataItemPersistent.OperationType = 'ObjectCreated' THEN 1 " +
//                                    "WHEN [{0}].AuditDataItemPersistent.OperationType = 'ObjectChanged' THEN 2 " +
//                                    "WHEN [{0}].AuditDataItemPersistent.OperationType = 'ObjectDeleted' THEN 3 " +
//                                    "WHEN [{0}].AuditDataItemPersistent.OperationType = 'InitialValueAssigned' THEN 4 " +
//                                    "ELSE 5 " +
//                                    "END AS OperationTypeOrder, " +
//                                    "CASE " +
//                                    "WHEN [{0}].AuditDataItemPersistent.OperationType = 'ObjectCreated' THEN 'ADD' " +
//                                    "WHEN [{0}].AuditDataItemPersistent.OperationType = 'ObjectChanged' THEN 'EDIT' " +
//                                    "WHEN [{0}].AuditDataItemPersistent.OperationType = 'ObjectDeleted' THEN 'DELETE' " +
//                                    "WHEN [{0}].AuditDataItemPersistent.OperationType = 'InitialValueAssigned' THEN 'ASSIGN' " +
//                                    "ELSE 'OTHER' " +
//                                    "END AS OperationName, " +
//                                    "CONVERT(VARCHAR(50),[{0}].AuditDataItemPersistent.ModifiedOn,9) AS ModifiedOn, " +
//                                    "[{0}].XPWeakReference.TargetKey, " +
//                                    "XPObjectTypeNewObject.TypeName AS TypeNameNewObject," +
//                                    "XPWeakReferenceNewObject.TargetKey AS TargetKeyNewObject " +
//            "FROM [{0}].AuditDataItemPersistent " +
//                                        "LEFT OUTER JOIN " +
//                                        "[{0}].AuditedObjectWeakReference " +
//                                        "ON [{0}].AuditDataItemPersistent.AuditedObject = [{0}].AuditedObjectWeakReference.Oid " +
//                                        "LEFT OUTER JOIN " +
//                                    "[{0}].XPWeakReference " +
//                                "ON [{0}].XPWeakReference.Oid = [{0}].AuditDataItemPersistent.AuditedObject " +
//                                        "LEFT OUTER JOIN " +
//                                    "dbo.XPObjectType " +
//                                        "ON [{0}].XPWeakReference.TargetType = dbo.XPObjectType.OID " +
//                                        "LEFT OUTER JOIN " +
//                                    "[{0}].XPWeakReference AS XPWeakReferenceNewObject " + 
//                                    "ON XPWeakReferenceNewObject.Oid = [{0}].AuditDataItemPersistent.NewObject " +
//                                        "LEFT OUTER JOIN " + 
//                                    "dbo.XPObjectType AS XPObjectTypeNewObject " + 
//                                    "ON XPWeakReferenceNewObject.TargetType = XPObjectTypeNewObject.OID " + 
//                                        "WHERE " +
//                                 "timestamp >= CONVERT(varbinary(max),{1}, 1) AND timestamp <= CONVERT(varbinary(max),{2}, 1) " +
//                                 //  "timestamp >= {1} AND timestamp <= {2} " +
//                                    "ORDER BY " +
//                                    "[{0}].AuditDataItemPersistent.AuditedObject, " +
//                                    "OperationTypeOrder, " +
//                                        "[{0}].AuditDataItemPersistent.PropertyName, " +
//                                    "[{0}].AuditDataItemPersistent.timestamp, " +
//                                    "[{0}].AuditDataItemPersistent.ModifiedOn DESC", AccountSchema, fromVersion, toVersion);

//            List<SyncDataObject> syncList = new List<SyncDataObject>();

//            SelectedData data = XpoDefault.Session.ExecuteQuery(stringQuery);

//            if (data.ResultSet != null)
//            {
//                for (int index = 0; index < data.ResultSet[0].Rows.Count(); index++)
//                {
//                    var row = data.ResultSet[0].Rows[index];

//                    if (row.Values[10].ToString() == "ADD")
//                    {
//                        SyncDataObject newItem = new SyncDataObject();
//                        newItem.TableName = MapTableName(row.Values[7].ToString());
//                        newItem.RecordStatus = row.Values[10].ToString();
//                        newItem.EntityOID = Guid.Parse(row.Values[1].ToString());

//                        string s2 = BitConverter.ToString((byte[])(row.Values[4]));
//                        String[] tempAry = s2.Split('-');
//                        string st = "0x";
//                        foreach (string str in tempAry) st += str;
//                        newItem.Version = st;

//                        newItem.Properties = new List<string>();
//                        newItem.Values = new List<object>();

//                        index++;
//                        if (index >= data.ResultSet[0].Rows.Count()) break;

//                        row = data.ResultSet[0].Rows[index];

//                        while (row.Values[10].ToString() == "ASSIGN")
//                        {
//                            newItem.Properties.Add(row.Values[2].ToString());
//                            if (row.Values[13] == null)
//                            {
//                                newItem.Values.Add(row.Values[3]);
//                            }
//                            else
//                            {
//                                newItem.Values.Add(row.Values[14]);
//                            }

//                            index++;
//                            if (index >= data.ResultSet[0].Rows.Count()) break;

//                            row = data.ResultSet[0].Rows[index];
//                        }

//                        syncList.Add(newItem);
//                    }

//                    if (index >= data.ResultSet[0].Rows.Count()) break;
//                    if (row.Values[10].ToString() == "EDIT" || 
//                        (row.Values[10].ToString() == "ASSIGN" && index == 0))
//                    {
//                        SyncDataObject editItem = new SyncDataObject();
//                        editItem.TableName = MapTableName(row.Values[7].ToString());
//                        editItem.RecordStatus = row.Values[10].ToString();
//                        editItem.EntityOID = Guid.Parse(row.Values[1].ToString());

//                        string s2 = BitConverter.ToString((byte[])(row.Values[4]));
//                        String[] tempAry = s2.Split('-');
//                        string st = "0x";
//                        foreach (string str in tempAry) st += str;
//                        editItem.Version = st;

//                        editItem.Properties = new List<string>();
//                        editItem.Values = new List<object>();

//                        var lastStatus = row.Values[10].ToString();
//                        var lastAuditedObject = row.Values[0].ToString();
//                        var lastModifyOn = row.Values[11].ToString();

//                        while (lastStatus == row.Values[10].ToString() &&
//                               lastAuditedObject == row.Values[0].ToString() &&
//                               lastModifyOn == row.Values[11].ToString())
//                        {
//                            editItem.Properties.Add(row.Values[2].ToString());
//                            if (row.Values[13] == null)
//                            {
//                                editItem.Values.Add(row.Values[3]);
//                            }
//                            else
//                            {
//                                editItem.Values.Add(row.Values[14]);
//                            }

//                            index++;
//                            if (index >= data.ResultSet[0].Rows.Count()) break;

//                            row = data.ResultSet[0].Rows[index];
//                        }

//                        syncList.Add(editItem);
//                    }
//                    if (index >= data.ResultSet[0].Rows.Count()) break;

//                    if (row.Values[10].ToString() == "DELETE")
//                    {
//                        SyncDataObject deleteItem = new SyncDataObject();
//                        deleteItem.TableName = MapTableName(row.Values[7].ToString());
//                        deleteItem.RecordStatus = row.Values[10].ToString();
//                        deleteItem.EntityOID = Guid.Parse(row.Values[1].ToString());

//                        string s2 = BitConverter.ToString((byte[])(row.Values[4]));
//                        String[] tempAry = s2.Split('-');
//                        string st = "0x";
//                        foreach (string str in tempAry) st += str;
//                        deleteItem.Version = st;


//                        deleteItem.Properties = new List<string>();
//                        deleteItem.Values = new List<object>();

//                        syncList.Add(deleteItem);
//                    }
//                }
//            }




//            //// SelectedData data = session.ExecuteSproc("GetModifiedDataAfterVersion", new OperandValue(fromVersion), new OperandValue(toVersion),new OperandValue(AccountSchema));
//            //    SelectedData data = XpoDefault.Session.ExecuteQuery(stringquery2);
//            //    List<SyncDataObject> mod = new List<SyncDataObject>();
//            //    if (data.ResultSet != null)
//            //    {
//            //        foreach (SelectStatementResultRow row in data.ResultSet[0].Rows)
//            //        {
//            //            SyncDataObject obj = mod.FirstOrDefault(r => r.EntityOID == Guid.Parse(row.Values[1].ToString()));
//            //            if (obj != null)
//            //            {
//            //                obj.TableName =   MapTableName(row.Values[7].ToString());// row.Values[7].ToString();
//            //                obj.RecordStatus = MapRecordStatus(row.Values[8].ToString()); //row.Values[8].ToString();// MapRecordStatus(row.Values[8].ToString());   
//            //                //   obj.Version = row.Values[4].ToString();
//            //                string p = obj.Properties.FirstOrDefault(r => r.Contains(row.Values[2].ToString()));
//            //                if (string.IsNullOrWhiteSpace(p))
//            //                {

//            //                    obj.Properties.Add(row.Values[2].ToString());
//            //                    obj.Values.Add(row.Values[3]);
//            //                }
//            //            }
//            //            else
//            //            {
//            //                SyncDataObject modifiedObject = new SyncDataObject(); 
//            //                modifiedObject.Properties = new List<string>();
//            //                modifiedObject.Values = new List<object>();
//            //                modifiedObject.EntityOID = Guid.Parse(row.Values[1].ToString());
//            //                modifiedObject.TableName = MapTableName(row.Values[7].ToString());// row.Values[7].ToString();// MapTableName(row.Values[7].ToString());
//            //                modifiedObject.RecordStatus = MapRecordStatus(row.Values[8].ToString());  //row.Values[8].ToString();// MapRecordStatus(row.Values[8].ToString()); 

//            //                // Convert time Stamp to String
//            //                string s2 = BitConverter.ToString((byte[])(row.Values[4]));
//            //                String[] tempAry = s2.Split('-');
//            //                string st = "0x";
//            //                foreach (string str in tempAry) st += str;
//            //                modifiedObject.Version = st;

//            //                string p = modifiedObject.Properties.FirstOrDefault(r => r.Contains(row.Values[2].ToString()));
//            //                if (string.IsNullOrWhiteSpace(p))
//            //                {
//            //                    if (row.Values[2].ToString() !="")
//            //                    {
//            //                        modifiedObject.Properties.Add(row.Values[2].ToString());
//            //                        modifiedObject.Values.Add(row.Values[3]);
//            //                        mod.Add(modifiedObject);
//            //                    }


//            //                }

//            //            }
//            //        }
//            //        return mod;
//            //    }
//            //    else
//            //    {
//            //        return mod;
//            //    }

//            return syncList;
//        }

//        public string MapRecordStatus(string AuditOperationType)
//        {
//            string ret = "";
//            if (!string.IsNullOrWhiteSpace(AuditOperationType))
//            {
//                switch (AuditOperationType)
//                {
//                    case "ObjectCreated": ret = "ADD"; break;
//                    case "InitialValueAssigned": ret = "ADD"; break;
//                    case "ObjectChanged": ret = "EDIT"; break;
//                    case "ObjectDeleted": ret = "DELETE"; break;

//                    //   case "AddedToCollection": ret = "ADD"; break;
//                    //   case "RemovedFromCollection": ret = "DELETE"; break;
//                    //   case "CollectionObjectChanged": ret = "EDIT"; break;
//                    //   case "AggregatedObjectChanged": ret = "EDIT"; break;
//                    //   case "CustomData": ret = "EDIT"; break;
//                    default: ret = "ADD";
//                        break;
//                }
//                return ret;
//            }
//            else
//            {
//                return "";
//            }
//        }
//        public string MapTableName(string returnedTableName)
//        {
//            string Oldtablename = returnedTableName.Substring(returnedTableName.LastIndexOf(".") + 1);
//            if (Oldtablename.Contains("Client") || Oldtablename.Contains("CLIENT"))
//            {
//                Oldtablename = Oldtablename.Replace("Client", "");
//            }

//            return Oldtablename;
//        }
//        public List<string> PushData(Guid accountOid, List<SyncDataObject> dataToUpdate, String deviceSeginture)//,List<SyncDataObject> dataToUpdate)
//        {
//            SchemaHelper.SetCleintSchema("dbo");


//            Account _Account = XpoDefault.Session.FindObject<Account>(CriteriaOperator.Parse("[Oid] = '" + accountOid.ToString() + "'"));


//            string AccountSchema = "";
//            if (_Account != null)
//            {
//                if (!string.IsNullOrWhiteSpace(_Account.DBSchema))
//                {
//                    AccountSchema = _Account.DBSchema;
//                }
//                else
//                {
//                    AccountSchema = _Account.AccountCode;
//                }
//            }
//            //string AccountSchema = "TestingSchema";
//            List<XPClassInfo> typesInfo = SchemaHelper.SetCleintSchema(AccountSchema);
//            // XPClassInfo[] typesInfo = SchemaHelper.SetCleintSchema(AccountSchema);

//            List<string> returnedlist = new List<string>();
//            string returnedstring = "";


//            foreach (var singleModifiedObject in dataToUpdate)
//            {
//                ClientEntity.CallFromSync = true;

//                string ModificationType = singleModifiedObject.RecordStatus.Trim().ToUpper();
//                string className = singleModifiedObject.TableName.TrimEnd();
//                if (className == "ENTITY")
//                {
//                    //className = singleModifiedObject.GetType().GetProperty("TypeId").GetValue(singleModifiedObject).ToString();
//                    for (int i = 0; i < singleModifiedObject.Properties.Count; i++)
//                    {
//                        if (singleModifiedObject.Properties[i].ToString().ToUpper().TrimEnd() == "TYPEID")
//                        {
//                            className = singleModifiedObject.Values[i].ToString();
//                            break;
//                        }
//                    }
//                }

//                var typeInfo = typesInfo.Where(r => r.FindAttributeInfo(typeof(IsClient)) != null && ((IsClient)r.GetAttributeInfo(typeof(IsClient))).ClientTable)
//                                        .First(r => (((IsClient)r.GetAttributeInfo(typeof(IsClient))).NeedPrefix && r.TableName == "Client" + className) ||
//                                                     (!((IsClient)r.GetAttributeInfo(typeof(IsClient))).NeedPrefix && r.TableName == className) ||
//                                                     (r.BaseClass.TableName == "ClientEntity" && r.ClassType.Name == className));
//                switch (ModificationType)
//                {
//                    case "ADD":
//                        {
//                            ClientBaseObject newObject = (ClientBaseObject)Activator.CreateInstance(typeInfo.ClassType, XpoDefault.Session);
//                            newObject = SetProperties(singleModifiedObject, newObject, singleModifiedObject.EntityOID.ToString());
//                            newObject.Save();
//                         //   AuditTrailService.Instance.SaveAuditData(XpoDefault.Session);
//                            returnedlist.Add(MapTableName(typeInfo.TableName) + "|" + singleModifiedObject.EntityOID.ToString() + "|" + newObject.ClientDataAuditOid.ToString());
//                            break;
//                        }
//                    case "EDIT":
//                        {
//                            ClientBaseObject existOjectobj = (ClientBaseObject)XpoDefault.Session.FindObject(typeInfo.ClassType, CriteriaOperator.Parse("[Oid] ='" + singleModifiedObject.EntityOID + "'"));
//                            if (existOjectobj == null)
//                            {
//                                goto case "ADD";
//                            }
//                            else
//                            {
//                                existOjectobj = SetProperties(singleModifiedObject, existOjectobj);
//                                existOjectobj.Save();
//                            //    AuditTrailService.Instance.SaveAuditData(XpoDefault.Session);
//                                returnedlist.Add(MapTableName(typeInfo.TableName) + "|" + singleModifiedObject.EntityOID.ToString() + "|" + existOjectobj.ClientDataAuditOid.ToString());
//                            }
//                            break;
//                        }
//                    case "DELETE":
//                        {
//                            #region Delete
//                            {
//                                // var existOjectobj = (XPBaseObject)XpoDefault.Session.FindObject(typeInfo.ClassType, CriteriaOperator.Parse("[Oid] ='" + singleModifiedObject.EntityOID + "'"));
//                                ClientBaseObject existOjectobj = (ClientBaseObject)XpoDefault.Session.FindObject(typeInfo.ClassType, CriteriaOperator.Parse("[Oid] ='" + singleModifiedObject.EntityOID + "'"));
//                                if (existOjectobj != null)
//                                {
//                                    existOjectobj.Delete();
//                                 //   AuditTrailService.Instance.SaveAuditData(XpoDefault.Session);
//                                    returnedlist.Add(existOjectobj.ClientDataAuditOid.ToString());
//                                    break;
//                                }
//                                else
//                                {
//                                    returnedstring = typeInfo.TableName + "|" + singleModifiedObject.EntityOID.ToString() + "|" + singleModifiedObject.Version.ToString();
//                                    break;
//                                }
//                            }
//                            #endregion Delete
//                        }
//                }
//                // Mahmoud 
//             //   AuditTrailService.Instance.SaveAuditData(XpoDefault.Session);
//                // Mahmoud 
//            }
//            ClientEntity.CallFromSync = false;
//            //Derby
//            //Return returnedstring
//            //derby
//            return returnedlist;
//        }
//        public string GetDBVersion(Guid accountOid)
//        {
//            //  session.ConnectionString = @"Data Source=NSDE_SARHN;Initial Catalog=Azure_Aria5SystemAdmin_Staging;User ID=sa;Password=aria_123";
//            //if (session.IsConnected != true) { session.Connect(); }
//            //if (session.IsConnected == true)

//            SchemaHelper.SetCleintSchema("dbo");

//            Account _Account = XpoDefault.Session.FindObject<Account>(CriteriaOperator.Parse("[Oid] = '" + accountOid.ToString() + "'"));
//            List<XPClassInfo> typesInfo = SchemaHelper.SetCleintSchema(_Account.DBSchema);
//            string AccountSchema = "";
//            if (_Account != null)
//            {
//                if (!string.IsNullOrWhiteSpace(_Account.DBSchema))
//                {
//                    AccountSchema = _Account.DBSchema;
//                }
//                else
//                {
//                    AccountSchema = _Account.AccountCode;
//                }
//            }
//            String stringquery = string.Format("select  MAX([{0}].[AuditDataItemPersistent].[timestamp]) from  {0}.AuditDataItemPersistent", AccountSchema);
//            SelectedData data = XpoDefault.Session.ExecuteQuery(stringquery);
//            if (data.ResultSet != null)
//            {
//                foreach (SelectStatementResultRow row in data.ResultSet[0].Rows)
//                {
//                    string s2 = BitConverter.ToString((byte[])(row.Values[0]));
//                    String[] tempAry = s2.Split('-');
//                    string st = "0x";
//                    foreach (string str in tempAry) st += str;
//                    return st;
//                }
//                return "";
//            }
//            else
//            {
//                return "";
//            }
//        }
//        public string GetMinFrontEndVersion()
//        {
//            //Min FrontEnd Version
//            string ret = "";
//            SchemaHelper.SetCleintSchema("dbo");

//            //session.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
//            //if (session.IsConnected != true) { session.Connect(); }
//            //if (session.IsConnected == true)
//            //{
//            Application_T App1 = (Application_T)XpoDefault.Session.FindObject(typeof(Application_T), CriteriaOperator.Parse("[Id] ='Aria5-Windows8Xaml-1TouchAwayFrontend'"));
//            if (App1 != null)
//            {
//                try
//                {
//                    ret = ret + App1.ApplicationSettings.FirstOrDefault(r => r.SettingID == "MINFRONTENDVERSION").SettingValue.ToString();

//                }
//                catch (Exception)
//                {

//                    ret += "|";
//                }
//            }
//            else
//            {
//            }

//            Application_T App2 = (Application_T)XpoDefault.Session.FindObject(typeof(Application_T), CriteriaOperator.Parse("[Id] ='Aria5-Windows8Xaml-MainSystem'"));
//            if (App2 != null)
//            {
//                try
//                {

//                    ret = ret + '|' + App2.ApplicationSettings.FirstOrDefault(r => r.SettingID == "MINFRONTENDVERSION").SettingValue.ToString();

//                }
//                catch (Exception)
//                {

//                    ret += "|";
//                }

//            }
//            else
//            {
//                ret += "|";
//            }
//            Application_T App3 = (Application_T)XpoDefault.Session.FindObject(typeof(Application_T), CriteriaOperator.Parse("[Id] ='Aria5-DevExpress-SystemAdmin'"));
//            if (App3 != null)
//            {
//                try
//                {

//                    ret = ret + '|' + App3.ApplicationSettings.FirstOrDefault(r => r.SettingID == "MINFRONTENDVERSION").SettingValue.ToString();

//                }
//                catch (Exception)
//                {

//                    ret += "|";
//                }
//            }
//            else
//            {
//                ret += "|";
//            }

//            //if (string.IsNullOrEmpty(ret))
//            //{
//            //    ret = ret.Substring(1);
//            //}
//            // string[] x = ret.Split('|');
//            return ret;
//        }
//        public string GetPropertyName(string FullPropertyName)
//        {
//            string[] FullPropertyNameArray = FullPropertyName.Split('.');
//            string TableName = FullPropertyNameArray[0];
//            string PropertyName = FullPropertyNameArray[1];
//            return PropertyName;
//        }
//        public ClientBaseObject SetProperties(SyncDataObject singleModifiedObject, ClientBaseObject existOjectobj, string Poid = "")
//        {
//            try
//            {
//                if (string.IsNullOrEmpty(Poid) == false)
//                {
//                    existOjectobj.SetMemberValue("oid", Guid.Parse(Poid.TrimEnd()));
//                }
//            }
//            catch (Exception ex)
//            { }

//            try
//            {
//                for (int i = 0; i < singleModifiedObject.Properties.Count; i++)
//                {
//                    if (singleModifiedObject.Values[i] != null)
//                    {
//                        string propertyName = GetPropertyName(singleModifiedObject.TableName.ToString().TrimEnd().ToUpper() + "." + singleModifiedObject.Properties[i]);
//                        if (propertyName.ToUpper().TrimEnd() != "TRANSACTIONTIMESTAMP" && propertyName.ToUpper().TrimEnd() != "" && existOjectobj.GetType().GetProperty(propertyName) != null)
//                        {
//                            //   if (existOjectobj.GetType().GetProperty(propertyName).PropertyType.ToString().ToUpper().IndexOf("Aria5SystemAdmin.Module".ToUpper()) > -1)
//                            //if (existOjectobj.GetType().GetProperty(propertyName).PropertyType != typeof(Guid) && singleModifiedObject.Values[i] is Guid)
//                            // Mahmoud Add the code to handle Guid parameter Ref: http://stackoverflow.com/questions/16944831/passing-listguid-as-web-service-parameter

//                            if (existOjectobj.GetType().GetProperty(propertyName).PropertyType.IsSubclassOf(typeof(XPBaseObject)))
//                            {
//                                var propOjectobj = XpoDefault.Session.FindObject(existOjectobj.GetType().GetProperty(propertyName).PropertyType, CriteriaOperator.Parse("[Oid] ='" + (singleModifiedObject.Values[i].ToString()) + "'"));
//                                if (propOjectobj != null)
//                                {
//                                    existOjectobj.SetMemberValue(propertyName, propOjectobj);
//                                }
//                                else
//                                { // cancel  and raise exception
//                                }
//                            }
//                            else
//                            {
//                                if (singleModifiedObject.Values[i] is Byte[])
//                                {
//                                    Bitmap im = new Bitmap(byteArrayToImage((byte[])singleModifiedObject.Values[i]));
//                                    im.Save("ImageName", System.Drawing.Imaging.ImageFormat.Bmp);
//                                    existOjectobj.SetMemberValue(propertyName, im);
//                                }
//                                else
//                                {
//                                    if (singleModifiedObject.Values[i] == DBNull.Value)
//                                    {
//                                        singleModifiedObject.Values[i] = null;
//                                    }
//                                    existOjectobj.SetMemberValue(propertyName, singleModifiedObject.Values[i]);
//                                }
//                            }
//                        }
//                    }
//                }
//            }
//            catch (Exception ex)
//            { }

//            return existOjectobj;
//        }
//        public static bool HasProperty(object obj, string propertyName)
//        {
//            return obj.GetType().GetProperty(propertyName) != null;
//        }
//        public string GetConvertTimeStamp(SyncDataObject singleModifiedObject, XPBaseObject existOjectobj, string accountSchema)
//        {
//            string returnedstring = "";
//            //SchemaHelper.SetCleintSchema("dbo");
//            //Account _Account = XpoDefault.Session.FindObject<Account>(CriteriaOperator.Parse("[Oid] = '" + singleModifiedObject.AccountOID.ToString() + "'"));
//            //List<XPClassInfo> typesInfo = SchemaHelper.SetCleintSchema(_Account.DBSchema);

//            //string AccountSchema = "";
//            //if (_Account != null)
//            //{
//            //    if (!string.IsNullOrWhiteSpace(_Account.DBSchema))
//            //    {
//            //        AccountSchema = _Account.DBSchema;
//            //    }
//            //    else
//            //    {
//            //        AccountSchema = _Account.AccountCode;
//            //    }
//            //}
//            String stringquery = string.Format("select  MAX([{0}].[AuditDataItemPersistent].[timestamp]) from  [{0}].AuditDataItemPersistent where Oid ='" + singleModifiedObject.EntityOID + "'", accountSchema);
//            SelectedData timestamp = XpoDefault.Session.ExecuteQuery(stringquery);
//            if (timestamp.ResultSet != null)
//            {
//                foreach (SelectStatementResultRow row1 in timestamp.ResultSet[0].Rows)
//                {
//                    string s2 = BitConverter.ToString((byte[])(row1.Values[0]));
//                    String[] tempAry = s2.Split('-');
//                    string st = "0x";
//                    foreach (string str in tempAry) st += str;
//                    singleModifiedObject.Version = st;
//                }
//                returnedstring = singleModifiedObject.TableName.ToString() + "|" + singleModifiedObject.EntityOID.ToString() + "|" + singleModifiedObject.Version.ToString();
//            }
//            return returnedstring;
//        }
//        public Image byteArrayToImage(byte[] byteArrayIn)
//        {
//            using (MemoryStream mStream = new MemoryStream(byteArrayIn))
//            {
//                return Image.FromStream(mStream);
//            }
//        }

//        # region "Notification"
//        public bool Register(Guid account, String deviceSeginture)
//        {
//            return true;
//            //    try
//            //    {
//            //            var channel =  PushNotificationChannelManager.CreatePushNotificationChannelForApplicationAsync();
//            //            var hub = new NotificationHub("mobileserviceresto", "Endpoint=sb://mobileserviceresto-ns.servicebus.windows.net/;SharedAccessKeyName=DefaultFullSharedAccessSignature;SharedAccessKey=x3TIEN74DpHlKwvS+oZS0a1r52kqZ8u2JqQibzAhvQg=");
//            //            List<string> tags = new List<string>();
//            //            tags.Add(account.ToString());
//            //            var result = hub.RegisterNativeAsync(channel.Uri, tags);
//            //            channel.PushNotificationReceived += OnPushNotification;
//            //    }
//            //    catch (Exception ex)
//            //    {

//            //    }
//        }
//        //public static void OnPushNotification(PushNotificationChannel sender, PushNotificationReceivedEventArgs e)
//        //{
//        //    String notificationContent = String.Empty;

//        //    switch (e.NotificationType)
//        //    {
//        //        case PushNotificationType.Badge:
//        //            notificationContent = e.BadgeNotification.Content.GetXml();
//        //            break;

//        //        case PushNotificationType.Tile:
//        //            notificationContent = e.TileNotification.Content.GetXml();
//        //            break;

//        //        case PushNotificationType.Toast:
//        //            notificationContent = e.ToastNotification.Content.GetXml();
//        //            break;

//        //        case PushNotificationType.Raw:
//        //            notificationContent = e.RawNotification.Content;
//        //            break;
//        //    }



//        //}
//        public bool Notify(String txt, Guid account)
//        {
//            //NotificationHubClient hub = NotificationHubClient.CreateClientFromConnectionString("Endpoint=sb://mariamtesthub-ns.servicebus.windows.net/;SharedAccessKeyName=DefaultFullSharedAccessSignature;SharedAccessKey=Z8q6zEf9qJ70eVYLj6LyPjO7UZ0ekDFY790cM+pB9b8=", "mariamtesthub");
//            //hub.SendWindowsNativeNotificationAsync(txt, account.ToString());
//            return true;
//        }
//        # endregion
//    }
//    // Sara.N,1 Synchronization Task [End]

//}