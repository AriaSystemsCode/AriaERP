using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
using DevExpress.Data.Filtering;
using DevExpress.Xpo;
using DevExpress.Xpo.DB;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using DotNetRuleEngine.Core;
using DevExpress.Xpo.Metadata;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Xpo;
using Aria5SystemAdmin.Module.BusinessObjects;
using System.Reflection;
using System.Threading.Tasks;
using DevExpress.Persistent.AuditTrail;
using DevExpress.Persistent.BaseImpl;
using Aria5SystemAdmin.Module;
using System.Data.SqlClient;
using System.IO;
using System.Drawing;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.Helpers;
using Aria5.DevExpress.OneTouchAway.Module.BusinessObjects;
using Aria5SystemAdmin.Module.Managers;
using System.Xml.Serialization;
using Microsoft.Azure.NotificationHubs;
using System.Web.Configuration;

namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Mangers
{
    public class SyncManager
    {
        public static Object thisLock = new Object();

        public string MapTableName(string returnedTableName)
        {
            string Oldtablename = returnedTableName.Substring(returnedTableName.LastIndexOf(".") + 1);
            if (Oldtablename.Contains("Client") || Oldtablename.Contains("CLIENT"))
            {
                Oldtablename = Oldtablename.Replace("Client", "");
            }

            return Oldtablename;
        }

        public string MapOperationType(string auditOperationType)
        {
            if (auditOperationType == "ObjectCreated") return "Add";
            if (auditOperationType == "ObjectChanged") return "Edit";
            if (auditOperationType == "ObjectDeleted") return "Delete";

            return "";
        }

        public string GetPropertyName(string FullPropertyName)
        {
            string[] FullPropertyNameArray = FullPropertyName.Split('.');
            string TableName = FullPropertyNameArray[0];
            string PropertyName = FullPropertyNameArray[1];
            return PropertyName;
        }

        public Image ByteArrayToImage(byte[] byteArrayIn)
        {
            using (MemoryStream mStream = new MemoryStream(byteArrayIn))
            {
                return Image.FromStream(mStream);
            }
        }

        public static byte[] ImageToByte(Image img)
        {
            ImageConverter converter = new ImageConverter();
            return (byte[])converter.ConvertTo(img, typeof(byte[]));
        }

        public string GetMinFrontEndVersion()
        {
            Session session = new Session(XpoHelper.GetNewDataLayer());

            string ret = "";
            SchemaHelper.SetCleintSchema(session, "dbo");

            Application_T App1 = (Application_T)session.FindObject(typeof(Application_T), CriteriaOperator.Parse("[Id] ='Aria5-Windows8Xaml-1TouchAwayFrontend'"));
            if (App1 != null)
            {
                try
                {
                    ret = ret + App1.ApplicationSettings.FirstOrDefault(r => r.SettingID == "MINFRONTENDVERSION").SettingValue.ToString();

                }
                catch (Exception)
                {

                    ret += "|";
                }
            }
            else
            {
            }

            Application_T App2 = (Application_T)session.FindObject(typeof(Application_T), CriteriaOperator.Parse("[Id] ='Aria5-Windows8Xaml-MainSystem'"));
            if (App2 != null)
            {
                try
                {

                    ret = ret + '|' + App2.ApplicationSettings.FirstOrDefault(r => r.SettingID == "MINFRONTENDVERSION").SettingValue.ToString();

                }
                catch (Exception)
                {

                    ret += "|";
                }

            }
            else
            {
                ret += "|";
            }
            Application_T App3 = (Application_T)session.FindObject(typeof(Application_T), CriteriaOperator.Parse("[Id] ='Aria5-DevExpress-SystemAdmin'"));
            if (App3 != null)
            {
                try
                {

                    ret = ret + '|' + App3.ApplicationSettings.FirstOrDefault(r => r.SettingID == "MINFRONTENDVERSION").SettingValue.ToString();

                }
                catch (Exception)
                {

                    ret += "|";
                }
            }
            else
            {
                ret += "|";
            }


            session.Dispose();

            return ret;
        }

        public string GetAccountSchema(Guid accountOid)
        {
            Session accountSession = new Session(XpoHelper.GetNewDataLayer());
            string accountSchema = "";

            SelectedData data = accountSession.ExecuteQuery("Select DbSchema from contact where oid = '" + accountOid.ToString() + "'");

            if (data.ResultSet != null && data.ResultSet.Count() > 0 && data.ResultSet[0].Rows.Count() > 0 &&
                data.ResultSet[0].Rows[0].Values[0] != null && data.ResultSet[0].Rows[0].Values[0] != DBNull.Value)
            {
                accountSchema = data.ResultSet[0].Rows[0].Values[0].ToString().Trim();
            }
            else
            {
                accountSchema = null;
            }

            accountSession.Dispose();

            return accountSchema;
        }

        public void SetProperties(SyncDataObject syncObject, ClientBaseObject existObject, Session session, string oid = "")
        {
            if (string.IsNullOrEmpty(oid) == false)
            {
                existObject.SetMemberValue("oid", Guid.Parse(oid.TrimEnd()));
            }

            for (int i = 0; i < syncObject.Properties.Count; i++)
            {
                try
                {
                    string propertyName = GetPropertyName(syncObject.TableName.ToString().TrimEnd().ToUpper() + "." + syncObject.Properties[i]);

                    if (propertyName.ToUpper().TrimEnd() != "TRANSACTIONTIMESTAMP" &&
                        propertyName.ToUpper().TrimEnd() != "HasSyncDataToBeSendInLog".ToUpper() &&
                        propertyName.ToUpper().TrimEnd() != "" &&
                        existObject.GetType().GetProperty(propertyName) != null)
                    {
                        if (syncObject.Values[i] == null || syncObject.Values[i] == DBNull.Value)
                        {
                            existObject.SetMemberValue(propertyName, null);
                        }
                        else
                        {
                            // Add the code to handle Guid parameter Ref: http://stackoverflow.com/questions/16944831/passing-listguid-as-web-service-parameter
                            if (existObject.GetType().GetProperty(propertyName).PropertyType.IsSubclassOf(typeof(XPBaseObject)) ||
                                existObject.GetType().GetProperty(propertyName).PropertyType.IsSubclassOf(typeof(BaseObject)) ||
                                existObject.GetType().GetProperty(propertyName).PropertyType.IsSubclassOf(typeof(ClientBaseObject)))
                            {
                                var propOjectobj = session.FindObject(existObject.GetType().GetProperty(propertyName).PropertyType, CriteriaOperator.Parse("[Oid] ='" + (syncObject.Values[i].ToString()) + "'"));
                                if (propOjectobj != null)
                                {
                                    existObject.SetMemberValue(propertyName, propOjectobj);
                                }
                            }
                            else
                            {
                                //17/05/2015 Fix Image Edit Issue not saved from Front end [Start]
                                if (existObject.GetType().GetProperty(propertyName).PropertyType == typeof(Image))
                                {
                                    //string result = System.Text.Encoding.UTF8.GetString(syncObject.Values[i] as byte[]);
                                    //11/08/2016 add,edit for big images take long time [Start]
                                    //List<byte> bytes = new List<byte>();

                                   
                                    //for (int j = 0; j < (syncObject.Values[i].ToString().Trim().Length - 1) / 3; j++)
                                    //{
                                    //    bytes.Add(Convert.ToByte(syncObject.Values[i].ToString().Substring(1 + 3 * j, 3)));
                                    //}

                                     
                                    byte[] bytes1 = Convert.FromBase64String(syncObject.Values[i].ToString());
                                    MemoryStream ms = new MemoryStream(bytes1);
                                    //11/08/2016 add,edit for big images take long time [End]


                                  //  MemoryStream ms = new MemoryStream(bytes.toArray);
                                   
                                    Bitmap image = new Bitmap(ms);
                                    existObject.SetMemberValue(propertyName, image);
                                }
                                //17/05/2015 Fix Image Edit Issue not saved from Front end [End]
                                //if (syncObject.Values[i] is Byte[])
                                //{
                                //    Bitmap image = new Bitmap(ByteArrayToImage((byte[])syncObject.Values[i]));
                                //    image.Save("ImageName", System.Drawing.Imaging.ImageFormat.Bmp);
                                //    existObject.SetMemberValue(propertyName, image);
                                //}
                                else
                                {
                                    if (syncObject.Values[i] == DBNull.Value)
                                    {
                                        syncObject.Values[i] = null;
                                    }
                                    ////existObject.SetMemberValue(propertyName, syncObject.Values[i]);

                                    //// mahmoud 5/16/2016
                                    if (existObject.GetType().GetProperty(propertyName).PropertyType == typeof(Guid))
                                    {
                                        existObject.SetMemberValue(propertyName, Guid.Parse(syncObject.Values[i].ToString()));
                                    }
                                    else
                                    {
                                        //    // mahmoud 5/16/2016

                                        existObject.SetMemberValue(propertyName, syncObject.Values[i]);
                                        //    // mahmoud 5/16/2016
                                    }
                                    //// mahmoud 5/16/2016
                                }
                            }
                        }
                    }
                }
                catch (Exception ex)
                {
                }
            }
        }

        public List<string> PushData(Guid accountOid, List<SyncDataObject> dataToUpdate, String deviceSeginture)//List<XPClassInfo> typesInfo
        
        {
            //  SqlConnection con = new SqlConnection(@"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=azuresqladmin;Password=aria_123");

            var accountSchema = GetAccountSchema(accountOid);
            if (accountSchema == null) return null;
            Session session = new Session(XpoHelper.GetNewDataLayer());
            List<XPClassInfo> typesInfo = SchemaHelper.SetCleintSchema(session, accountSchema);
            //List<XPClassInfo> typesInfo = ClientSessionInfo.SetClientDataLayer(accountOid);// SchemaHelper.SetCleintSchema(session, accountSchema);

            //if (typesInfo == null || typesInfo.Count == 0)
            //{
            //    throw new Exception("It's Empty");
            //}

            //Session session = new Session(XpoHelper.GetNewDataLayer());

            //Sara.N,1 Change the mechanism of the Push service to cash data layer 16-03-2016[End]

            List<string> result = new List<string>();

            foreach (var syncObject in dataToUpdate)
            {
                //18-05-2016 Threading Problem due to static property [Start]

                ClientEntity.CallFromSync = true;
                //18-05-2016 Threading Problem due to static property [End]

                string modificationType = syncObject.RecordStatus.Trim().ToUpper();
                string className = syncObject.TableName.TrimEnd();

                var typeInfo = typesInfo.Where(r => r.FindAttributeInfo(typeof(IsClient)) != null && ((IsClient)r.GetAttributeInfo(typeof(IsClient))).ClientTable)
                                        .First(r => (((IsClient)r.GetAttributeInfo(typeof(IsClient))).NeedPrefix && r.TableName == "Client" + className) ||
                                                     (!((IsClient)r.GetAttributeInfo(typeof(IsClient))).NeedPrefix && r.TableName == className) ||
                                                     (r.BaseClass.TableName == "ClientEntity" && r.ClassType.Name == className));


                switch (modificationType)
                {
                    case "ADD":
                        ClientBaseObject oldObject = (ClientBaseObject)session.FindObject(typeInfo.ClassType, CriteriaOperator.Parse("[Oid] ='" + syncObject.EntityOID + "'"));
                        if (oldObject != null)
                        {
                            goto case "EDIT";
                        }
                        else
                        {
                            //sara.N,1 Mapping the subclass table not the parent Class 09-06-2016 [Start]
                            ClientBaseObject addObject = (ClientBaseObject)Activator.CreateInstance(typeInfo.ClassType, session);
                            //ClientBaseObject addObject;
                            //if (typeInfo.ClassType == typeof(ClientEntity))
                            //{
                            //    int typeidIndex = syncObject.Properties.IndexOf("TypeId");
                            //    string typeid = syncObject.Values[typeidIndex].ToString();
                            //    XPClassInfo subclass = typesInfo.FirstOrDefault(s => s.ClassType.Name.ToUpper() == typeid.ToUpper() && s.ClassType.IsSubclassOf(typeof(ClientEntity)));
                            //    addObject = (ClientBaseObject)Activator.CreateInstance(subclass.ClassType, session);

                            //}
                            //else
                            //{
                            //     addObject = (ClientBaseObject)Activator.CreateInstance(typeInfo.ClassType, session);
                            //}
                            //sara.N,1 Mapping the subclass table not the parent Class 09-06-2016 [End]

                            //18-05-2016 Threading Problem due to static property [Start]
                            // addObject.CallFromSync = true;
                            //18-05-2016 Threading Problem due to static property [End]

                            SetProperties(syncObject, addObject, session, syncObject.EntityOID.ToString());

                            addObject.Save();

                            ClientDataAudit AddClientDataAudit = (ClientDataAudit)session.FindObject(typeof(ClientDataAudit), CriteriaOperator.Parse("[Oguid] ='" + addObject.ClientDataAuditOid + "'"));
                            
                            // Sara.N,1 08-03-2016 Add Device Signature to Pull data of All Devices except the user device [Start]                         
                            session.ExecuteNonQuery("update [" + accountSchema + "].[ClientDataAudit] set DeviceSignature='" + deviceSeginture.ToString() + "' where Oid=" + AddClientDataAudit.Oid+ " ");
                            // Sara.N,1 08-03-2016 Add Device Signature to Pull data of All Devices except the user device [End]
                            
                            result.Add(MapTableName(typeInfo.TableName) + "|" + syncObject.EntityOID.ToString() + "|" + AddClientDataAudit.Oid.ToString());
                        }
                        break;

                    case "EDIT":

                        //sara.N,1 Mapping the subclass table not the parent Class 09-06-2016 [Start]
                        ClientBaseObject editObject = (ClientBaseObject)session.FindObject(typeInfo.ClassType, CriteriaOperator.Parse("[Oid] ='" + syncObject.EntityOID + "'"));

                        //ClientBaseObject editObject;
                        //if (typeInfo.ClassType == typeof(ClientEntity))
                        //{
                        //    int typeidIndex = syncObject.Properties.IndexOf("TypeId");
                        //    string typeid = syncObject.Values[typeidIndex].ToString();
                        //    XPClassInfo subclass = typesInfo.FirstOrDefault(s => s.ClassType.Name.ToUpper() == typeid.ToUpper() && s.ClassType.IsSubclassOf(typeof(ClientEntity)));
                        //    editObject = (ClientBaseObject)Activator.CreateInstance(subclass.ClassType, session);

                        //}
                        //else
                        //{
                        //    editObject = (ClientBaseObject)Activator.CreateInstance(typeInfo.ClassType, session);
                        //}
                        //sara.N,1 Mapping the subclass table not the parent Class 09-06-2016 [End]
                        if (editObject == null)
                        {
                            goto case "ADD";
                        }
                        else
                        {
                            //18-05-2016 Threading Problem due to static property [Start]
                            // editObject.CallFromSync = true;
                            //18-05-2016 Threading Problem due to static property [End]

                            //Sara.n,1 Compare incoming Version if less than Current ignore Editing 08-06-2016 [Start]
                            //ClientDataAudit editClientDataAudit = (ClientDataAudit)session.FindObject(typeof(ClientDataAudit), CriteriaOperator.Parse("[Auditedobject] ='" + editObject.Oid + "'"));
                            XPCollection<ClientDataAudit> editClientDataAudit = new XPCollection<ClientDataAudit>(session, CriteriaOperator.Parse("[AuditedObject] ='" + editObject.Oid + "'"));
                            // editClientDataAudit.TopReturnedObjects
                            // editClientDataAudit.Reverse();
                            if (editClientDataAudit != null && editClientDataAudit.Count > 0)
                            {
                                ClientDataAudit editClientDataAuditMax = editClientDataAudit.OrderByDescending(r => r.Oid).FirstOrDefault();
                                // Sara.N,1 08-03-2016 Add Device Signature to Pull data of All Devices except the user device [Start]
                                //  if Frontend = Backend do as Normal to update back end and return the new version
                                if (!string.IsNullOrWhiteSpace(syncObject.Version) && editClientDataAudit != null && editClientDataAuditMax.Oid == int.Parse(syncObject.Version.Trim()))
                                {
                                   
                                    SetProperties(syncObject, editObject, session);
                                    editObject.Save();
//                                    result.Add(MapTableName(typeInfo.TableName) + "|" + syncObject.EntityOID.ToString() + "|" + editClientDataAuditMax.Oid.ToString());

                                    XPCollection<ClientDataAudit> editClientDataAuditafteredit = new XPCollection<ClientDataAudit>(session, CriteriaOperator.Parse("[AuditedObject] ='" + editObject.Oid + "'"));

                                    ClientDataAudit editClientDataAuditMaxafteredit = editClientDataAuditafteredit.OrderByDescending(r => r.Oid).FirstOrDefault();
                                    // Sara.N,1 08-03-2016 Add Device Signature to Pull data of All Devices except the user device [Start]                         
                                    session.ExecuteNonQuery("update [" + accountSchema + "].[ClientDataAudit] set DeviceSignature='" + deviceSeginture.ToString() + "' where Oid=" + editClientDataAuditMaxafteredit.Oid + " ");

                                    // Sara.N,1 08-03-2016 Add Device Signature to Pull data of All Devices except the user device [End]
                                    result.Add(MapTableName(typeInfo.TableName) + "|" + syncObject.EntityOID.ToString() + "|" + editClientDataAuditMaxafteredit.Oid.ToString());
                                    //sara

                                }
                                // if Backend > frontend return the backend number
                                else if (!string.IsNullOrWhiteSpace(syncObject.Version) && editClientDataAudit != null && editClientDataAuditMax.Oid > int.Parse(syncObject.Version.Trim()))
                                {

                                    result.Add(MapTableName(typeInfo.TableName) + "|" + syncObject.EntityOID.ToString() + "|" + editClientDataAuditMax.Oid.ToString());

                                }
                                else
                                {  // Return the Incoming frontEnd version
                                    result.Add(MapTableName(typeInfo.TableName) + "|" + syncObject.EntityOID.ToString() + "|" + syncObject.Version);
                                }
                            }
                            else
                            {
                                // return incoming version
                                result.Add(MapTableName(typeInfo.TableName) + "|" + syncObject.EntityOID.ToString() + "|" + syncObject.Version);
                            }
                            // editClientDataAudit.
                            //Sara.n,1 Compare incoming Version if less than Current ignore Editing 06-08-2016 [End]

                            // SetProperties(syncObject, editObject, session);
                            //editObject.Save();

                            //18-05-2016 Threading Problem due to static property [End]
                            //Sara.n,1 Compare incoming Version if less than Current ignore Editing 06-08-2016 [Start]
                            //  ClientDataAudit editClientDataAudit = (ClientDataAudit)session.FindObject(typeof(ClientDataAudit), CriteriaOperator.Parse("[Oguid] ='" + editObject.ClientDataAuditOid + "'"));


                            //Sara.n,1 Compare incoming Version if less than Current ignore Editing 06-08-2016 [End]
                            // result.Add(MapTableName(typeInfo.TableName) + "|" + syncObject.EntityOID.ToString() + "|" + editClientDataAudit.Oid.ToString());
                        }
                        break;

                    case "DELETE":
                        ClientBaseObject deleteOjectobj = (ClientBaseObject)session.FindObject(typeInfo.ClassType, CriteriaOperator.Parse("[Oid] ='" + syncObject.EntityOID + "'"));
                        if (deleteOjectobj != null)
                        {
                            //18-05-2016 Threading Problem due to static property [Start]
                            // deleteOjectobj.CallFromSync = true;
                            //18-05-2016 Threading Problem due to static property [End]

                            deleteOjectobj.Delete();
                            ClientDataAudit deleteClientDataAudit = (ClientDataAudit)session.FindObject(typeof(ClientDataAudit), CriteriaOperator.Parse("[Oguid] ='" + deleteOjectobj.ClientDataAuditOid + "'"));

                            // Sara.N,1 08-03-2016 Add Device Signature to Pull data of All Devices except the user device [Start]                         
                            session.ExecuteNonQuery("update [" + accountSchema + "].[ClientDataAudit] set DeviceSignature='" + deviceSeginture.ToString() + "' where Oid=" + deleteClientDataAudit.Oid + " ");

                            // Sara.N,1 08-03-2016 Add Device Signature to Pull data of All Devices except the user device [End]

                            result.Add(MapTableName(typeInfo.TableName) + "|" + syncObject.EntityOID.ToString() + "|" + deleteClientDataAudit.Oid.ToString());
                        }

                        break;
                }
            }

            //18-05-2016 Threading Problem due to static property [Start]

            ClientEntity.CallFromSync = false;

            //18-05-2016 Threading Problem due to static property [End]

            session.Dispose();
            var NotifyResult = Notify("SendNotification", accountOid);
            return result;
        }

        public List<SyncDataObject> PullData(Guid accountOid, int fromVersion, int toVersion, String deviceSeginture)
        {
            //Session accountSession = new Session(XpoHelper.GetNewDataLayer());

            //Account _Account = accountSession.FindObject<Account>(CriteriaOperator.Parse("[Oid] = '" + accountOid.ToString() + "'"));
            //if (_Account == null) return null;

            //string AccountSchema = "";
            //AccountSchema = _Account.DBSchema;
            //accountSession.Dispose();
            // List<XPClassInfo> typesInfo = SchemaHelper.SetCleintSchema(session, AccountSchema);

            //Session session = new Session(XpoHelper.GetNewDataLayer());
            //if (typesInfo == null || typesInfo.Count == 0)
            //{
            //    throw new Exception("It's Empty");
            //}

            var accountSchema = GetAccountSchema(accountOid);
            if (accountSchema == null) return null;
            Session session = new Session(XpoHelper.GetNewDataLayer());
            List<XPClassInfo> typesInfo = SchemaHelper.SetCleintSchema(session, accountSchema);
              // Sara.N,1 08-03-2016 Add Device Signature to Pull data of All Devices except the user device [Start]
             XPCollection<ClientDataAudit> audits ;
            
                if (string.IsNullOrWhiteSpace(deviceSeginture) || string.IsNullOrEmpty(deviceSeginture)) // Pull All data 
                {
                     audits = new XPCollection<ClientDataAudit>(session, CriteriaOperator.Parse("[Oid] >= " + fromVersion.ToString() + " AND " + "[Oid] <= " + toVersion), new SortProperty[] { new SortProperty("Oid", SortingDirection.Ascending) });

                }
                else // Pull Data but exculde my device data 
                {
                   // audits = new XPCollection<ClientDataAudit>(session, CriteriaOperator.Parse("[Oid] >= " + fromVersion.ToString() + " AND " + "[Oid] <= " + toVersion), new SortProperty[] { new SortProperty("Oid", SortingDirection.Ascending) });
                    CriteriaOperator C1 = new BinaryOperator("Oid", fromVersion, BinaryOperatorType.GreaterOrEqual);
                    CriteriaOperator C2 = new BinaryOperator("Oid", toVersion, BinaryOperatorType.LessOrEqual);
                    CriteriaOperator C3 = new BinaryOperator("DeviceSignature", deviceSeginture, BinaryOperatorType.NotEqual);

                    audits = new XPCollection<ClientDataAudit>(session, CriteriaOperator.Parse(C1.LegacyToString()+"AND"+C2.LegacyToString()+"AND"+C3.LegacyToString()), new SortProperty[] { new SortProperty("Oid", SortingDirection.Ascending) });
                 
                  //  audits = new XPCollection<ClientDataAudit>(session, CriteriaOperator.Parse("[Oid] >= " + fromVersion.ToString() + " AND " + "[Oid] <= " + toVersion.ToString() + " AND " + "[DeviceSignature] = " + deviceSeginture.ToString()), new SortProperty[] { new SortProperty("Oid", SortingDirection.Ascending) });                    
                }
                // Sara.N,1 08-03-2016 Add Device Signature to Pull data of All Devices except the user device [End]

            var auditedObjectsTypes = audits.Select(r => r.AuditedObjectTypeName).Distinct().ToList();

            List<SyncDataObject> result = new List<SyncDataObject>();

            foreach (var auditedObjectType in auditedObjectsTypes)
            {
                var auditedObjectsForType = audits.Where(r => r.AuditedObjectTypeName == auditedObjectType);

                string select = "SELECT " +
                                "{0}.[Oid], " +                         // 0
                                "{0}.[Oguid], " +                       // 1
                                "{0}.[AuditedObjectTypeName], " +       // 2
                                "{0}.[AuditedObject], " +               // 3
                                "{0}.[AuditOperationType], " +          // 4
                                "{0}.[AddDateTime], " +                 // 5
                                       // 6
                                "{0}.[OptimisticLockField], " +         // 6
                                "{0}.[GCRecord]," +                     // 7
                                "{0}.[DeviceSignature], ";               // 8

                string tableName = MapTableName(auditedObjectType);

                var classInfo = typesInfo.Find(r => r.FullName.ToUpper().Trim() == auditedObjectType.ToUpper().Trim());

                int colIndex = 7; // GCRECORD Column #
                int colKeyIndex = -1;
                foreach (XPMemberInfo prop in classInfo.PersistentProperties)
                {
                    if (prop.Name != "ObjectType")
                    {
                        colIndex++;
                        select += "{1}.[" + prop.Name + "], ";

                        if (prop.Name.ToUpper().Trim() == "Oid".ToUpper())
                        {
                            colKeyIndex = colIndex;
                        }
                    }
                }

                select = select.Substring(0, select.Length - 2) + " FROM {0} LEFT JOIN {1} ON {0}.[AuditedObject] = {1}.Oid WHERE {0}.Oid >= {2} AND {0}.Oid <= {3} AND {0}.[AuditedObjectTypeName] = '{4}' AND {0}.[DeviceSignature] !='{5}'";
                    select = string.Format(select, "[" + accountSchema + "].[ClientDataAudit]", classInfo.Table.Name, fromVersion.ToString(), toVersion.ToString(), auditedObjectType,deviceSeginture.ToString());  
                
                var resultSubSet = session.ExecuteQuery(select);

                foreach (var row in resultSubSet.ResultSet[0].Rows)
                {
                    //if (audits.FirstOrDefault(r=>r.Oid=)
                    //{
                        
                    //}
                    SyncDataObject newItem = new SyncDataObject();
                    newItem.TableName = tableName;
                    newItem.RecordStatus = MapOperationType(row.Values[4].ToString());
                    newItem.EntityOID = Guid.Parse(row.Values[3].ToString());

                    newItem.Version = row.Values[0].ToString();

                    newItem.Properties = new List<string>();
                    newItem.Values = new List<object>();

                    if (newItem.RecordStatus == "Delete")
                    {
                        result.Add(newItem);
                    }
                    else
                    {
                        if (row.Values[colKeyIndex] != null)
                        {
                            colIndex = 8;
                            foreach (XPMemberInfo prop in classInfo.PersistentProperties)
                            {
                                if (prop.Name != "ObjectType")
                                {
                                    colIndex++;

                                    newItem.Properties.Add(prop.Name);
                                    newItem.Values.Add(row.Values[colIndex]);
                                }
                            }

                            result.Add(newItem);
                        }
                    }
                }
            }

            session.Dispose();

            return result;
        }
        # region Commented Pull and push Data
        //public List<SyncDataObject> PullData(Guid accountOid, int fromVersion, int toVersion, String deviceSeginture, List<XPClassInfo> typesInfo)
        //{
        //    //Session accountSession = new Session(XpoHelper.GetNewDataLayer());

        //    //Account _Account = accountSession.FindObject<Account>(CriteriaOperator.Parse("[Oid] = '" + accountOid.ToString() + "'"));
        //    //if (_Account == null) return null;

        //    //string AccountSchema = "";
        //    //AccountSchema = _Account.DBSchema;
        //    //accountSession.Dispose();
        //    // List<XPClassInfo> typesInfo = SchemaHelper.SetCleintSchema(session, AccountSchema);

        //    Session session = new Session(XpoHelper.GetNewDataLayer());
        //    if (typesInfo == null || typesInfo.Count == 0)
        //    {
        //        throw new Exception("It's Empty");
        //    }

        //    XPCollection<ClientDataAudit> audits = new XPCollection<ClientDataAudit>(session, CriteriaOperator.Parse("[Oid] >= " + fromVersion.ToString() + " AND " + "[Oid] <= " + toVersion), new SortProperty[] { new SortProperty("Oid", SortingDirection.Ascending) });

        //    List<SyncDataObject> result = new List<SyncDataObject>();

        //    foreach (var audit in audits)
        //    {
        //        SyncDataObject newItem = new SyncDataObject();
        //        newItem.TableName = MapTableName(audit.AuditedObjectTypeName);
        //        newItem.RecordStatus = MapOperationType(audit.AuditOperationType);
        //        newItem.EntityOID = audit.AuditedObject;

        //        newItem.Version = audit.Oid.ToString();

        //        newItem.Properties = new List<string>();
        //        newItem.Values = new List<object>();

        //        if (newItem.RecordStatus == "Delete")
        //        {
        //            result.Add(newItem);
        //        }
        //        else
        //        {
        //            ClientBaseObject retObject = (ClientBaseObject)session.FindObject(typeof(ClientEntity).Assembly.GetType(audit.AuditedObjectTypeName), CriteriaOperator.Parse("[Oid] ='" + newItem.EntityOID + "'"));

        //            if (retObject != null)
        //            {
        //                foreach (XPMemberInfo prop in retObject.ClassInfo.PersistentProperties)
        //                {
        //                    if (prop.Name != "ObjectType")
        //                    {
        //                        newItem.Properties.Add(prop.Name);

        //                        if (retObject.GetMemberValue(prop.Name) == null)
        //                        {
        //                            newItem.Values.Add(null);
        //                        }
        //                        else if (retObject.GetMemberValue(prop.Name).GetType().IsSubclassOf(typeof(ClientBaseObject)))
        //                        {
        //                            newItem.Values.Add(((ClientBaseObject)retObject.GetMemberValue(prop.Name)).Oid);
        //                        }
        //                        else if (retObject.GetMemberValue(prop.Name).GetType().IsSubclassOf(typeof(BaseObject)))
        //                        {
        //                            newItem.Values.Add(((BaseObject)retObject.GetMemberValue(prop.Name)).Oid);
        //                        }
        //                        else if (retObject.GetMemberValue(prop.Name).GetType().IsSubclassOf(typeof(XPBaseObject)))
        //                        {
        //                            newItem.Values.Add(((BaseObject)retObject.GetMemberValue(prop.Name)).Oid);
        //                        }
        //                        else if (retObject.GetMemberValue(prop.Name).GetType() == typeof(Guid))
        //                        {
        //                            newItem.Values.Add(retObject.GetMemberValue(prop.Name).ToString());
        //                        }
        //                        else if (retObject.GetMemberValue(prop.Name).GetType() == typeof(Bitmap))
        //                        {
        //                            newItem.Values.Add(ImageToByte(((Bitmap)retObject.GetMemberValue(prop.Name))));
        //                        }
        //                        else
        //                        {
        //                            newItem.Values.Add(retObject.GetMemberValue(prop.Name));
        //                        }
        //                    }
        //                }

        //                result.Add(newItem);
        //            }
        //        }
        //    }

        //    session.Dispose();

        //    return result;
        //}

        # endregion
        public int GetDbVersion(Guid accountOid)//, List<XPClassInfo> typesInfo
        {
            // Session accountSession = new Session(XpoHelper.GetNewDataLayer());

            //Account _Account = accountSession.FindObject<Account>(CriteriaOperator.Parse("[Oid] = '" + accountOid.ToString() + "'"));
            //if (_Account == null) return 0;

            //string AccountSchema = "";
            //AccountSchema = _Account.DBSchema;
            //accountSession.Dispose();


            var accountSchema = GetAccountSchema(accountOid);
            if (accountSchema == null) return 0;
            Session session = new Session(XpoHelper.GetNewDataLayer());
            List<XPClassInfo> typesInfo = SchemaHelper.SetCleintSchema(session, accountSchema);

            var value = session.Evaluate<ClientDataAudit>(CriteriaOperator.Parse("Max(Oid)"), null);
            session.Dispose();

            int result;
            if (value != null && Int32.TryParse(value.ToString(), out result))
            {
                return result;
            }
            else
            {
                return 0;
            }
        }

        public bool Notify(String txt, Guid account)
        {
            //TFSService.WebService1SoapClient webService1SoapClient = new TFSService.WebService1SoapClient();
            //webService1SoapClient.Notify(txt, account);
            //return true;

            NotificationHubClient hub = NotificationHubClient.CreateClientFromConnectionString(WebConfigurationManager.ConnectionStrings["AzureMobileConnectionString"].ConnectionString.ToString(), "mobileserviceresto");
            Notification notification = new WindowsNotification("New Data");
            notification.Headers.Add("X-WNS-Cache-Policy", "cache");
            notification.Headers.Add("X-WNS-Type", "wns/raw");
            notification.ContentType = "application/octet-stream";
            hub.SendNotificationAsync(notification, account.ToString());
            return true;
        }
    }
}