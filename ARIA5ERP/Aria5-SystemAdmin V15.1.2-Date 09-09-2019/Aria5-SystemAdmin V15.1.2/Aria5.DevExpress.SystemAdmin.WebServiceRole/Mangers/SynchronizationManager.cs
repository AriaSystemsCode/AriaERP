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
///using Microsoft.Azure.NotificationHubs;
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
//using Microsoft.WindowsAzure.Messaging;
//using DevExpress.Internal.WinApi.Windows.Networking.PushNotifications;


//using Microsoft.r
namespace Aria5.DevExpress.SystemAdmin.WebServiceRole.Mangers
{
    public class SynchronizationManager
    {
        public List<SyncDataObject> PullData(String fromVersion, String toVersion, Guid accountOid)
        {
            // should return List<SyncDataObject>
            Session session = XpoDefault.Session;
             //  session.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {

                SelectedData data = session.ExecuteSproc("GetModifiedDataAfterVersion", new OperandValue(fromVersion), new OperandValue(toVersion));
                List<SyncDataObject> mod = new List<SyncDataObject>();
                if (data.ResultSet != null)
                {
                    foreach (SelectStatementResultRow row in data.ResultSet[0].Rows)
                    {
                        SyncDataObject obj = mod.FirstOrDefault(r => r.EntityOID == Guid.Parse(row.Values[1].ToString()));
                        if (obj != null)
                        {
                            obj.TableName = row.Values[7].ToString();
                            obj.RecordStatus = row.Values[8].ToString();
                            //   obj.Version = row.Values[4].ToString();
                            string p = obj.Properties.FirstOrDefault(r => r.Contains(row.Values[2].ToString()));
                            if (string.IsNullOrWhiteSpace(p))
                            {
                                obj.Properties.Add(row.Values[2].ToString());
                                obj.Values.Add(row.Values[3].ToString());
                            }
                        }
                        else
                        {
                            SyncDataObject modifiedObject = new SyncDataObject();
                            modifiedObject.Properties = new List<string>();
                            modifiedObject.Values = new List<object>();
                            modifiedObject.EntityOID = Guid.Parse(row.Values[1].ToString());
                            modifiedObject.TableName = row.Values[7].ToString();
                            modifiedObject.RecordStatus = row.Values[8].ToString();

                            // Convert time Stamp to String
                            string s2 = BitConverter.ToString((byte[])(row.Values[4]));
                            String[] tempAry = s2.Split('-');
                            string st = "0x";
                            foreach (string str in tempAry) st += str;
                            modifiedObject.Version = st;

                            string p = modifiedObject.Properties.FirstOrDefault(r => r.Contains(row.Values[2].ToString()));
                            if (string.IsNullOrWhiteSpace(p))
                            {
                                modifiedObject.Properties.Add(row.Values[2].ToString());
                                modifiedObject.Values.Add(row.Values[3].ToString());
                            }
                            mod.Add(modifiedObject);
                        }
                    }
                    return mod;
                }
                else
                {
                    return mod;
                }
            }
            else
            {

            }

            return new List<SyncDataObject>();
        }
        public List<string> PushData(Guid accountOid, List<SyncDataObject> dataToUpdate, String deviceSeginture)//,List<SyncDataObject> dataToUpdate)
        {
            List<string> returnedlist = new List<string>();
            string returnedstring = "";
            Session session = XpoDefault.Session;
       //   session.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";

            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {

              

                XPDictionary xpDictionary = XpoTypesInfoHelper.GetXpoTypeInfoSource().XPDictionary;
                var typesInfo = xpDictionary.CollectClassInfos(typeof(Account).Assembly);
                foreach (var singleModifiedObject in dataToUpdate)
                {
                    string className = singleModifiedObject.TableName.ToString().ToUpper();
                    switch (singleModifiedObject.TableName.ToString().ToUpper())
                    {
                        case "ATTACHMENTS": className = "ATTACHMENT"; break;
                        case "PROFILEGROUP": className = "ProfileSection"; break;
                        case "PROFILEGROUPTILE": className = "ProfileSectionTile"; break;
                        case "ENTITIESRELATIONSHIP": className = "EntityRelationship"; break;

                        default: className = singleModifiedObject.TableName.ToString().ToUpper();
                            break;
                    }

                    if (className == "ENTITY")
                    {
                        //className = singleModifiedObject.GetType().GetProperty("TypeId").GetValue(singleModifiedObject).ToString();
                        for (int i = 0; i < singleModifiedObject.Properties.Count; i++)
                        {
                            if (singleModifiedObject.Properties[i].ToString().ToUpper().TrimEnd() == "TYPEID")
                            {

                                className = singleModifiedObject.Values[i].ToString();
                                break;
                            }
                        }

                    }

                    //string typeString = typesInfo.FirstOrDefault(r => r.TableName == singleModifiedObject.TableName.ToString()).ToString();

                    foreach (var item in typesInfo)
                    {
                          

                        XPClassInfo classInfo = item as XPClassInfo;
                        //if (classInfo.FullName == singleModifiedObject.TableName.ToString() || classInfo.TableName == singleModifiedObject.TableName.ToString())
                       

                        if (classInfo.FullName.ToUpper() == className || classInfo.TableName.ToUpper() == className)
                        {

                            
                            session.Dictionary.GetDataStoreSchema(classInfo.ClassType);
                            AuditTrailService.Instance.SetXPDictionary(session.Dictionary);
                            AuditTrailService.Instance.AuditDataStore = new AuditDataStore<AuditDataItemPersistent, AuditedObjectWeakReference>();
                            AuditTrailService.Instance.BeginSessionAudit(session, AuditTrailStrategy.OnObjectChanged, ObjectAuditingMode.Full);
                            string ModificationType = singleModifiedObject.RecordStatus.Trim().ToUpper();
                            switch (ModificationType)
                            {
                                case "EDIT":
                                    #region Edit
                                    {

                                        //SelectedData data = session.ExecuteQuery("select Top 1 NewOID from  EntityRef where RefOid='" + singleModifiedObject.EntityOID + "'");
                                        //if (!(data.ResultSet != null && data.ResultSet[0].Rows.Length > 0))
                                        //{
                                        //    goto case "ADD";
                                        //}
                                        //else
                                        //{
                                        //SelectStatementResultRow row = data.ResultSet[0].Rows[0];

                                        //Guid passedOid = singleModifiedObject.EntityOID;
                                        //singleModifiedObject.EntityOID = Guid.Parse(row.Values[0].ToString());

                                        XPBaseObject existOjectobj = (XPBaseObject)Session.DefaultSession.FindObject(classInfo.ClassType, CriteriaOperator.Parse("[Oid] ='" + singleModifiedObject.EntityOID + "'"));
                                        if (existOjectobj == null)
                                        {
                                            goto case "ADD";
                                        }
                                        else
                                        {
                                            existOjectobj = SetProperties(singleModifiedObject, existOjectobj);
                                            existOjectobj.Save();
                                            AuditTrailService.Instance.SaveAuditData(session);
                                            returnedlist.Add(GetConvertTimeStamp(singleModifiedObject, existOjectobj, session));
                                        }

                                        break;
                                    }

                                // }
                                    #endregion Edit

                                case "ADD":
                                    #region Add
                                    {
                                        // Mahmoud Code
                                        XPBaseObject existOjectobj = (XPBaseObject)Activator.CreateInstance(classInfo.ClassType, session);
                                        existOjectobj = SetProperties(singleModifiedObject, existOjectobj, singleModifiedObject.EntityOID.ToString());
                                        //try
                                        //{
                                        //    if (singleModifiedObject.TableName.ToString().ToUpper() == "ENTITY")
                                        //    existOjectobj.GetType().GetMethod("FillExtraData").Invoke(existOjectobj, null);
                                        //}
                                        //catch (Exception ex) { }

                                        existOjectobj.Save();
                                        AuditTrailService.Instance.SaveAuditData(session);
                                        session.ExecuteNonQuery("Insert into EntityRef values ('" + session.GetKeyValue(existOjectobj) + "','" + singleModifiedObject.EntityOID + "')");
                                        returnedlist.Add(GetConvertTimeStamp(singleModifiedObject, existOjectobj, session));

                                        break;
                                    }
                                    #endregion Add

                                case "DELETE":
                                    #region Delete
                                    {
                                        var existOjectobj = (XPBaseObject)Session.DefaultSession.FindObject(classInfo.ClassType, CriteriaOperator.Parse("[Oid] ='" + singleModifiedObject.EntityOID + "'"));
                                        if (existOjectobj != null)
                                        {
                                            existOjectobj.Delete();
                                            AuditTrailService.Instance.SaveAuditData(session);
                                            returnedlist.Add(GetConvertTimeStamp(singleModifiedObject, existOjectobj, session));
                                            break;
                                        }
                                        else
                                        {
                                            returnedstring = classInfo.TableName + "|" + singleModifiedObject.EntityOID.ToString() + "|" + singleModifiedObject.Version.ToString();

                                            break;
                                        }
                                    }
                                    #endregion Delete
                            }
                            //break foreach type
                            break;
                         }
                    }
                }
                return returnedlist;
            }

            return returnedlist;
        }
        public string GetDBVersion(Guid accountOid)
        {
            Session session = XpoDefault.Session;
            //  session.ConnectionString = @"Data Source=NSDE_SARHN;Initial Catalog=Azure_Aria5SystemAdmin_Staging;User ID=sa;Password=aria_123";
            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                SelectedData data = session.ExecuteQuery("select  MAX([dbo].[AuditDataItemPersistent].[timestamp]) from   dbo.AuditDataItemPersistent");
                if (data.ResultSet != null)
                {
                    foreach (SelectStatementResultRow row in data.ResultSet[0].Rows)
                    {
                        string s2 = BitConverter.ToString((byte[])(row.Values[0]));
                        String[] tempAry = s2.Split('-');
                        string st = "0x";
                        foreach (string str in tempAry) st += str;
                        return st;
                    }
                    return "";
                }
                else
                {
                    return "";
                }
            }
            return "";
        }

        public string GetMinFrontEndVersion()
        {

            string ret = "";
            Session session = XpoDefault.Session;
            //session.ConnectionString = @"Data Source=NSDE_SARHN;Initial Catalog=Azure_Aria5SystemAdmin_Staging;User ID=sa;Password=aria_123";
            session.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                //SelectedData data = session.ExecuteQuery("select  MIN([dbo].[AuditDataItemPersistent].[timestamp]) from   dbo.AuditDataItemPersistent");
                //if (data.ResultSet != null)
                //{
                //    foreach (SelectStatementResultRow row in data.ResultSet[0].Rows)
                //    {
                //        string s2 = BitConverter.ToString((byte[])(row.Values[0]));
                //        String[] tempAry = s2.Split('-');
                //        string st = "0x";
                //        foreach (string str in tempAry) st += str;
                //        return st;
                //    }
                //    return "";
                //}
                //else
                //{
                //    return "";
                //}

                Application_T App1 = (Application_T)Session.DefaultSession.FindObject(typeof(Application_T), CriteriaOperator.Parse("[Id] ='Aria5-Windows8Xaml-1TouchAwayFrontend'"));
                                                                                                                                           
                if (App1!=null)
                { ret = ret+'|'+ App1.ApplicationSettings.FirstOrDefault(r => r.SettingID == "MINFRONTENDVERSION").SettingValue.ToString(); }

                Application_T App2 = (Application_T)Session.DefaultSession.FindObject(typeof(Application_T), CriteriaOperator.Parse("[Id] ='Aria5-Windows8Xaml-MainSystem'"));
                if (App2 != null)
                { ret = ret + '|' + App2.ApplicationSettings.FirstOrDefault(r => r.SettingID == "MINFRONTENDVERSION").SettingValue.ToString(); }

                Application_T App3 = (Application_T)Session.DefaultSession.FindObject(typeof(Application_T), CriteriaOperator.Parse("[Id] ='Aria5-DevExpress-SystemAdmin'"));
                if (App3 != null)
                { ret = ret + '|' + App3.ApplicationSettings.FirstOrDefault(r => r.SettingID == "MINFRONTENDVERSION").SettingValue.ToString(); }

                //Min FrontEnd Version

            }
            if (string.IsNullOrEmpty(ret))
            {
                ret = ret.Substring(1);

            }

            return ret;
        }

        public bool Register(Guid account, String deviceSeginture)
        {
            return true;
            try
            {
                //var channel = PushNotificationChannelManager.CreatePushNotificationChannelForApplicationAsync();
                //var hub = new NotificationHub("mobileserviceresto", "Endpoint=sb://mobileserviceresto-ns.servicebus.windows.net/;SharedAccessKeyName=DefaultFullSharedAccessSignature;SharedAccessKey=x3TIEN74DpHlKwvS+oZS0a1r52kqZ8u2JqQibzAhvQg=");
                //List<string> tags = new List<string>();
                //tags.Add(account.ToString());
                //var result = hub.RegisterNativeAsync(channel.Uri, tags);
                //channel.PushNotificationReceived += OnPushNotification;
            }
            catch (Exception ex)
            {

            }
        }
        //public static void OnPushNotification(PushNotificationChannel sender, PushNotificationReceivedEventArgs e)
        //{
        //    String notificationContent = String.Empty;

        //    switch (e.NotificationType)
        //    {
        //        case PushNotificationType.Badge:
        //            notificationContent = e.BadgeNotification.Content.GetXml();
        //            break;

        //        case PushNotificationType.Tile:
        //            notificationContent = e.TileNotification.Content.GetXml();
        //            break;

        //        case PushNotificationType.Toast:
        //            notificationContent = e.ToastNotification.Content.GetXml();
        //            break;

        //        case PushNotificationType.Raw:
        //            notificationContent = e.RawNotification.Content;
        //            break;
        //    }



        //}
        public bool Notify(String txt, Guid account)
        {
            //NotificationHubClient hub = NotificationHubClient.CreateClientFromConnectionString("Endpoint=sb://mariamtesthub-ns.servicebus.windows.net/;SharedAccessKeyName=DefaultFullSharedAccessSignature;SharedAccessKey=Z8q6zEf9qJ70eVYLj6LyPjO7UZ0ekDFY790cM+pB9b8=", "mariamtesthub");
            //hub.SendWindowsNativeNotificationAsync(txt, account.ToString());
            return true;
        }

        public string GetPropertyName(string FullPropertyName)
        {
            string[] FullPropertyNameArray = FullPropertyName.Split('.');
            string TableName = FullPropertyNameArray[0];
            string PropertyName = FullPropertyNameArray[1];

            switch (TableName.Trim().ToUpper())
            {
                case "ENTITY":
                    #region Handle "Entity" Table
                    switch (PropertyName.Trim().ToUpper())
                    {
                        case "Id": return "Id";
                        case "TYPE": return "EntityType";
                        case "CATEGORY": return "EntityCategory";
                        case "CLASSIFICATION": return "EntityClassification";
                        case "TRANSACTIONTIMESTAMP": return "";
                        case "ADDDATE": return "EnteredDate";
                        case "ACCOUNT": return "Division";
                        case "GUID": return "";

                        default:
                            return PropertyName;
                    }
                    #endregion Handle "Entity" Table

                case "NEWS":
                    #region Handle "Entity" Table
                    switch (PropertyName.Trim().ToUpper())
                    {
                        case "Id": return "Id";
                        case "TYPE": return "EntityType";
                        case "CATEGORY": return "EntityCategory";
                        case "CLASSIFICATION": return "EntityClassification";
                        case "TRANSACTIONTIMESTAMP": return "";
                        case "ADDDATE": return "EnteredDate";
                        case "ACCOUNT": return "Division";
                        case "GUID": return "";

                        default:
                            return PropertyName;
                    }
                    #endregion Handle "Entity" Table

                case "ACCOUNT":
                    #region Handle "Account" Table
                    switch (PropertyName.Trim().ToUpper())
                    {
                        case "AccountId":
                            return "Id";

                        //case "Contact":
                        //    return "Id";

                        //case "ActiveProfile":
                        //    return "Id";

                        //case "Logo":
                        //    return "Id";

                        //case "IsActive":
                        //    return "Id";

                        //case "DefaultLanguage":
                        //    return "Id";

                        //case "DBVersion":
                        //    return "Id";



                        case "ENTITYTYPE":
                            return "Type";
                        case "ENTITYCATEGORY":

                            return "Category";
                        case "EntityClassification":

                            return "Classification";
                        case "TRANSACTIONTIMESTAMP":

                            return "";
                        case "ADDDATE":

                            return "EnteredDate";

                        case "ACCOUNT":

                            return "Division";

                        default:
                            return "";
                    }
                    #endregion Handle "Entity" Table

                case "ATTACHMENTS":
                    #region Handle "ATTACHMENTS" Table
                    switch (PropertyName.Trim().ToUpper())
                    {
                        case "ATTACHMENT":
                            return "AttachmentFile";
                        default:
                            return PropertyName;
                    }

                    #endregion Handle "ATTACHMENTS" Table

                default:
                    return PropertyName;
            }
        }

        public XPBaseObject SetProperties(SyncDataObject singleModifiedObject, XPBaseObject existOjectobj, string Poid = "")
        {
            try
            {
                if (string.IsNullOrEmpty(Poid) == false)
                {
                    existOjectobj.SetMemberValue("oid", Guid.Parse(Poid.TrimEnd()));
                }
            }
            catch (Exception ex)
            { }


            for (int i = 0; i < singleModifiedObject.Properties.Count; i++)
            {
                if (singleModifiedObject.Values[i] != null)
                {


                    string propertyName = GetPropertyName(singleModifiedObject.TableName.ToString().TrimEnd().ToUpper() + "." + singleModifiedObject.Properties[i]);

                    if (propertyName.ToUpper().TrimEnd() != "TRANSACTIONTIMESTAMP" && propertyName.ToUpper().TrimEnd() != "")
                    {
                        if (existOjectobj.GetType().GetProperty(propertyName).PropertyType.ToString().ToUpper().IndexOf("Aria5SystemAdmin.Module".ToUpper()) > -1)
                        {
                            //XPClassInfo classInfo = existOjectobj.GetType().GetProperty(propertyName).PropertyType as XPClassInfo;
                            try
                            {
                                Guid tOid = Guid.Parse(singleModifiedObject.Values[i].ToString().TrimEnd());
                            }catch (Exception ex)
                            {

                              //  ex.Data. = "";

                            }
                            var propOjectobj = Session.DefaultSession.FindObject(existOjectobj.GetType().GetProperty(propertyName).PropertyType, CriteriaOperator.Parse("[Oid] ='" + Guid.Parse(singleModifiedObject.Values[i].ToString().TrimEnd()) + "'"));
                            if (propOjectobj != null)
                            { existOjectobj.SetMemberValue(propertyName, propOjectobj); }
                            else
                            { // cancel  and raise exception
                            }

                        }
                        else
                        {
                            if (singleModifiedObject.Values[i] is Byte [])
                            {
                              
                                Bitmap im = new  Bitmap(byteArrayToImage((byte[])singleModifiedObject.Values[i]));
                                im.Save("ImageName", System.Drawing.Imaging.ImageFormat.Bmp);
                                existOjectobj.SetMemberValue(propertyName, im);
                               
                            }
                            else
                            {
                                existOjectobj.SetMemberValue(propertyName, singleModifiedObject.Values[i]);

                            }
                        }


                    }
                }
            }
            return existOjectobj;
        }

        public string GetConvertTimeStamp(SyncDataObject singleModifiedObject, XPBaseObject existOjectobj, Session session)
        {
            string returnedstring = "";
            SelectedData timestamp = session.ExecuteQuery("select  MAX([dbo].[SyncView].[timestamp]) from   dbo.Syncview where GuidId ='" + singleModifiedObject.EntityOID + "'");
            if (timestamp.ResultSet != null)
            {
                foreach (SelectStatementResultRow row1 in timestamp.ResultSet[0].Rows)
                {
                    string s2 = BitConverter.ToString((byte[])(row1.Values[0]));
                    String[] tempAry = s2.Split('-');
                    string st = "0x";
                    foreach (string str in tempAry) st += str;
                    singleModifiedObject.Version = st;
                }
                returnedstring = singleModifiedObject.TableName.ToString() + "|" + singleModifiedObject.EntityOID.ToString() + "|" + singleModifiedObject.Version.ToString();
            }
            return returnedstring;
        }

        public void SwitchSchema(Guid AccountOid)
        {
            Session session = XpoDefault.Session;
            session.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
            if (session.IsConnected != true) { session.Connect(); }
            if (session.IsConnected == true)
            {
                Account account = session.FindObject<Account>(CriteriaOperator.Parse("[Oid] ='" +  AccountOid + "'"));
                if (account != null)
                {
                    string SchemaName = "";
                    if (string.IsNullOrWhiteSpace(account.AccountCode))
                    {
                        SchemaName = account.AccountCode;
                    }
                    else
                    {
                        SchemaName = account.Id;
                    }
                    string strCommand = String.Format("IF (NOT EXISTS (SELECT * FROM sys.schemas WHERE name = '" + SchemaName + "')) BEGIN EXEC ('CREATE SCHEMA [" + SchemaName + "] AUTHORIZATION [dbo]') END", SchemaName, "dbo");

                    //  SqlConnection myConnection = new SqlConnection(@"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin;User ID=azuresqladmin;Password=aria_123");
                    SqlConnection myConnection = new SqlConnection(@"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123");

                    try
                    {
                        myConnection.Open();

                        // Authenticate user before any further processing 

                        SqlCommand command = new SqlCommand(strCommand, myConnection);
                        SqlDataReader reader = command.ExecuteReader();
                    }
                    catch (SqlException ex)
                    { }

                    XPDictionary xpDictionary = XpoTypesInfoHelper.GetXpoTypeInfoSource().XPDictionary;
                    List<string> clientTables = new List<string>();
                    foreach (var classes in xpDictionary.Classes)
                    {
                        dynamic typeInfo = classes;
                        var table1 = typeInfo.Table;
                        foreach (Attribute att in typeInfo.Attributes)
                        {
                            if (att is IsClient)
                            {
                                clientTables.Add(table1.Name);
                            }
                        }
                    }

                    foreach (var classes in xpDictionary.Classes)
                    {
                        dynamic typeInfo = classes;
                        var table1 = typeInfo.Table;

                        //var typeInfo = xpDictionary.GetClassInfo(obj);
                        bool isClient = false;
                        foreach (Attribute att in typeInfo.Attributes)
                        {
                            if (att is IsClient)
                            {
                                //clientTables.Add(table1.Name);
                                isClient = true;
                            }
                        }

                        if (isClient)
                        {
                            if (typeInfo.Table != null)
                            {
                                if (!typeInfo.Table.Name.StartsWith(SchemaName + "."))
                                {
                                    if (typeInfo.Table.Name.Contains("."))
                                    {
                                        typeInfo.Table.Name = SchemaName + "." + typeInfo.Table.Name.Split('.')[1];
                                    }
                                    else
                                    {
                                        typeInfo.Table.Name = SchemaName + "." + typeInfo.Table.Name;
                                    }

                                    string s = typeInfo.Table.Name;
                                }

                                foreach (var fk in typeInfo.Table.ForeignKeys)
                                {
                                    if (!string.IsNullOrEmpty(fk.PrimaryKeyTable) && clientTables.Contains(fk.PrimaryKeyTable))
                                    {
                                        if (!fk.PrimaryKeyTable.StartsWith(SchemaName + "."))
                                        {
                                            fk.PrimaryKeyTable = SchemaName + "." + fk.PrimaryKeyTable.Split('.')[1];
                                        }
                                    }
                                }
                            }
                        }

                    }
                }

            }

        }

        public Image byteArrayToImage(byte[] byteArrayIn)
        {
            using (MemoryStream mStream = new MemoryStream(byteArrayIn))
            {
                return Image.FromStream(mStream);
            }
        }
    }

}