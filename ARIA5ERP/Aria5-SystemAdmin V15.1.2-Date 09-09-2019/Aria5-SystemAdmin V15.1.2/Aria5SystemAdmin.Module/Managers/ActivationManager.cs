using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
//using Aria5.DevExpress.SystemAdmin.WebServiceRole;
using DevExpress.Xpo;
using Aria5SystemAdmin.Module.BusinessObjects;
using DevExpress.Data.Filtering;
using System.Text;
using System.Security.Cryptography;
using System.Xml.Serialization;
using System.IO;
using System.Xml;
using Aria5SystemAdmin.Module.DataTypes;
using Aria5.DevExpress.MainSystem.Module.Managers;
using DevExpress.ExpressApp;


namespace Aria5SystemAdmin.Module.Managers
{
    public class Utf8StringWriter : StringWriter
    {
        public override Encoding Encoding
        {
            get { return Encoding.UTF8; }
        }
    }
    public class ActivationManager
    {

        IObjectSpace _session;

        public ActivationManager(IObjectSpace session)
        {
            _session = session;
        }

        public ActivationManager()
        {
            // TODO: Complete member initialization
        }


        //public void Start
        //()
        //{
        //    XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=SystemAdminTestingUser1;Password=aria_123";

        //    Account temp = new Account(XpoDefault.Session);

        //    string schemaName = "DBO";

        //    foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
        //    {
        //        Type[] types = null;

        //        try
        //        {
        //            types = assembly.GetTypes();
        //        }
        //        catch (Exception)
        //        {
        //        }

        //        if (types != null) foreach (var type in types)
        //            {
        //                if (type == typeof(XPObjectType) || type.IsSubclassOf(typeof(XPBaseObject)))
        //                {
        //                    var typeInfo = XpoDefault.Session.DataLayer.Dictionary.GetClassInfo(type);

        //                    if (typeInfo.Table != null)
        //                    {
        //                        if (!typeInfo.Table.Name.StartsWith(schemaName + "."))
        //                        {
        //                            typeInfo.Table.Name = schemaName + "." + typeInfo.Table.Name;
        //                        }

        //                        foreach (var fk in typeInfo.Table.ForeignKeys)
        //                        {
        //                            if (!fk.PrimaryKeyTable.StartsWith(schemaName + "."))
        //                            {
        //                                fk.PrimaryKeyTable = schemaName + "." + fk.PrimaryKeyTable;
        //                            }
        //                        }
        //                    }
        //                }
        //            }
        //    }

        //}

        private static string GetSHA512(string text)
        {
            byte[] hashValue;
            byte[] message = GetBytes(text);

            SHA512Managed hashString = new SHA512Managed();
            string hex = "";

            hashValue = hashString.ComputeHash(message);
            foreach (byte x in hashValue)
            {
                hex += String.Format("{0:x2}", x);
            }
            return hex;
        }

        static byte[] GetBytes(string str)
        {
            byte[] bytes = new byte[str.Length];

            for (int i = 0; i < str.Length; i++) bytes[i] = (byte)str[i];

            return bytes;
        }

        public string ToXML(ActivationKey activationKeyBO)
        {
            if (activationKeyBO != null)
            {

                ActivationDataType activationMarshalling = new ActivationDataType();

                activationMarshalling.ActivationKeyID = activationKeyBO.ID;
                activationMarshalling.AccountCode = activationKeyBO.AccountId;
                activationMarshalling.AccountName = activationKeyBO.AccountName;
               activationMarshalling.Application = activationKeyBO.Application.Oid.ToString();

                activationMarshalling.InstallationType = activationKeyBO.InstallationType.ToString();
                activationMarshalling.VersionType = activationKeyBO.VersionType.ToString();

                activationMarshalling.ExpireDate = activationKeyBO.EndDate;

                activationMarshalling.NumberOfUsers = activationKeyBO.NumberOfUsers;
                activationMarshalling.DeviceName = activationKeyBO.DeviceName;
                activationMarshalling.DeviceSignature = activationKeyBO.DeviceSignature;
                activationMarshalling.DigitalSignature = activationKeyBO.DigitalSignature;

                var serializer = new XmlSerializer(typeof(ActivationDataType));
                string utf8; 

                using (StringWriter writer = new Utf8StringWriter())
                {
                    serializer.Serialize(writer, activationMarshalling);
                    utf8 = writer.ToString();
                    return utf8;
                }

                //var stringwriter = new System.IO.StringWriter();
                //var serializer = new XmlSerializer(typeof(ActivationDataType));


                //serializer.Serialize(stringwriter, activationMarshalling);
                //return stringwriter.ToString();
            }
            else
                return string.Empty;

        }

        private string PrepareToHash(string hash)
        {
            char[] charArray = hash.ToCharArray();
            Array.Reverse(charArray);

            for (int i = 0; i < charArray.Length-1; i++)
            {
                char temp = charArray[i + 1];
                charArray[i + 1] = charArray[i];
                charArray[i] = temp;
                
            }
                return new string(charArray);

        }


        public ActivationKey GenerateNewActivationKey(Guid configrationItemOid)
        {
           
           // StartConnection();
            ActivationKey activationKeyBO;
            if (_session == null)
            {
                // XPO
                Session session = XpoDefault.Session;
                activationKeyBO = new ActivationKey(session);
            }
            else
            {
                // XAF
                activationKeyBO = (ActivationKey)_session.CreateObject(typeof(ActivationKey));
            }


            ActivationDataType activationMarshalling = new ActivationDataType();

            if (configrationItemOid != Guid.Empty)
            {


                //ConfigurationItem configurationItemObj = session.FindObject<ConfigurationItem>(CriteriaOperator.Parse("[Oid] = '" + configrationItemOid + "'"));
                ConfigurationItem configurationItemObj = activationKeyBO.Session.FindObject<ConfigurationItem>(CriteriaOperator.Parse("[Oid] = '" + configrationItemOid + "'"));

                if (configurationItemObj != null)
                {
                   // string configureType = 
                   // string configureVer = 
                    activationKeyBO.ID = IdentifierStructureStub();


                    activationKeyBO.ConfigurationItem = configurationItemObj;

                    activationKeyBO.GenerateDate = DateTime.Now;

                    if (configurationItemObj.Account != null)
                    {
                        activationKeyBO.AccountId = configurationItemObj.Account.Id;
                        activationKeyBO.AccountName = configurationItemObj.Account.Name;
                    }
                    else
                    {
                        //Account account = activationKeyBO.Session.FindObject<Account>(CriteriaOperator.Parse("[Id] = '" + configurationItemObj.AccountId + "'"));
                        //activationKeyBO.AccountId = account.Id;
                        //activationKeyBO.AccountName = account.Name;
                        //activationKeyBO.Account = account;
                        throw new Exception(" Account can't be empty while creating activation key ");
                    }

                    activationKeyBO.Device = configurationItemObj.Device;
                    if(configurationItemObj.Device != null) activationKeyBO.DeviceName = configurationItemObj.Device.Name;
                    activationKeyBO.DeviceSignature = configurationItemObj.DeviceSignature;

                    activationKeyBO.Application = configurationItemObj.Application;

                    activationKeyBO.ApplicationId = configurationItemObj.Application.Id;
                    activationKeyBO.ApplicationName = configurationItemObj.Application.Name;

                    activationKeyBO.InstallationType = configurationItemObj.InstallationType; 
                    activationKeyBO.VersionType = configurationItemObj.VersionType;
                    activationKeyBO.NumberOfUsers = configurationItemObj.NumberOfUsers;
                    activationKeyBO.StartDate = configurationItemObj.StartDate;
                    activationKeyBO.EndDate = configurationItemObj.EndDate;

                    //sara.N 3/2/2015 [Start]
                    activationKeyBO.Description = configurationItemObj.Description;
                    activationKeyBO.Status = configurationItemObj.Status;
                    
                    activationKeyBO.NumberOfUsers = configurationItemObj.NumberOfUsers;
                    activationKeyBO.ReferenceNumber = configurationItemObj.ReferenceNumber;
                    activationKeyBO.ReferenceTitle = configurationItemObj.ReferenceTitle;
                    activationKeyBO.SerialNumber = configurationItemObj.SerialNumber;
                    activationKeyBO.SerialNo = configurationItemObj.SerialNo;
                    activationKeyBO.ServiceOrBundle = configurationItemObj.ServiceOrBundle;
                    activationKeyBO.ContactName = activationKeyBO.ContactName;
                    activationKeyBO.DeviceSignature = activationKeyBO.DeviceSignature;
                    //sara.N 3/2/2015 [End]
                    string hashValues = activationKeyBO.AccountName + " - " + activationKeyBO.EndDate.Day.ToString() + " - " + activationKeyBO.EndDate.Month.ToString() + " - " + activationKeyBO.EndDate.Year.ToString() + " - " + activationKeyBO.VersionType.ToString();

                    string digitalSignature = GetSHA512(PrepareToHash(hashValues));

                    activationKeyBO.DigitalSignature = digitalSignature;

                    activationKeyBO.ActivationKeyFile = ToXML(activationKeyBO);

                    activationKeyBO.Save();
                    try
                    {
                        activationKeyBO.Session.CommitTransaction();
                    }
                    catch (Exception ex)
                    {
 
                    }
                    configurationItemObj.ActivationKeyID = activationKeyBO.ID;
                    configurationItemObj.Save();


                }

                else
                    throw new Exception("|This Item was not Found|");
            }

            else
                throw new Exception("|Invalid Guid|");

            return activationKeyBO;

            
        }

        public bool IsActivationKeyFound(Guid configrationItemOid)
        {
           // StartConnection();
            Session session = XpoDefault.Session;
            ActivationKey activationKey = session.FindObject<ActivationKey>(CriteriaOperator.Parse("[ConfigurationItem] = '" + configrationItemOid + "'"));

            if (activationKey.EndDate > DateTime.Now)
            {
                return true;
            }

            else
                return false;
        }

        public ActivationKey GetCurrentActivationKeyInfo(Guid configrationItemOid)
        {
           // StartConnection();
            Session session = XpoDefault.Session;
            ActivationKey activationKey = session.FindObject<ActivationKey>(CriteriaOperator.Parse("[ConfigurationItem] = '" + configrationItemOid + "'"));

            if (activationKey.EndDate > DateTime.Now)
            {
                 return activationKey;
            }

            else
                throw new Exception("|This Activation Key Is Not Valid|");
        }

        public ActivationKey GetCurrentActivationKeyInfoByApp(Guid applicationOId, Guid accountOid, string deviceSignature)
        {
            Session session = XpoDefault.Session;


            Application_T application = session.FindObject<Application_T>(CriteriaOperator.Parse("[Oid] = '" + applicationOId + "'"));

            AccountDevice device = session.FindObject<AccountDevice>(CriteriaOperator.Parse("[Account] = '" + accountOid + "' and [DeviceSignature] ='" + deviceSignature + "'"));

            ConfigurationItem configurationItem = session.FindObject<ConfigurationItem>(CriteriaOperator.Parse("[Device] = '" + device.Oid + "' and [ApplicationId] = '" + application.Id + "'"));

            ActivationKey activationKey = session.FindObject<ActivationKey>(CriteriaOperator.Parse(" [ID] ='" + configurationItem.ActivationKeyID + "'"));

            if (activationKey.EndDate > DateTime.Now)
            {
                return activationKey;
            }

            else
                throw new Exception("|This Activation Key Is Not Valid|");
        }

        public ActivationKey GetCurrentActivationKeyId(string iD)
        {
            // StartConnection();
            Session session = XpoDefault.Session;
            ActivationKey activationKey = session.FindObject<ActivationKey>(CriteriaOperator.Parse("[ID] = '" + iD + "'"));

            if (activationKey.EndDate > DateTime.Now)
            {
                return activationKey;
            }

            else
                throw new Exception("|This Activation Key Is Not Valid|");
        }


        #region STUB

        public string IdentifierStructureStub()
        {
            List<string> guidList = new List<string>();
            string returnValue = "";
            string nextId = "";
            Dictionary<int, object> seg = new Dictionary<int, object>();

            if (_session == null)
            {
                guidList = IdentifierStructureManager.GetAllIdentifierStructures();
            }
            else
            {
               
                guidList = IdentifierStructureManager.GetAllIdentifierStructures(_session);
            }

            nextId = guidList.FirstOrDefault().ToString();

            if (_session == null)
            {
                returnValue = IdentifierStructureManager.GetNextId(nextId, seg);
            }
            else
            {
                returnValue = IdentifierStructureManager.GetNextId(nextId, seg, _session);
            }

            return returnValue;
        #endregion
        }


        //Sara [Begin]  
        public static bool UnRegisterActivationKeys(ConfigurationItem configuration)
        {
            bool returnvalue = false;

            Session session = XpoDefault.Session;
           

            XPCollection<ActivationKey> activationKeys = new XPCollection<ActivationKey>(session, CriteriaOperator.Parse("[ConfigurationItem] = '" + configuration.Oid + "'"));
            activationKeys.Load();
            foreach (ActivationKey activationKey in activationKeys)
            {
                UnRegisterActivationKey(activationKey);
            }

            returnvalue = true;
            return returnvalue;

        }

        public static bool UnRegisterActivationKey(ActivationKey activationKey)
        {
            bool returnvalue = false;

            Session session = XpoDefault.Session;


            activationKey.Delete();
            returnvalue = true;

            return returnvalue;

        }
        //Sara [End]  


        
    }
}
