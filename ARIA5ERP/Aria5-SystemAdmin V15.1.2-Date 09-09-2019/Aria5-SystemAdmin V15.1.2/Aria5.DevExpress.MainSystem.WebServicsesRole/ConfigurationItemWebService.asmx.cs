using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Services;

namespace Aria5.DevExpress.MainSystem.WebServicsesRole
{

    /// <summary>
    /// Summary description for AccountWebService
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    // [System.Web.Script.Services.ScriptService]
    public class ConfigurationItem : System.Web.Services.WebService
    {

        [WebMethod(EnableSession = true)]
        public Information_about_running_application_on_specific_device GetInformation(String AccountID, String ApplicationID, String DeviceSignature)
        {
            Information_about_running_application_on_specific_device information_about_running_application_on_specific_device = new Information_about_running_application_on_specific_device();
            if (Login.GetAuthenticateStatus() == true)
            {
                //- If Athunticate then, Calls the Device Manager entity to return the device ID corresponding to the sent device Signature.
                //- If Athunticate then, Searches the ConfigurationItem table for the account, device and application. 

                //information_about_running_application_on_specific_device.VersionType = ;
                //information_about_running_application_on_specific_device.Startdate = ;
                //information_about_running_application_on_specific_device.ExpirationDate = ;
                return information_about_running_application_on_specific_device;
            }
            else
            {
                return null;
            }


        }


        [WebMethod(EnableSession = true)]
        public String GetActiviationFile(Guid Account, Guid Device, Guid Application)
        {

            if (Login.GetAuthenticateStatus() == true)
            {
                string ActivationKeyID;
                //- If Athunticate then, Searches the configuration item table for the received parameters and get the related Activation Key OID
                //- If Athunticate then, Calls the Activation entity, pass the activation key OID and get the activation key file
                //- If Athunticate then, Return the Activation Key file string
                return ActivationKeyID;
            }
            else
            {
                return null;
            }


        }

        [WebMethod(EnableSession = true)]
        public bool CheckActiviationFile(Guid Account, Guid Device, Guid Application, string ActivationKeyID)
        {

            if (Login.GetAuthenticateStatus() == true)
            {
                bool ActivationKeyMatched;
                // - If Athunticate then, Searches the configuration item table for the received Account, Device, Application and get the related Activation Key ID.
                //- If Athunticate then, Compare this ID with the activation key ID for the configuration item for this account\device\application.
                //- If Athunticate then, Return true of the two IDs are matched and false if they are different

                return ActivationKeyMatched;
            }
            else
            {
                return false;
            }


        }


        [WebMethod(EnableSession = true)]
        public AddingConfigurationItemInformation AddNewConfigurationItem(String ConfigurationItemDescription, Guid AccountOID, Guid ContactOID, Guid DeviceOID,
                                                                         Guid ApplicationOID, String InstallationType, String VersionType, DateTime StartDate, DateTime EndDate)
        {

            AddingConfigurationItemInformation addingConfigurationItemInformation = new AddingConfigurationItemInformation();

            if (Login.GetAuthenticateStatus() == true)
            {
                // prepare    
                XpoDefault.ConnectionString = "Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin;uid=azuresqladmin;pwd=aria_123;Pooling=False";
                Session session = new Session();
                Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem configurationItem = new Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem(session);

                //- If Athunticate then, Calls the Identifier Code Structure entity to use the current identifier structure for the Configuration Item entity and generate ID for the newly created configuration item.
                //look
                configurationItem.Id = IdentifierCodeStructureEntity();
                //- If Athunticate then, Search the Contact table for the sent account OID, get the related account ID and account name and update the Configuration Item table with the account id and name.

                Aria5SystemAdmin.Module.BusinessObjects.Contact account = session.FindObject<Contact>(CriteriaOperator.Parse("[OID] = '" + AccountOID + "'"));

                //- If Athunticate then, Search the Contact table for the sent account contact OID, get the related contact name and update the Configuration Item table with the contact name.

                //- If Athunticate then, Search the AccountDevice table for the sent device OID, get the related device signature and device name and update the Configuration Item table with the device
                //   signature and device name.
                //- If Athunticate then, Search the Application table for the sent application OID, get the related application ID and application name and update the Configuration Item table with
                //   the application ID and application name.
                //- If Athunticate then, Return the adding new Configuration Item status of {Added Successfully or Failed}.

                return addingConfigurationItemInformation;
            }
            else
            {
                return null;
            }

        }





    }


    [Serializable]
    public class Information_about_running_application_on_specific_device
    {
        string versionType;

        public string VersionType
        {
            get { return versionType; }
            set { versionType = value; }
        }
        DateTime startdate;

        public DateTime Startdate
        {
            get { return startdate; }
            set { startdate = value; }
        }
        DateTime expirationDate;

        public DateTime ExpirationDate
        {
            get { return expirationDate; }
            set { expirationDate = value; }
        }


    }

    [Serializable]
    public class DataError
    {
        string columnName;

        public string ColumnName
        {
            get { return columnName; }
            set { columnName = value; }
        }

        string errorDescription;

        public string ErrorDescription
        {
            get { return errorDescription; }
            set { errorDescription = value; }
        }

    }

    [Serializable]
    public class AddingConfigurationItemInformation
    {
        List<DataError> dataErrorList;

        public List<DataError> DataErrorList
        {
            get { return dataErrorList; }
            set { dataErrorList = value; }
        }

        string addingStatus;

        public string AddingStatus
        {
            get { return addingStatus; }
            set { addingStatus = value; }
        }

        string itemID;

        public string ItemID
        {
            get { return itemID; }
            set { itemID = value; }
        }


    }

    public static class Login
    {
        public static bool GetAuthenticateStatus()
        {  // look add the session check here

            return true;
        }

        static string connectionString = "Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin;uid=azuresqladmin;pwd=aria_123;Pooling=False";

        public static string ConnectionString
        {
            get { return connectionString; }
            set { connectionString = value; }
        }

    }


}
