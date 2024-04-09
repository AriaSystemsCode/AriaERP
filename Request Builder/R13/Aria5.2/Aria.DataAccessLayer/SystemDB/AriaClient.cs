using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Data.SqlClient;
using System.Data;
using System.IO;
using Aria.DataAccessLayer.SystemDB.SystemMasterDataSetTableAdapters;
using Aria.DataAccessLayer.SystemDB;

namespace Aria.DataAccessLayer.SystemDB
{
    /// <summary>
    /// Used to Mange(<c>Insert,Update,Delete and retrieve</c>) Client table
    /// </summary>
    public class AriaClient
    {
        private CLIENTSTableAdapter _clientsAdapter = null;

        public CLIENTSTableAdapter ClientsAdapter
        {
            get
            {
                if (_clientsAdapter == null)
                    _clientsAdapter = new CLIENTSTableAdapter();
                return _clientsAdapter;
            }
        }

        /// <summary>
        /// Get all records from Clients Tabel.
        /// </summary>
        /// <returns>Returns an Strong type DataTable for the type-<a>SystemMasterDataSet.CLIENTSDataTable</a></returns>
        public SystemMasterDataSet.CLIENTSDataTable GetClients()
        {
            return ClientsAdapter.GetData();
        }

        /// <summary>
        /// Get all records from Clients Tabel and return string as XML format.
        /// </summary>
        /// <example>
        /// <code>
        /// using Aria.DataAccessLayer.SystemDB;
        /// ....
        /// AriaClient ariaClient = new AriaClient();
        /// string lcXML = ariaClient.GetClientsAsArray();
        /// ....
        /// //To run this code from Fox and open string as cursor
        /// ariaClient = CreateObject("Aria.DataAccessLayer.SystemDB.AriaClient")
        /// lcXML = ariaClient.GetClientsAsArray()
        /// XMLTOCURSOR(STRCONV(lcXML,11),"curCustomerList",512)
        /// </code>
        /// </example>
        /// <returns>Returns an string as formating XML.</returns>
        public string GetClientsAsXMLString()
        {
            SystemMasterDataSet.CLIENTSDataTable ClientsTable = ClientsAdapter.GetData();
            StringWriter SW = new StringWriter();
            ClientsTable.WriteXml(SW, XmlWriteMode.WriteSchema);
            return SW.ToString();
        }

        /// <summary>
        /// Get clients records from Clients Tabel depend on CClientID.
        /// </summary>
        /// <param name="CClientID">Client ID refare to CClientID Field in Database</param>
        /// <returns>Returns an string as formating XML.</returns>
        public SystemMasterDataSet.CLIENTSDataTable GetClientsByCClientID(string CClientID)
        {
            return ClientsAdapter.GetDataByCCLIENTID(CClientID);
        }

        /// <summary>
        /// Get clients records from Clients Tabel and return string as XML format depend on CClientID.
        /// </summary>
        /// <example>
        /// <code>
        /// using Aria.DataAccessLayer.SystemDB;
        /// ....
        /// AriaClient ariaClient = new AriaClient();
        /// string lcXML = ariaClient.GetClientsByCClientIDAsXMLString("BRI10");
        /// ....
        /// //To run this code from Fox and open string as cursor
        /// ariaClient = CreateObject("Aria.DataAccessLayer.SystemDB.AriaClient")
        /// lcXML = ariaClient.GetClientsByCClientIDAsXMLString("BRI10")
        /// XMLTOCURSOR(STRCONV(lcXML,11),"curCustomerList",512)
        /// </code>
        /// </example>
        /// <param name="CClientID">Client ID refare to CClientID Field in Database.</param>
        /// <returns>Returns an string as formating XML.</returns>
        public string GetClientsByCClientIDAsXMLString(string CClientID)
        {
            SystemMasterDataSet.CLIENTSDataTable ClientsTable = ClientsAdapter.GetDataByCCLIENTID(CClientID);
            StringWriter SW = new StringWriter();
            ClientsTable.WriteXml(SW, XmlWriteMode.WriteSchema);
            return SW.ToString();
        }

        /// <summary>
        /// Add new Client on CLIENTS tabel on DB.
        /// </summary>
        /// <param name="ClientRow"></param>
        /// <returns></returns>
        public bool AddNewClient(SystemMasterDataSet.CLIENTSRow ClientRow)
        {
            SystemMasterDataSet.CLIENTSDataTable ClientTable = new SystemMasterDataSet.CLIENTSDataTable();
            SystemMasterDataSet.CLIENTSRow CRow = ClientTable.NewCLIENTSRow();

            CRow.CCLIENTID = ClientRow.CCLIENTID;
            CRow.CCLIENTNAME = ClientRow.CCLIENTNAME;
            CRow.CCONDBNAME = ClientRow.CCONDBNAME;
            CRow.CCONPASWRD = ClientRow.CCONPASWRD;
            CRow.CCONSERVER = ClientRow.CCONSERVER;
            CRow.CCONUSERID = ClientRow.CCONUSERID;
            CRow.CDATAPATH = ClientRow.CDATAPATH;
            CRow.CINSDFCOM = ClientRow.CINSDFCOM;
            CRow.LLOCKSYS = ClientRow.LLOCKSYS;

            ClientTable.AddCLIENTSRow(CRow);
            int SucessUpdate = ClientsAdapter.Update(ClientTable);
            if (SucessUpdate > 0)
                return true;
            else
                return false;
        }

        /// <summary>
        /// Used to update Data into Clients Table
        /// <c>Note: Must be add value in all fields is not null as CCLIENTID,CDATAPATH and LLOCKSYS</c>
        /// </summary>
        /// <example>
        /// <code>
        /// using Aria.DataAccessLayer.SystemDB;
        /// ....
        /// //This way to fill object by old data of specific Client.
        /// AriaClient ariaClient = new AriaClient();
        /// SystemMasterDataSet.CLIENTSDataTable ClientTable = ariaClient.GetClientsByCClientID("BRI10");
        /// SystemMasterDataSet.CLIENTSRow =  ClientTable[0];
        /// //To Update any value
        /// CLIENTSRow.CCONDBNAME="New value Updated";
        /// bool Success = ariaClient.UpdateClient(CLIENTSRow);
        /// </code>
        /// </example>
        /// <param name="ClientRow">Client ID</param>
        /// <returns>Returns True if Success Update.</returns>
        public bool UpdateClient(SystemMasterDataSet.CLIENTSRow ClientRow)
        {
            SystemMasterDataSet.CLIENTSDataTable ClientTable = GetClientsByCClientID(ClientRow.CCLIENTID);
            SystemMasterDataSet.CLIENTSRow CRow = ClientTable[0];

            CRow.CCLIENTID = ClientRow.CCLIENTID;
            if (!ClientRow.IsCCLIENTNAMENull())
                CRow.CCLIENTNAME = ClientRow.CCLIENTNAME;
            if (!ClientRow.IsCCONDBNAMENull())
                CRow.CCONDBNAME = ClientRow.CCONDBNAME;
            if (!ClientRow.IsCCONPASWRDNull())
                CRow.CCONPASWRD = ClientRow.CCONPASWRD;
            if (!ClientRow.IsCCONSERVERNull())
                CRow.CCONSERVER = ClientRow.CCONSERVER;
            if (!ClientRow.IsCCONUSERIDNull())
                CRow.CCONUSERID = ClientRow.CCONUSERID;
            if (ClientRow.CDATAPATH == null)
                CRow.CDATAPATH = ClientRow.CDATAPATH;
            if (!ClientRow.IsCINSDFCOMNull())
                CRow.CINSDFCOM = ClientRow.CINSDFCOM;
            if (ClientRow.LLOCKSYS == null)
                CRow.LLOCKSYS = ClientRow.LLOCKSYS;

            int SucessUpdate = ClientsAdapter.Update(ClientRow);
            if (SucessUpdate > 0)
                return true;
            else
                return false;
        }

        /// <summary>
        /// Used to delete row by CCLIENTID from CLIENTS Table.
        /// </summary>
        /// <param name="CCleintID">CCLIENTID field as BRI10</param>
        /// <returns>True if delete Success</returns>
        public bool DeleteClient(string CCleintID)
        {
            int rowsAffected = ClientsAdapter.Delete(GetClientsByCClientID(CCleintID)[0].ID);
            if (rowsAffected > 0)
                return true;
            else
                return false;
        }
    }
}
