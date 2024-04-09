using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.Environment;
using Aria.DataTypes.ObjectDictionary;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.Xml;
using System.Text.RegularExpressions;
using System.Data;
using Aria.DataTypes.Settings;
using Aria.DataTypes;
using System.Data.Odbc;
using System.Xml;
using System.Collections;
using Aria.Utilities.Aria40Converter.Helpers;
using System.Windows.Forms;
using Aria.EnterpriseServices.RequestHandler;

namespace Aria.Utilities.Aria40Converter.WhiteTesting
{
    public class AriaRequestAgentWhiteTest
    {
        public DataTable TestAriaRequestAgent(string clientID, string companyID)
        {
            AriaObjectDictionaryDBCentric dbCentric = new AriaObjectDictionaryDBCentric();
            ArrayList dataObjectList = dbCentric.LoadAriaObjectChildObjectsOfType(new AriaDbConnection(), "Aria4XP", AriaObjectTypes.Data, "");

            DataTable table = new DataTable();

            table.Columns.Add("ObjectName");
            table.Columns.Add("Result");
            table.Columns.Add("NumberOfRowsReturn");
            table.Columns.Add("ErrorMessage");

            DataSet ds = new DataSet();
            ds.Tables.Add(table);

            foreach (AriaObject ariaObject in dataObjectList)
            {
                AriaRequestAgent agent = new AriaRequestAgent();

                ArrayList dataPointers = new ArrayList();

                try
                {
                    AriaDbConnection connection = new AriaDbConnection();

                    connection.CompanyName = companyID;

                    dataPointers = agent.GetAgentRequests(connection, ariaObject.ObjectName.Trim(), new AriaConditionList(), clientID);
                    table.Rows.Add(ariaObject.ObjectName, "True", dataPointers.Count.ToString(), "");
                }
                catch (Exception e)
                {
                    table.Rows.Add(ariaObject.ObjectName, "False", dataPointers.Count.ToString(), e.Message);
                }
            }

            return table;
        }
    }
}
