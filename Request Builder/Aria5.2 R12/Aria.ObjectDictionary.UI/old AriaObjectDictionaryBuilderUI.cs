using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Aria.Utilities.Aria40Converter.AriaDictionaryTree;
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using Aria.Utilities.Aria40Converter.Properties;
using Aria.Data;
using Aria.Utilities.Aria40Converter;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.DataTypes.ObjectDictionary;
using Aria.Environment;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.Xml;
using System.Text.RegularExpressions;
using System.Data;
using Aria.DataTypes.Settings;
using Aria.DataTypes;
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using Aria.Utilities.Aria40Converter.Helpers;
using System.Data.Odbc;

namespace Aria.Utilities.Aria40Converter
{
    /// <summary>
    /// This is windows form to enable used from build Object dictionary
    /// </summary>
    public partial class AriaObjectDictionaryBuilderUI : Form
    {
        public AriaObjectDictionaryBuilderUI()
        {
            InitializeComponent();
        }

        private void convertToolStripMenuItem_Click(object sender, EventArgs e)
        {

            AriaDbCommand command7 = new AriaDbCommand("DELETE  FROM AriaObjectMethodParameter where ObjectID IN(select objectID from AriaObject where ObjectName like 'Aria4Xp%')", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command7.ExecuteNonQuery();

            AriaDbCommand command6 = new AriaDbCommand("DELETE  FROM AriaObjectMethod  where ObjectID IN(select objectID from AriaObject where ObjectName like 'Aria4Xp%')", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command6.ExecuteNonQuery();

            AriaDbCommand command5 = new AriaDbCommand("DELETE  FROM AriaObjectEventParameter  where ObjectID IN(select objectID from AriaObject where ObjectName like 'Aria4Xp%')", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command5.ExecuteNonQuery();

            AriaDbCommand command4 = new AriaDbCommand("DELETE  FROM AriaObjectEvent  where ObjectID IN(select objectID from AriaObject where ObjectName like 'Aria4Xp%')", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command4.ExecuteNonQuery();

            AriaDbCommand command3 = new AriaDbCommand("DELETE  FROM AriaObjectProperty  where ObjectID IN(select objectID from AriaObject where ObjectName like 'Aria4Xp%')", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command3.ExecuteNonQuery();

            AriaDbCommand command2 = new AriaDbCommand("DELETE  FROM AriaObjectRevision  where ObjectID IN(select objectID from AriaObject where ObjectName like 'Aria4Xp%')", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command2.ExecuteNonQuery();

            AriaDbCommand command1 = new AriaDbCommand("DELETE  FROM AriaObject where ObjectName like 'Aria4Xp%'", new AriaDbConnection("", ""), Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command1.ExecuteNonQuery();



            Aria4XPSchemaInformation schema = new Aria4XPSchemaInformation(Settings.Default.Aria4XPConnectionString, Settings.Default.Aria27ConnectionString, Settings.Default.Aria27MergePath, Settings.Default.Aria4XpMergePath,Settings.Default.Aria5MergePath);
            AriaPackageObjects root = new AriaPackageObjects(schema);
            root.CreateChildren();
            root.Save();
        }

        private void showDictionaryToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ObjectDictionaryViewer viewer = new ObjectDictionaryViewer();
            viewer.ShowDialog();
        }

        private void AriaObjectDictionaryBuilderUI_Load(object sender, EventArgs e)
        {

        }

        private void Fill_sydobjct(string Path)
        {
            OdbcCommand command = new OdbcCommand();
            command.CommandText = "SELECT * FROM sydobjct";
            command.Connection = new OdbcConnection(Settings.Default.Aria4XPConnectionString);
            command.Connection.Open();

            OdbcDataAdapter adapter = new OdbcDataAdapter(command);
            DataTable table = new DataTable();

            adapter.Fill(table);

            command.Connection.Close();

            DataTable table2 = new DataTable();
            DataSet dt = new DataSet();
            dt.ReadXml(Path);
            table2 = dt.Tables[0];
            foreach (DataRow row in table.Rows)
            {
                if (!string.IsNullOrEmpty(row["CARIA5ID"].ToString()))
                {
                    if (table2.Select("ObjectID = '" + row["CARIA5ID"] + "'").Count() > 0)
                    {
                        OdbcCommand command1 = new OdbcCommand();
                        command1.CommandText = "UPDATE sydobjct SET A5OBJNAM = '" + table2.Select("ObjectID = '" + row["CARIA5ID"] + "'").First()["ObjectName"].ToString().Trim() + "' WHERE CARIA5ID= '" + row["CARIA5ID"] + "'";
                        command1.Connection = new OdbcConnection(Settings.Default.Aria4XPConnectionString);
                        command1.Connection.Open();
                        command1.ExecuteNonQuery();
                        command1.Connection.Close();
                    }
                }
            }


        }

        private void agentSalesOrderAllToolStripMenuItem_Click(object sender, EventArgs e)
        {
            AriaConditionList conditions = new AriaConditionList();

            AriaCondition condition = new AriaCondition();

            condition.LeftHandSide = new AriaStandardDataType();
            condition.LeftHandSide.PropertyDataPathDictionary.Add("Value", "SalesOrderType");
            condition.Operator = AriaConditionOperators.Like;
            condition.IsOperator = true;
            condition.RightHandSide = new AriaStandardDataType();
            ((AriaStandardDataType)condition.RightHandSide).Value = "O";

            conditions.Items.Add(condition);

            Aria.EnterpriseServices.RequestHandler.AriaRequestAgent agent = new Aria.EnterpriseServices.RequestHandler.AriaRequestAgent();
            AriaDbConnection connection = new AriaDbConnection();
            connection.CompanyName = "01";
            ArrayList dataPointers = agent.GetAgentRequests(connection, "Aria4XP.SalesOrderHeader", conditions, "BRI10");
        }

        private void pointerToolStripMenuItem_Click(object sender, EventArgs e)
        {
            AriaDbConnection connection = new AriaDbConnection();
            connection.CompanyName = "01";

            Aria.DataTypes.AriaDataObjectPointer pointer = new AriaDataObjectPointer();
            pointer.AddKeyField("CORDTYPE", "O");
            pointer.AddKeyField("ORDER", "030042");

            Aria.DataTypes.Settings.AriaDataObjectPointerSettings pointerSetting = new AriaDataObjectPointerSettings();
            pointerSetting.DataObjectName = "ARIA4XP.SalesOrderHeader";
            pointerSetting.DataObjectRevision = "001.000";


            Aria.Data.DataTypes.AriaDataObjectPointerAdapter x = new Aria.Data.DataTypes.AriaDataObjectPointerAdapter(connection, pointerSetting, pointer, "BRI10");

            MessageBox.Show(x.GetData("CompleteDate", "BRI10").ToString());
            MessageBox.Show(x.GetData("DaysToCompleteDate", "BRI10").ToString());
        }
    }
}