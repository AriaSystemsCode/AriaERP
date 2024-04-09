using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using System.IO;
using System.Data.SqlClient;
using System.Data;

namespace Aria.RequestHandler.Troubleshooting
{
    public class CheckDatabaseVersion : Check
    {
        private string _databaseName;
        public string DatabaseName
        {
            get { return _databaseName; }
            set { _databaseName = value; }
        }

        private string _versionName;
        public string VersionName
        {
            get { return _versionName; }
            set { _versionName = value; }
        }

        private string _currentVersion;
        public string CurrentVersion
        {
            get { return _currentVersion; }
            set { _currentVersion = value; }
        }
        
        public CheckDatabaseVersion(string DatabaseName, string VersionName, string CurrentVersion)
        {
            _databaseName = DatabaseName;
            _versionName = VersionName;            
            _currentVersion = CurrentVersion;
            CheckAction = "Check Database "+VersionName+" of " + DatabaseName + " (Current Version is " + CurrentVersion + " )";
            CheckResult = CheckResult.Failed;
        }

        public override void GetCheckResult()
        {
            if (ParentCheck != null && ParentCheck.CheckResult == CheckResult.Failed)
            {
                CheckResult = CheckResult.Failed;
                CheckError = "Parent check failed";
                return;
            }


            string dbVersion = GetDatabaseVersion(AriaVars.ServerName, AriaVars.ServerLoginType, AriaVars.UserName, AriaVars.Password, DatabaseName, VersionName).ToString().Trim();
            if (dbVersion == CurrentVersion)
            {
                CheckResult = CheckResult.Succeed;
            }
            else
            {
                CheckResult = CheckResult.Failed;
                CheckError = VersionName + " of database " + DatabaseName + " is " + dbVersion + " while the Current " + VersionName + " is " + CurrentVersion;
            }            
        }

        public static int GetDatabaseVersion(string serverName, DBLoginTypes auth, string userName, string pwd, string database, string extPropName)
        {
            SqlConnection.ClearAllPools();

            SqlConnection sqlConnection;
            if (auth == DBLoginTypes.SqlServerAuthentication)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";Integrated Security=true");
            }

            sqlConnection.Open();

            SqlCommand s = new SqlCommand("select Value from sys.extended_properties WHERE [Class] = 0 AND [Name] = '" + extPropName + "'", sqlConnection);

            SqlDataAdapter sqlAdpater = new SqlDataAdapter(s);
            DataTable newTable = new DataTable();

            sqlAdpater.Fill(newTable);

            sqlConnection.Close();

            return newTable.Rows.Count > 0 ? Convert.ToInt32(newTable.Rows[0]["Value"].ToString()) : 0;
        }
    }
}
