using System;
using System.Collections.Generic;
using System.Text;
using System.Data.SqlClient;
using System.Data;
using System.IO;
using System.Windows.Forms;
using System.Text.RegularExpressions;
using System.Collections;
using System.Data.Sql;
using Microsoft.SqlServer.Management.Smo;
using Microsoft.SqlServer.Management.Common;

namespace Aria.Configuration.Server
{
    public class DatabaseUtilities
    {
        public static ArrayList GetServers()
        {
            DataTable r = SqlDataSourceEnumerator.Instance.GetDataSources();
            ArrayList result = new ArrayList();

            foreach (DataRow row in r.Rows)
            {
                result.Add(row["ServerName"].ToString() + (row["InstanceName"].ToString().Length > 0 ? @"\" + row["InstanceName"].ToString() : ""));
            }

            result.Sort();

            return result;
        }

        public static bool IsServerExists(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd, ref string error)
        {
            if (serverName.Trim() == "") return false;

            SqlConnection.ClearAllPools(); 

            try
            {
                SqlConnection sqlConnection;
                if (auth == DatabaseServerLoginTypes.SqlServerAuthentication)
                {
                    sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=Master;User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
                }
                else
                {
                    sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=Master;Integrated Security=true");
                }

                sqlConnection.Open();

                sqlConnection.Close();

                return true;
            }
            catch (Exception e)
            {
                error = e.GetBaseException().Message;
                return false;
            }
        }

        public static bool IsDatabaseExist(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd, string database)
        {
            SqlConnection.ClearAllPools(); 

            SqlConnection sqlConnection;
            if (auth == DatabaseServerLoginTypes.SqlServerAuthentication)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=Master;User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=Master;Integrated Security=true");
            }

            sqlConnection.Open();

            SqlCommand s = new SqlCommand("SELECT * FROM sysdatabases WHERE [Name] = '" + database + "'", sqlConnection);

            SqlDataAdapter sqlAdpater = new SqlDataAdapter(s);
            DataTable newTable = new DataTable();

            sqlAdpater.Fill(newTable);

            sqlConnection.Close();


            return newTable.Rows.Count > 0;
        }

        public static int GetDatabaseVersion(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd, string database)
        {
            SqlConnection.ClearAllPools();
            
            SqlConnection sqlConnection;
            if (auth == DatabaseServerLoginTypes.SqlServerAuthentication)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";Integrated Security=true");
            }

            sqlConnection.Open();

            SqlCommand s = new SqlCommand("select Value from sys.extended_properties WHERE [Class] = 0 AND [Name] = 'Version'", sqlConnection);

            SqlDataAdapter sqlAdpater = new SqlDataAdapter(s);
            DataTable newTable = new DataTable();

            sqlAdpater.Fill(newTable);

            sqlConnection.Close();

            return newTable.Rows.Count > 0 ? Convert.ToInt32(newTable.Rows[0]["Value"].ToString()) : 0;
        }

        public static int GetDatabaseCustomizationVersion(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd, string database)
        {
            SqlConnection.ClearAllPools();

            SqlConnection sqlConnection;
            if (auth == DatabaseServerLoginTypes.SqlServerAuthentication)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";Integrated Security=true");
            }

            sqlConnection.Open();

            SqlCommand s = new SqlCommand("select Value from sys.extended_properties WHERE [Class] = 0 AND [Name] = 'CustomizationVersion'", sqlConnection);

            SqlDataAdapter sqlAdpater = new SqlDataAdapter(s);
            DataTable newTable = new DataTable();

            sqlAdpater.Fill(newTable);

            sqlConnection.Close();

            return newTable.Rows.Count > 0 ? Convert.ToInt32(newTable.Rows[0]["Value"].ToString()) : 0;
        }

        public static int GetTargetDatabaseVersion(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd, string database, string subDir)
        {
            DirectoryInfo root = new DirectoryInfo(Application.StartupPath);

            if (Directory.Exists(Path.Combine(root.Parent.FullName, "Databases\\" + subDir)))
            {
                string[] dirs = Directory.GetDirectories(Path.Combine(root.Parent.FullName, "Databases\\" + subDir));

                List<string> dirList = new List<string>();
                foreach (string dir in dirs)
                {
                    DirectoryInfo dirInfo = new DirectoryInfo(dir);
                    int version = 0;
                    if (dirInfo.Name.Length == 13 && dirInfo.Name.StartsWith("Version") && int.TryParse(dirInfo.Name.Substring(7), out version))
                    {
                        if (dirInfo.GetFiles(database + ".*").Length > 0)
                        {
                            dirList.Add(dir);
                        }
                    }
                }

                dirList.Sort();

                int lastVersion = 0;

                if (dirList.Count > 0)
                {
                    DirectoryInfo lastDirInfo = new DirectoryInfo(dirList[dirList.Count - 1]);

                    int.TryParse(lastDirInfo.Name.Substring(7), out lastVersion);
                }

                return lastVersion;
            }
            else
            {
                return 0;
            }
        }

        public static void AppendXmlToDatabase(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd, string database, string file, ToolStripProgressBar progress, int dirCount)
        {
            SqlConnection.ClearAllPools(); 

            SqlConnection sqlConnection;
            if (auth == DatabaseServerLoginTypes.SqlServerAuthentication)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";Integrated Security=true");
            }

            sqlConnection.Open();

            DataSet dataSet = new DataSet();
            dataSet.ReadXml(file);

            int allCount = 0;
            
            foreach (DataTable table in dataSet.Tables)
            {
                allCount += table.Rows.Count;
            }

            decimal inc = (100 / dirCount) / allCount;
            decimal currentValue = 0;
            try
            {
                currentValue = progress.Value;
            }
            catch (Exception)
            {
            }

            foreach (DataTable table in dataSet.Tables)
            {
                for (int row = 0; row < table.Rows.Count; row++)
                {
                    List<string> ids = new List<string>();
                    List<string> keys = new List<string>();
                    List<object> keyValues = new List<object>();
                    List<string> fields = new List<string>();
                    List<object> values = new List<object>();

                    try
                    {
                        currentValue += inc;
                        progress.Value = Convert.ToInt32(currentValue);
                    }
                    catch (Exception)
                    {
                    }

                    for (int col = 0; col < table.Columns.Count; col++)
                    {
                        if (table.Columns[col].ColumnName.Contains("!"))
                        {
                            ids.Add(table.Columns[col].ColumnName.Replace("!", "").Replace("$", "").Replace("#", ""));
                        }

                        if (table.Columns[col].ColumnName.Contains("#") && table.Columns[col].ColumnName.Contains("$"))
                        {
                            string subQuery = table.Rows[row][table.Columns[col].ColumnName].ToString();
                            SqlCommand subQueryCommand = new SqlCommand(subQuery, sqlConnection);

                            keys.Add(table.Columns[col].ColumnName.Replace("!", "").Replace("$", "").Replace("#", ""));
                            keyValues.Add(subQueryCommand.ExecuteScalar());

                            fields.Add(table.Columns[col].ColumnName.Replace("!", "").Replace("$", "").Replace("#", ""));
                            values.Add(subQueryCommand.ExecuteScalar());
                        }
                        else if (table.Columns[col].ColumnName.Contains("$"))
                        {
                            string subQuery = table.Rows[row][table.Columns[col].ColumnName].ToString();
                            SqlCommand subQueryCommand = new SqlCommand(subQuery, sqlConnection);

                            fields.Add(table.Columns[col].ColumnName.Replace("!", "").Replace("$", "").Replace("#", ""));
                            values.Add(subQueryCommand.ExecuteScalar());
                        }
                        else if (table.Columns[col].ColumnName.Contains("#"))
                        {
                            keys.Add(table.Columns[col].ColumnName.Replace("!", "").Replace("$", "").Replace("#", ""));
                            keyValues.Add(table.Rows[row][table.Columns[col].ColumnName]);

                            fields.Add(table.Columns[col].ColumnName.Replace("!", "").Replace("$", "").Replace("#", ""));
                            values.Add(table.Rows[row][table.Columns[col].ColumnName]);
                        }
                        else
                        {
                            fields.Add(table.Columns[col].ColumnName);
                            values.Add(table.Rows[row][table.Columns[col].ColumnName]);
                        }
                    }

                    string selectSql = "SELECT Count(*) FROM " + table.TableName + " WHERE ";
                    for (int i = 0; i < keys.Count; i++)
                    {
                        if (i != 0) selectSql += " AND ";
                        selectSql += keys[i] + " = @" + keys[i];
                    }

                    SqlCommand selectCommand = new SqlCommand(selectSql, sqlConnection);

                    for (int i = 0; i < keys.Count; i++)
                    {
                        selectCommand.Parameters.Add(new SqlParameter(keys[i], keyValues[i] == null ? DBNull.Value : keyValues[i]));
                    }

                    object selectResult = selectCommand.ExecuteScalar();
                    if (selectResult == DBNull.Value || Convert.ToInt32(selectResult) == 0)
                    {
                        string insertSql = "INSERT INTO " + table.TableName + " ";

                        insertSql += "(";

                        for (int i = 0; i < fields.Count; i++)
                        {
                            if (ids.Count == 0 || fields[i] != ids[0])
                            {
                                if (i != 0) insertSql += ",";
                                insertSql += fields[i];
                            }
                        }
                        insertSql += ") VALUES (";

                        for (int i = 0; i < fields.Count; i++)
                        {
                            if (ids.Count == 0 || fields[i] != ids[0])
                            {
                                if (i != 0) insertSql += ",";
                                insertSql += "@" + fields[i];
                            }
                        }
                        insertSql += ")";

                        SqlCommand insertCommand = new SqlCommand(insertSql, sqlConnection);

                        for (int i = 0; i < fields.Count; i++)
                        {
                            insertCommand.Parameters.Add(new SqlParameter(fields[i], values[i] == null ? DBNull.Value : values[i]));
                        }

                        insertCommand.ExecuteNonQuery();
                    }
                    else
                    {
                        string updateSql = "UPDATE " + table.TableName + " SET ";

                        for (int i = 0; i < fields.Count; i++)
                        {
                            if (i != 0) updateSql += ",";
                            updateSql += fields[i] + " = @" + fields[i];
                        }
                        updateSql += " WHERE ";

                        for (int i = 0; i < keys.Count; i++)
                        {
                            if (i != 0) updateSql += " AND ";
                            updateSql += keys[i] + " = @" + keys[i];
                        }

                        SqlCommand updateCommand = new SqlCommand(updateSql, sqlConnection);

                        for (int i = 0; i < fields.Count; i++)
                        {
                            updateCommand.Parameters.Add(new SqlParameter(fields[i], values[i] == null ? DBNull.Value : values[i]));
                        }

                        updateCommand.ExecuteNonQuery();
                    }
                }
            }
        }

        public static void CreateDatabase(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd, string database, ToolStripProgressBar progress)
        {
            SqlConnection.ClearAllPools();
            
            SqlConnection sqlConnection;
            if (auth == DatabaseServerLoginTypes.SqlServerAuthentication)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=Master;User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=Master;Integrated Security=true");
            }

            sqlConnection.Open();

            SqlCommand command = new SqlCommand("CREATE DATABASE [" + database + "]", sqlConnection);
            command.ExecuteNonQuery();
        }

        public static void UpdateDatabase(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd, string database, string template, ToolStripProgressBar progress, string subDir)
        {
            SqlConnection.ClearAllPools(); 

            int currentVersion = GetDatabaseVersion(serverName, auth, userName, pwd, database);

            DirectoryInfo root = new DirectoryInfo(Application.StartupPath);

            string[] dirs = Directory.GetDirectories(Path.Combine(root.Parent.FullName, "Databases\\" + subDir));

            List<string> dirList = new List<string>();
            foreach (string dir in dirs)
            {
                DirectoryInfo dirInfo = new DirectoryInfo(dir);
                int version = 0;
                if (dirInfo.Name.Length == 13 && dirInfo.Name.StartsWith("Version") && int.TryParse(dirInfo.Name.Substring(7), out version) && version > currentVersion)
                {
                    dirList.Add(dir);
                }
            }

            dirList.Sort();


            foreach (string dir in dirList)
            {
                try
                {
                    progress.Value += 100 / dirList.Count;
                }
                catch (Exception)
                {
                }

                DirectoryInfo dirInfo = new DirectoryInfo(dir);
                int version = 0;
                if (dirInfo.Name.StartsWith("Version") && int.TryParse(dirInfo.Name.Substring(7), out version) && version == currentVersion + 1)
                {
                    SqlConnection sqlConnection;
                    if (auth == DatabaseServerLoginTypes.SqlServerAuthentication)
                    {
                        sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
                    }
                    else
                    {
                        sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";Integrated Security=true");
                    }

                    sqlConnection.Open();

                    #region Update Schema
                    if (File.Exists(Path.Combine(dir, template + ".sql")))
                    {
                        FileInfo file = new FileInfo(Path.Combine(dir, template + ".sql"));

                        Microsoft.SqlServer.Management.Smo.Server server = new Microsoft.SqlServer.Management.Smo.Server(new ServerConnection(sqlConnection));
                        server.ConnectionContext.ExecuteNonQuery(File.ReadAllText(Path.Combine(dir, template + ".sql")));

                        //string SQLscript = file.OpenText().ReadToEnd();
                        //SQLscript = SQLscript.Replace("GO", "");
                        //SQLscript = Regex.Replace(SQLscript, "([/*][*]).*([*][/])", "");
                        //SQLscript = Regex.Replace(SQLscript, "\\s{2,}", " ");

                        //SqlCommand sqlCommand = new SqlCommand(SQLscript, sqlConnection);

                        //sqlCommand.ExecuteNonQuery();
                    }
                    #endregion

                    #region Update Content
                    if (File.Exists(Path.Combine(dir, template + ".xml")))
                    {
                        DatabaseUtilities.AppendXmlToDatabase(serverName, auth, userName, pwd, database, Path.Combine(dir, template + ".xml"), progress, dirList.Count);
                    }
                    #endregion

                    string addVersion = "EXEC sys.sp_addextendedproperty " +
                       "@name = N'Version', " +
                       "@value = N'" + version.ToString() + "'";


                    SqlCommand addVersionCommand = new SqlCommand(addVersion, sqlConnection);

                    try
                    {
                        addVersionCommand.ExecuteNonQuery();
                    }
                    catch (Exception)
                    {
                    }

                    string updateVersion = "EXEC sys.sp_updateextendedproperty " +
                                           "@name = N'Version', " +
                                           "@value = N'" + version.ToString() + "'";

                    SqlCommand updateVersionCommand = new SqlCommand(updateVersion, sqlConnection);

                    updateVersionCommand.ExecuteNonQuery();

                    sqlConnection.Close();

                    currentVersion++;
                }
            }
        }

        public static void UpdateDatabaseCustomization(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd, string database, string template, ToolStripProgressBar progress, string subDir)
        {
            SqlConnection.ClearAllPools();

            int currentVersion = GetDatabaseCustomizationVersion(serverName, auth, userName, pwd, database);

            DirectoryInfo root = new DirectoryInfo(Application.StartupPath);

            string[] dirs = Directory.GetDirectories(Path.Combine(root.Parent.FullName, "Databases\\" + subDir));

            List<string> dirList = new List<string>();
            foreach (string dir in dirs)
            {
                DirectoryInfo dirInfo = new DirectoryInfo(dir);
                int version = 0;
                if (dirInfo.Name.Length == 13 && dirInfo.Name.StartsWith("Version") && int.TryParse(dirInfo.Name.Substring(7), out version) && version > currentVersion)
                {
                    dirList.Add(dir);
                }
            }

            dirList.Sort();


            foreach (string dir in dirList)
            {
                try
                {
                    progress.Value += 100 / dirList.Count;
                }
                catch (Exception)
                {
                }

                DirectoryInfo dirInfo = new DirectoryInfo(dir);
                int version = 0;
                if (dirInfo.Name.StartsWith("Version") && int.TryParse(dirInfo.Name.Substring(7), out version) && version == currentVersion + 1)
                {
                    SqlConnection sqlConnection;
                    if (auth == DatabaseServerLoginTypes.SqlServerAuthentication)
                    {
                        sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
                    }
                    else
                    {
                        sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";Integrated Security=true");
                    }

                    sqlConnection.Open();

                    #region Update Schema
                    if (File.Exists(Path.Combine(dir, template + ".sql")))
                    {
                        FileInfo file = new FileInfo(Path.Combine(dir, template + ".sql"));

                        Microsoft.SqlServer.Management.Smo.Server server = new Microsoft.SqlServer.Management.Smo.Server(new ServerConnection(sqlConnection));
                        server.ConnectionContext.ExecuteNonQuery(File.ReadAllText(Path.Combine(dir, template + ".sql")));

                        //string SQLscript = file.OpenText().ReadToEnd();
                        //SQLscript = SQLscript.Replace("GO", "");
                        //SQLscript = Regex.Replace(SQLscript, "([/*][*]).*([*][/])", "");
                        //SQLscript = Regex.Replace(SQLscript, "\\s{2,}", " ");

                        //SqlCommand sqlCommand = new SqlCommand(SQLscript, sqlConnection);

                        //sqlCommand.ExecuteNonQuery();
                    }
                    #endregion

                    #region Update Content
                    if (File.Exists(Path.Combine(dir, template + ".xml")))
                    {
                        DatabaseUtilities.AppendXmlToDatabase(serverName, auth, userName, pwd, database, Path.Combine(dir, template + ".xml"), progress, dirList.Count);
                    }
                    #endregion

                    string addVersion = "EXEC sys.sp_addextendedproperty " +
                       "@name = N'CustomizationVersion', " +
                       "@value = N'" + version.ToString() + "'";


                    SqlCommand addVersionCommand = new SqlCommand(addVersion, sqlConnection);

                    try
                    {
                        addVersionCommand.ExecuteNonQuery();
                    }
                    catch (Exception)
                    {
                    }

                    string updateVersion = "EXEC sys.sp_updateextendedproperty " +
                                           "@name = N'CustomizationVersion', " +
                                           "@value = N'" + version.ToString() + "'";

                    SqlCommand updateVersionCommand = new SqlCommand(updateVersion, sqlConnection);

                    updateVersionCommand.ExecuteNonQuery();

                    sqlConnection.Close();

                    currentVersion++;
                }
            }
        }

        public static DataTable GetDataTable(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd, string database, string sqlCommand)
        {
            SqlConnection.ClearAllPools();

            SqlConnection sqlConnection;
            if (auth == DatabaseServerLoginTypes.SqlServerAuthentication)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";Integrated Security=true");
            }
            
            sqlConnection.Open();

            DataTable table = new DataTable();

            SqlCommand cmd = new SqlCommand(sqlCommand, sqlConnection);

            SqlDataAdapter adapter = new SqlDataAdapter(cmd);
            adapter.Fill(table);

            return table;
        }
        
        public static void ExecuteCommand(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd, string database, string sqlCommand)
        {
            SqlConnection.ClearAllPools(); 
            
            SqlConnection sqlConnection;
            if (auth == DatabaseServerLoginTypes.SqlServerAuthentication)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=" + database + @";Integrated Security=true");
            }

            sqlConnection.Open();

            DataTable table = new DataTable();

            SqlCommand cmd = new SqlCommand(sqlCommand, sqlConnection);
            cmd.ExecuteNonQuery();
        }

        public static bool Is64Server(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd)
        {
            SqlConnection.ClearAllPools();
            
            SqlConnection sqlConnection;
            if (auth == DatabaseServerLoginTypes.SqlServerAuthentication)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=Master;User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=Master;Integrated Security=true");
            }

            sqlConnection.Open();
            
            SqlCommand com = new SqlCommand("SELECT SERVERPROPERTY('edition')", sqlConnection);

            string result = com.ExecuteScalar().ToString();

            return result.Contains("64");
        }

        public static bool CheckIfSQLServiceCanAccessLocation(string serverName, DatabaseServerLoginTypes auth, string userName, string pwd, string path)
        {
            SqlConnection.ClearAllPools();

            SqlConnection sqlConnection;
            if (auth == DatabaseServerLoginTypes.SqlServerAuthentication)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=Master;User ID=" + userName + @";Password=" + pwd + @";Trusted_Connection=no");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + serverName + @";Initial Catalog=Master;Integrated Security=true");
            }

            sqlConnection.Open();

            DataTable table = new DataTable();

            SqlCommand cmd = new SqlCommand("master..xp_FILEEXIST", sqlConnection);
            cmd.CommandType = CommandType.StoredProcedure;
            cmd.Parameters.Add(new SqlParameter("File", path));
            DataTable result = new DataTable();
            result.Load(cmd.ExecuteReader());

            return result.Rows[0]["Parent Directory Exists"].ToString().Trim() == "1" &&
                   result.Rows[0]["File is a Directory"].ToString().Trim() == "1";
        }
    }
}
